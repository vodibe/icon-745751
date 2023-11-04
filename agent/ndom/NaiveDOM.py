import agent.definitions as defs
import re
import datetime
from math import exp
from bs4 import (
    BeautifulSoup,
    NavigableString,
    Tag,
    Comment,
    Stylesheet,
    Script,
    TemplateString,
)
from agent.libs.aipython.searchProblem import Arc, Search_problem_from_explicit_graph
from agent.ndom.NaiveDOMSearcher import NaiveDOMSearcher
from agent.preproc.utils import _create_driver

import networkx as nx
import matplotlib.pyplot as plt
from agent.libs.nx_layout.hierarchy_pos import hierarchy_pos

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.common.exceptions import NoSuchElementException


# dimensione minima e massima etichetta nodi NDOM
_MIN_NDOM_LABEL_LENGTH = 2
_MAX_NDOM_LABEL_LENGTH = 140

# nodi del DOM da non esplorare
_TAG_BLACKLIST = [
    "applet",
    "area",
    "base",
    "button",
    "canvas",
    "data",
    "embed",
    "fieldset",
    "form",
    "frame",
    "frameset",
    "head",
    "iframe",
    "link",
    "map",
    "meta",
    "noscript",
    "object",
    "option",
    "param",
    "script",
    "style",
    "svg",
    "template",
    "title",
    "track",
]

# nodi del DOM che, una volta inseriti nel NDOM, conterranno potenzialmente altri nodi
_TAG_PARENTS = [
    "address",
    "article",
    "aside",
    "body",
    "footer",
    "header",
    "html",
    "li",
    "nav",
    "ol",
    "section",
    "table",
    "ul",
]

# nodi del DOM che per certo rappresentano una foglia del NDOM
_TAG_LEAFS = ["a", "h1", "h2", "h3", "h4", "h5", "h6", "img", "p"]

# dizionario dei target predefinito
TASKS_DEFAULT = {
    "task1": ["circolari", "comunicazioni", "circolare"],
    "task2": ["organigramma", "organizzazione", "schema organizzativo", "persone"],
    "task3": ["notizie", "news", "eventi"],
    "task4": ["progetti", "progetto", "projects"],
    "task5": ["regolamento", "regolamenti", "regolamentazione"],
    "task6": ["amministrazione trasparente", "ammin. trasparente"],
    "task7": ["registro"],
    "task8": ["indirizzo", "i luoghi", "dove siamo", "contatti"],
}

# 1^ parte features del NDOM che verranno considerate in un modello di apprendimento
ds2_features_part = [
    "school_id",
    "page_url",
    "page_load_time_ms",
    "page_width",
    "page_height",
    "NDOM_nodes",
    "NDOM_height",
]

# 2^ parte features del sito che verranno considerate in un modello di apprendimento
ds2_features_askable = ["page_template", "page_menu_or", "page_ungrouped_multim", "metric"]

# features incluse nel modello di apprendimento
ds2_features = ds2_features_part + ds2_features_askable + list(TASKS_DEFAULT.keys())


class NaiveDOM:
    """Classe che modella un modello DOM più semplice della pagina web, chiamato NaiveDOM
    o NDOM, dove sono esclusi tag ad uso prettamente tecnico e di struttura della pagina.

    Attrs:
        - location (string): Percorso o URL della pagina di cui si sta costruendo il NDOM
        - nodes (dict): dizionario xpath:label di ogni elemento del NDOM
        - nodes_coords (dict): dizionario xpath:(x, y) di ogni elemento del NDOM
        - arcs (list): lista di Arc(from, to, cost)
        - start (string): xpath del nodo radice del NDOM
        - nodes_goal (list): lista di nodi obiettivo. Cambia a seconda del task da svolgere
        - pen_task_nf (int): costo di default di un task per il quale non esistono nodi obiettivo
        - features (dict): dizionario: feature:valore utile per i modelli di SL.


    """

    def _calc_arc_cost(self, driver: webdriver, NDOM_parent_xpath, xpath) -> float:
        """Calcola il costo in termini di usabilità che l'utente paga quando passa dall'
        interagire con l'elemento NDOM_parent_xpath all'interagire con l'elemento xpath.
        Doc. find_element: https://stackoverflow.com/questions/15510882/selenium-get-coordinates-or-dimensions-of-element-with-python

        Args:
            - driver (webdriver): istanza webdriver
            - NDOM_parent_xpath: xpath nodo genitore nel modello NDOM
            - xpath: xpath nodo corrente nel modello NDOM

        Returns:
            float: costo in termini di usabilità
        """

        # costo se l'elemento non si trova
        node_not_found_cost = 6

        # coordinate nodo genitore
        if NDOM_parent_xpath in self.nodes_coords:
            parent_coords = self.nodes_coords[NDOM_parent_xpath]
        else:
            parent_coords = (0, 0)
            self.nodes_coords[NDOM_parent_xpath] = parent_coords

        # coordinate nodo corrente
        try:
            elem = driver.find_element(By.XPATH, xpath)
        except NoSuchElementException:
            return node_not_found_cost

        elem_coords = (elem.location["x"], elem.location["y"])
        self.nodes_coords[xpath] = elem_coords

        # distanza euclidea coord. nodo corrente - coord. nodo genitore
        if NDOM_parent_xpath == self.start:
            ignore_x_coeff = 0
        else:
            ignore_x_coeff = 1

        distance = (
            ((elem_coords[0] - parent_coords[0]) ** 2) * ignore_x_coeff
            + (elem_coords[1] - parent_coords[1]) ** 2
        ) ** 0.5

        # funzione di costo in base alla distanza
        # https://www.desmos.com/calculator/wf68sucipn?lang=it
        arc_cost = round(((distance * exp(1.3) / (defs.BROWSER_DIAG))), 2)

        return arc_cost

    def _calc_task_cost(self, tasks: dict = TASKS_DEFAULT):
        """A partire dal NDOM e da una lista di task, calcola il costo in usabilità
        necessario per svolgere questi task. Se un task non può essere portato a termine
        (perchè non ci sono nodi obiettivo), si assume che tale task viene eseguito con
        un costo di default, cioè self.pen_task_nf.

        Args:
            tasks (dict, optional): Lista di task. Default:_TASKS_DEFAULT
        """

        problem = Search_problem_from_explicit_graph(self.nodes.keys(), self.arcs, self.start)

        for task_id, task_keywords in tasks.items():
            # per ogni task individua quali sono i nodi obiettivo del NDOM
            self.nodes_goal = []
            for node_xpath, node_label in self.nodes.items():
                keyword_found_in_label = False
                for keyword in task_keywords:
                    if " " in keyword:
                        # ad es. se la keyword è "amministrazione trasparente"
                        # devo cercare questa frase intera all'interno della label
                        if keyword in node_label:
                            self.nodes_goal.append(node_xpath)
                            keyword_found_in_label = True
                    else:
                        # ad es. se la keyword è "indirizzo" devo fare una ricerca
                        # whole-word.
                        # ad es.: label = provvedimenti organo indirizzo-politico -> no
                        # ad es.: label = indirizzo della scuola -> si
                        s = re.split(r"\s|\.", node_label)
                        if any(st == keyword for st in s):
                            self.nodes_goal.append(node_xpath)
                            keyword_found_in_label = True

                    if keyword_found_in_label:
                        # esamina l'altro nodo
                        break

            # per ogni task calcola il percorso necessario per andare dalla radice
            # a uno dei nodi obiettivo per quel task
            if self.nodes_goal:
                problem.set_goals(self.nodes_goal)
                NDOM_searcher = NaiveDOMSearcher(problem)

                # restituisce un oggetto Path se esiste un percorso, None altrimenti
                task_path = NDOM_searcher.search()

                pen_paths_expanded = NDOM_searcher.num_expanded / 280 - 0.2
                pen_paths_expanded = 0 if pen_paths_expanded < 0 else pen_paths_expanded

                task_cost = round(
                    task_path.cost + pen_paths_expanded + (len(list(task_path.nodes())) * 0.15),
                    2,
                )
                self.features[task_id] = task_cost
            else:
                self.features[task_id] = self.pen_task_nf

    def _browse_DOM(
        self,
        root,
        DOM_parent_xpath=None,
        DOM_ci=1,
        NDOM_parent_xpath=None,
        depth=0,
        driver: webdriver = None,
    ):
        """Sfoglia DOM della pagina web, decidendo quali tag rientreranno nel modello NaiveDOM
        della pagina.

        Args:
            - root: Non per forza un elemento bs4.Tag, ma può anche essere di tipo bs4.NavigableString
            - DOM_parent_xpath (optional): xpath elemento genitore del DOM. Default: None.
            - DOM_ci (int, optional): child index del DOM. Default: 1.
            - NDOM_parent_xpath (optional): id del nodo genitore di root (all'interno del NDOM). Default: None.
            - depth (optional): profondità del nodo root.
            - driver (webdriver, optional): istanza webdriver. Default: None.
        """

        is_DOM_tag = isinstance(root, Tag)

        if (is_DOM_tag and root.name in _TAG_BLACKLIST) or isinstance(
            root, (Stylesheet, Script, TemplateString, Comment)
        ):
            return

        # inizializzazione
        xpath = None
        label = None  # None solo quando il nodo non e' ne' nodo del NDOM ne' foglia del NDOM
        next_NDOM_parent_xpath = None  # None se foglia del NDOM, altro se nodo del NDOM
        next_depth = depth

        is_DOM_root = is_DOM_tag and not DOM_parent_xpath and not NDOM_parent_xpath
        if is_DOM_root:
            # root e' la radice del NDOM
            xpath = f"//{root.name}"
            label = root.name

            next_NDOM_parent_xpath = xpath
            self.start = xpath

        elif is_DOM_tag and root.name in _TAG_PARENTS:
            # root e' un nodo interno del NDOM
            xpath = f"{DOM_parent_xpath}/*[{(DOM_ci)}]"
            label = root.name

            next_NDOM_parent_xpath = xpath

        elif is_DOM_tag and root.name in _TAG_LEAFS:
            # root e' un tag foglia
            xpath = f"{DOM_parent_xpath}/*[{(DOM_ci)}]"
            label = " ".join(root.stripped_strings)

            # elimino nodi che contengono testo lungo o non leggibile
            label_len = len(label)
            if label_len <= _MIN_NDOM_LABEL_LENGTH or label_len >= _MAX_NDOM_LABEL_LENGTH:
                return

        elif is_DOM_tag:
            # root e' un tag che non e' ne' nodo interno del NDOM ne' foglia del NDOM
            xpath = f"{DOM_parent_xpath}/*[{(DOM_ci)}]"

            next_NDOM_parent_xpath = NDOM_parent_xpath

        else:
            # isinstance(root, NavigableString)
            # root e' una stringa, quindi una foglia
            xpath = DOM_parent_xpath
            label = repr(root)

            # elimino nodi che contengono testo lungo o non leggibile
            label_len = len(label)
            if label_len <= _MIN_NDOM_LABEL_LENGTH or label_len >= _MAX_NDOM_LABEL_LENGTH:
                return

        if label:
            # aggiungo nodo
            self.nodes[xpath] = label.lower()

            # aggiungo arco entrante. ovviamente il nodo radice non ha archi entranti
            if not is_DOM_root and NDOM_parent_xpath != xpath:
                self.arcs.append(
                    Arc(
                        NDOM_parent_xpath,
                        xpath,
                        cost=self._calc_arc_cost(driver, NDOM_parent_xpath, xpath),
                    )
                )
                # aggiorno profondita'
                next_depth = depth + 1
                if next_depth > self.features["NDOM_height"]:
                    self.features["NDOM_height"] = next_depth

        # sfoglio ciascun sottoalbero
        if next_NDOM_parent_xpath:
            i = 1
            for child in root.children:
                self._browse_DOM(
                    child,
                    DOM_parent_xpath=xpath,
                    DOM_ci=i,
                    NDOM_parent_xpath=next_NDOM_parent_xpath,
                    depth=next_depth,
                    driver=driver,
                )
                # le stringhe dentro un tag non si considerano elementi figli del tag
                if not isinstance(child, (NavigableString, Comment)):
                    i += 1

    def __init__(
        self,
        location,
        alias="",
        from_file=False,
        driver: webdriver = None,
        driver_close_at_end=True,
    ):
        # inizializzazione attributi
        self.location = location
        self.nodes = {}  # dict xpath:label
        self.nodes_coords = {}  # dict xpath:(x,y)
        self.arcs = []
        self.start = None
        self.nodes_goal = []
        self.pen_task_nf = None

        # il dizionario delle features è un attributo del NDOM
        self.features = dict()

        print(f"Building NDOM for {self.location}")

        # ottenimento sorgente e istanza driver
        print(f"Reading HTML...")
        if from_file:
            with open(location, "r") as f:
                html = f.read()
            driver = None
        else:
            # r = requests.get(location, headers=defs.headers)
            # html = r.text
            if not driver:
                driver = _create_driver()

            load_start = datetime.datetime.now()
            driver.get(location)
            load_end = datetime.datetime.now()

            self.features["page_width"] = driver.execute_script("return document.body.scrollWidth")
            self.features["page_height"] = driver.execute_script(
                "return document.body.scrollHeight"
            )

            html = driver.page_source

        # opzionale, per velocizzare parsing con beautifulsoup
        print("Cleaning HTML...")
        # html = htmlmin.minify(html, remove_comments=True)
        for tag in _TAG_BLACKLIST:  # rimuove contenuto tag in blacklist
            html = re.sub(rf"<{tag}[^>]*>.*?</{tag}>", f"<{tag}></{tag}>", html)
        html = re.sub(r"<!--(.*?)-->", "", html, flags=re.DOTALL)  # rimuove commenti
        html = re.sub(">\s*<", "><", html)  # rimuove spazi vuoti dopo tag

        # parser: 'lxml' o 'html5lib' (chiudono tag lasciati aperti)
        soup = BeautifulSoup(html, "html5lib")

        # costruisci NDOM: popola attributi
        print("Parsing HTML <body> tag...")
        self.features["NDOM_height"] = 0
        self._browse_DOM(soup.html.body, driver=driver)

        self.pen_task_nf = round((6.5 + (len(self.nodes) // 500) * 0.5), 2)
        # self.pen_graph = (nodes_len**2) * (10 ** (-6.5))

        # popola features
        print("Populating features...")
        self.features["school_id"] = alias
        self.features["page_url"] = self.location
        self.features["page_load_time_ms"] = int((load_end - load_start).total_seconds() * 1000)
        # "page_width" -> assegnato
        # "page_height" -> assegnato
        # "page_template" -> assegnato
        self.features["NDOM_nodes"] = len(self.nodes)
        # "NDOM_height" -> assegnato
        self._calc_task_cost()

        # chiudi driver
        if driver and driver_close_at_end:
            driver.close()

    def get_features(self) -> dict:
        """Restituisce un dizionario feature:valore"""

        return self.features

    def plot(self):
        """Renderizza il NaiveDOM.
        Docs: https://networkx.org/documentation/latest/reference/generated/networkx.drawing.nx_pylab.draw.html
        """

        G = nx.Graph()
        G.add_nodes_from(list(self.nodes))
        G.add_edges_from([(arc.from_node, arc.to_node) for arc in self.arcs])

        pos = hierarchy_pos(G, self.start)
        nx.draw(G, pos=pos, with_labels=False)
        nx.draw_networkx_labels(G, pos, self.nodes)
        nx.draw_networkx_edge_labels(
            G,
            pos,
            edge_labels=dict(((arc.from_node, arc.to_node), arc.cost) for arc in self.arcs),
        )

        plt.show()


if __name__ == "__main__":
    # NDOM_file1 = NaiveDOM('source1.html', from_file=True)
    NDOM_website1 = NaiveDOM("https://itisandria.edu.it/")

    print(NDOM_website1.get_features())
    NDOM_website1.plot()
