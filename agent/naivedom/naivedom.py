import agent.definitions as defs
import requests
import htmlmin
import re
from bs4 import BeautifulSoup, NavigableString, Tag, Comment
from agent.libs.aipython.searchProblem import Arc, Search_problem_from_explicit_graph
#from agent.libs.aipython.searchGeneric import LCFSSearcher

import networkx as nx
import matplotlib.pyplot as plt 
from agent.libs.nx_layout.hierarchy_pos import hierarchy_pos

from selenium import webdriver
from selenium.webdriver.common.by import By
import math

from agent.naivedom.NaiveDOMSearcher import NaiveDOMSearcher


#dimensione minima e massima etichetta nodi NDOM
_MIN_LABEL_LENGTH = 2
_MAX_LABEL_LENGTH = 40

# nodi del DOM da non esplorare
_TAG_BLACKLIST = [
    'head', 'script', 'style', 'svg', 'meta', 'link', 'title', 'base',
    'noscript', 'template', 'iframe', 'canvas', 'object', 'embed', 'param',
    'applet', 'frame', 'frameset', 'map', 'area', 'track', 'data',
    'figcaption', 'form', 'fieldset', 'p', 'button'
]

# nodi del DOM che, una volta inseriti nel NDOM, conterranno potenzialmente altri nodi
_TAG_PARENTS = [
    'html', 'body', 'header', 'section', 'nav', 'ul', 'ol', 'article', 'aside',
    'footer', 'table', 'address'
]

# nodi del DOM che per certo rappresentano una foglia del NDOM
_TAG_LEAFS = [
    'img', 'a', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6'
]

_TASKS_DEFAULT = {
    1: ['circolari', 'comunicazion'],
    2: ['organigramma', 'organizzazione'],
    3: ['notizie'],
    4: ['progett'],
    5: ['regolament'],
    6: ['amministrazione trasparente'],
    7: ['registro'],
    8: ['indirizzo', 'i luoghi', 'dove siamo', 'contatti']
}

def _get_driver(location) -> webdriver:
    """Restituisce un'istanza della classe webdriver, cioè un browser avente una scheda
    aperta al sito location.
    Vedi: #https://stackoverflow.com/a/55878622

    Args:
        location: URL del sito

    Returns:
        webdriver: istanza webdriver
    """

    options = webdriver.FirefoxOptions()
    options.add_argument(f'--user-agent={defs.headers["User-Agent"]}')
    options.add_argument(f'--width={defs.BROWSER_WIDTH}')
    options.add_argument(f'--height={defs.BROWSER_HEIGHT}')

    driver = webdriver.Firefox(options=options)
    driver.get(location)

    return driver





class NaiveDOM():
    """Classe che modella un modello DOM più semplice della pagina web, chiamato NaiveDOM
    o NDOM, dove sono esclusi tag ad uso prettamente tecnico e di struttura della pagina.

    Attrs:
        - location (string): Percorso o URL della pagina di cui si sta cercando di costruire il NDOM
        - nodes (dict): dizionario xpath:label di ogni elemento del NDOM
        - nodes_coords (dict): dizionario xpath:(x, y) di ogni elemento del NDOM
        - nodes_arcs (list): lista di Arc(from, to, cost)
        - start (string): xpath del nodo radice del NDOM
    """

    def _calc_arc_cost(self, driver: webdriver, NDOM_parent_xpath, xpath) -> float:
        """Calcola il costo in termini di usabilità che l'utente paga quando passa dall'
        interagire con l'elemento NDOM_parent_xpath all'interagire con l'elemento xpath.
        Ulteriori informazioni: report.pdf
        Doc. find_element: https://stackoverflow.com/questions/15510882/selenium-get-coordinates-or-dimensions-of-element-with-python

        Args:
            driver (webdriver): istanza webdriver
            NDOM_parent_xpath: xpath nodo genitore nel modello NDOM
            xpath (_type_): xpath nodo corrente nel modello NDOM

        Returns:
            float: costo in termini di usabilità
        """
        
        # coordinate nodo genitore
        if NDOM_parent_xpath in self.nodes_coords:
            parent_coords = self.nodes_coords[NDOM_parent_xpath]
        else:
            parent_coords = (0, 0)
            self.nodes_coords[NDOM_parent_xpath] = parent_coords

        # coordinate nodo corrente
        try:
            elem = driver.find_element(By.XPATH, xpath)
        except:
            return 60
        
        elem_coords = (elem.location['x'], elem.location['y'])
        self.nodes_coords[xpath] = elem_coords

        # distanza euclidea coord. nodo corrente - coord. nodo genitore
        ignore_x_coeff = 0 if NDOM_parent_xpath == self.start else 1
        distance = (    ((elem_coords[0] - parent_coords[0])**2)*ignore_x_coeff +
                        (elem_coords[1] - parent_coords[1])**2
                    ) ** 0.5

        # funzione di costo in base alla distanza
        if distance == 0:
            return 0
        else:
            return round(math.log(distance)*distance/1200, 1)
    

    def _browse_DOM(self, root, DOM_parent_xpath=None, DOM_ci=0, NDOM_parent_xpath=None, driver: webdriver=None):
        """Sfoglia DOM della pagina web, decidendo quali tag rientreranno nel modello NaiveDOM
        della pagina.

        Args:
            - root: Non per forza un elemento bs4.Tag, ma può anche essere di tipo bs4.NavigableString
            - DOM_parent_xpath (optional): xpath elemento genitore del DOM. Default: None.
            - DOM_ci (int, optional): child index del DOM. Default: 0.
            - NDOM_parent_xpath (optional): id del nodo genitore di root (all'interno del NDOM). Default: None.
            - driver (webdriver, optional): istanza webdriver. Default: None.
        """

        is_DOM_tag = isinstance(root, Tag)

        if is_DOM_tag and root.name in _TAG_BLACKLIST:
            return
        
        #inizializzazione
        xpath = None
        label = None #None solo quando il nodo non e' ne' nodo del NDOM ne' foglia del NDOM
        next_NDOM_parent_xpath = None #None se foglia del NDOM, altro se nodo del NDOM     

        is_DOM_root = is_DOM_tag and not DOM_parent_xpath and not NDOM_parent_xpath
        if is_DOM_root:
            # root e' la radice del NDOM
            xpath = f'//{root.name}'
            label = root.name

            next_NDOM_parent_xpath = xpath
            self.start = xpath

        elif is_DOM_tag and root.name in _TAG_PARENTS:
            # root e' un nodo interno del NDOM
            xpath = f'{DOM_parent_xpath}/*[{(DOM_ci)}]'
            label = root.name

            next_NDOM_parent_xpath = xpath

        elif is_DOM_tag and root.name in _TAG_LEAFS:
            # root e' un tag foglia
            xpath = f'{DOM_parent_xpath}/*[{(DOM_ci)}]'
            label = ' '.join(root.stripped_strings)
        
        elif is_DOM_tag:
            # root e' un tag che non e' ne' nodo interno del NDOM ne' foglia del NDOM
            xpath = f'{DOM_parent_xpath}/*[{(DOM_ci)}]'

            next_NDOM_parent_xpath = NDOM_parent_xpath
        
        elif isinstance(root, NavigableString):
            # root e' una stringa, quindi una foglia
            label = repr(root)
            label_len = len(label)
            if label_len <= _MIN_LABEL_LENGTH or label_len >= _MAX_LABEL_LENGTH:
                return
            
            xpath = DOM_parent_xpath
        
        else:
            # isinstance(root, (Stylesheet, Script, Template, Comment))
            return
        
        if label:
            # aggiungo nodo
            self.nodes[xpath] = label.lower()

            # aggiungo arco entrante. ovviamente il nodo radice non ha archi entranti
            if not is_DOM_root:
                self.arcs.append(Arc(   NDOM_parent_xpath,
                                        xpath,
                                        cost=self._calc_arc_cost(driver, NDOM_parent_xpath, xpath)
                                    )
                                ) 
            
        # sfoglio ciascun sottoalbero
        if next_NDOM_parent_xpath: 
            i = 1
            for child in root.children:
                self._browse_DOM(   child,
                                    DOM_parent_xpath=xpath,
                                    DOM_ci=i,
                                    NDOM_parent_xpath=next_NDOM_parent_xpath,
                                    driver=driver
                                )
                c = 0 if isinstance(child, (NavigableString, Comment)) else 1
                i+=c

    def __init__(self, location, from_file=False):
        """Crea un nuovo oggetto NaiveDOM.

        Args:
            - location (string): Percorso o URL del codice sorgente 
            - from_file (bool, optional): Specifica se creare un nuovo NaiveDOM da file .html. Default: False.
        """

        # inizializzazione
        self.location = location
        self.nodes = {} # dict
        self.nodes_coords = {} # dict
        self.arcs = []
        self.start = None
        self.nodes_goal = {} # set

        # ottenimento sorgente e (eventulamente) driver selenium
        print(f'Getting HTML of {self.location} ...')
        if from_file:
            with open(location, "r") as f:
                html = f.read() 
            driver = None
        else:
            #r = requests.get(location, headers=defs.headers)
            #html = r.text
            driver = _get_driver(location)
            html = driver.page_source
        
        # opzionale, per velocizzare parsing con beautifulsoup
        print('Optimizing HTML...')
        #html = htmlmin.minify(html, remove_comments=True)
        for tag in _TAG_BLACKLIST:
            html = re.sub(fr'<{tag}[^>]*>.*?</{tag}>', f'<{tag}></{tag}>', html)

        html = re.sub(r'<!--(.*?)-->', '', html, flags=re.DOTALL)
        html = re.sub('>\s*<', '><', html)

        # parser: 'lxml' o 'html5lib' (chiudono tag lasciati aperti)
        soup = BeautifulSoup(html, 'html5lib')

        # popola attributi
        print('Building NDOM...')
        self._browse_DOM(soup.html.body, driver=driver)

        # chiudi driver
        if driver: driver.close()
    
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
        nx.draw_networkx_edge_labels(G, pos, edge_labels=dict(((arc.from_node, arc.to_node), arc.cost) for arc in self.arcs))
        
        plt.show()

    def get_target_score(self, tasks:dict=_TASKS_DEFAULT) -> float:
        """Restituisce il target score dell'oggetto NaiveDOM.

        Args:
            - tasks (dict, optional): dizionario task id:keyword delle parole chiave utili a
            individuare i nodi goal del NDOM. Default: _TASKS_DEFAULT.

        Returns:
            - target_score: float
        """

        target_score = 0
        task_completed = 0

        for task_id, task_keywords in tasks.items():

            self.nodes_goal = set()
            for node_xpath, node_label in self.nodes.items():
                for keyword in task_keywords:
                    if keyword in node_label:
                        self.nodes_goal.add(node_xpath)
                        break

            if self.nodes_goal:
                path =  NaiveDOMSearcher(
                                Search_problem_from_explicit_graph(
                                self.nodes, self.arcs,
                                self.start, self.nodes_goal)
                        ).search()
                task_score = path.cost                
                task_completed += 1
            else:
                task_score = 10
            target_score += task_score
        
        target_score = target_score / task_completed

        return target_score
        

if __name__ == '__main__':
    #d = NaiveDOM('source1.html', from_file=True)
    #https://www.leonardope.edu.it/

    d = NaiveDOM('https://itisandria.edu.it/')

    #ff = open('./d.nodes.txt', 'w', encoding='utf-8')
    #print(d.nodes, file=ff)
    #ff.close()
    #print(d.nodes)
    #print('---------------------------')
    print(d.get_target_score())
    d.plot()
    #print('############')
    #print(d.arcs)
