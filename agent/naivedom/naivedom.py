import agent.definitions as defs
import requests
import htmlmin
import re
from bs4 import BeautifulSoup, NavigableString, Tag
from agent.libs.aipython.searchProblem import Arc

import networkx as nx
import matplotlib.pyplot as plt 
from agent.libs.nx_layout.hierarchy_pos import hierarchy_pos

from selenium import webdriver
from selenium.webdriver.common.by import By


#dimensione minima e massima etichetta nodi NDOM
_MIN_LABEL_LENGTH = 2
_MAX_LABEL_LENGTH = 40

# nodi del DOM da non esplorare
_TAG_BLACKLIST = [
    'head', 'script', 'style', 'svg', 'meta', 'link', 'title', 'base',
    'noscript', 'template', 'iframe', 'canvas', 'object', 'embed', 'param',
    'applet', 'frame', 'frameset', 'map', 'area', 'track', 'data',
    'figure', 'figcaption', '\n', 'form', 'fieldset', 'p', 'button'
]

# nodi del DOM che, una volta inseriti nel NDOM, conterranno potenzialmente altri nodi
_TAG_PARENTS = [
    'html', 'body', 'ul', 'ol', 'nav', 'article', 'section', 'aside',
    'header', 'footer', 'table'
]

# nodi del DOM che per certo rappresentano una foglia del NDOM
_TAG_LEAFS = [
    'a', 'img', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6'
]



class NaiveDOM():

    def _calc_arc_cost(self, driver: webdriver, NDOM_parent_xpath, xpath) -> float:
        #const xp = "//body/*[3]/*[5]/*[3]/*[1]/*[2]/*[1]/*[1]/*[1]/*[1]";
        #const element = document.evaluate(xp, document, null, XPathResult.FIRST_ORDERED_NODE_TYPE,null).singleNodeValue;
        #var rect = element.getBoundingClientRect();
        #rect.top
        # DOMRect { x: 205.5,
        # y: 1262.9666748046875,
        # width: 472,
        # height: 62.15000915527344,
        # top: 1262.9666748046875,
        # right: 677.5,
        # bottom: 1325.116683959961,
        # left: 205.5
        # }

        #https://stackoverflow.com/questions/15510882/selenium-get-coordinates-or-dimensions-of-element-with-python

        e = driver.find_element(By.XPATH, '//body/*[3]/*[5]/*[3]/*[1]/*[2]/*[1]/*[1]/*[1]/*[1]' )

        location = e.location
        size = e.size
        w, h = size['width'], size['height']

        print(location)
        print(size)
        print(w, h)


        return 1
    

    def _browse_DOM(self, root, DOM_parent_xpath=None, DOM_ci=0, NDOM_parent_xpath=None, driver: webdriver=None):

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

            xpath = f'{DOM_parent_xpath}/*[{(DOM_ci+1)}]'
            label = root.name

            next_NDOM_parent_xpath = xpath

        elif is_DOM_tag and root.name in _TAG_LEAFS:
            # root e' un tag foglia

            xpath = f'{DOM_parent_xpath}/*[{(DOM_ci+1)}]'
            label = ' '.join(root.stripped_strings)
        
        elif is_DOM_tag:
            # root e' un tag che non e' ne' nodo interno del NDOM ne' foglia del NDOM

            xpath = f'{DOM_parent_xpath}/*[{(DOM_ci+1)}]'
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
        
        # aggiungo nodo e arco
        if label:
            self.nodes[xpath] = label
            if not is_DOM_root:
                self.arcs.append(Arc(NDOM_parent_xpath, xpath, cost=self._calc_arc_cost(driver, NDOM_parent_xpath, xpath))) 
            
        # sfoglio ciascun sottoalbero
        if next_NDOM_parent_xpath: 
            i = 0
            for child in root.children:
                self._browse_DOM(child, DOM_parent_xpath=xpath, DOM_ci=i, NDOM_parent_xpath=next_NDOM_parent_xpath)
                i+=1


    def __init__(self, location, from_file=False, driver: webdriver=None):
        """Crea un nuovo oggetto NaiveDOM.

        Args:
            - location (string): Percorso o URL del codice sorgente 
            - from_file (bool, optional): Specifica se creare un nuovo NaiveDOM da file .html. Default: False.
        """

        if from_file:
            driver = None
            with open(location, "r") as f:
                html = f.read()
            
        else:
            # scarica il sorgente dell'URL
            r = requests.get(location, headers=defs.headers)
            html = r.text
        
        # opzionale, per velocizzare parsing con beautifulsoup
        #html = htmlmin.minify(html, remove_comments=True, remove_empty_space=True)
        html = re.sub(">\s*<","><",html)
        # parser: 'lxml' o 'html5lib' (chiudono tag lasciati aperti)
        soup = BeautifulSoup(html, 'lxml')

        self.nodes = {}
        self.nodes_coords = {}
        self.arcs = []
        self.start = None
        
        self._browse_DOM(soup.html.body)

    
    def plot(self):
        """Renderizza il NaiveDOM.
        Documentazione: https://networkx.org/documentation/latest/reference/generated/networkx.drawing.nx_pylab.draw.html
        """

        G = nx.Graph()
        G.add_nodes_from(list(self.nodes))
        G.add_edges_from([(arc.from_node, arc.to_node) for arc in self.arcs])

        pos = hierarchy_pos(G, self.start)
        nx.draw(G, pos=pos, with_labels=False)
        nx.draw_networkx_labels(G, pos, self.nodes)
        nx.draw_networkx_edge_labels(G, pos, edge_labels=dict(((arc.from_node, arc.to_node), arc.cost) for arc in self.arcs))
        
        plt.show()

if __name__ == '__main__':
    #d = NaiveDOM('source1.html', from_file=True)

    #https://stackoverflow.com/a/55878622
    options = webdriver.FirefoxOptions()
    options.add_argument(f'--user-agent={defs.headers["User-Agent"]}')
    options.add_argument(f'--width={defs.BROWSER_WIDTH}')
    options.add_argument(f'--height={defs.BROWSER_HEIGHT}')

    driver = webdriver.Firefox(options=options)
    driver.get('https://www.devitidemarco.edu.it/')

    d = NaiveDOM('https://www.devitidemarco.edu.it/', driver=driver)
    print(d.nodes)
    #print('---------------------------')
    d.plot()
    #print('############')
    #print(d.arcs)
