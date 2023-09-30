import requests
from bs4 import BeautifulSoup, NavigableString, Comment
import networkx as nx
import matplotlib.pyplot as plt
import random
from agent.definitions import headers
from agent.libs.aipython.searchProblem import Arc


class NaiveDOM():
    """Classe che modella un albero DOM piu' semplice rispetto a quello dell'intero
    codice sorgente della pagina. La radice e' l'elemento <html>, i nodi interni sono
    tutti i nodi che hanno tra i loro discendenti le stringhe visibili sulla pagina.
    Le foglie sono le stringhe visibili sulla pagina.

    Attributes:
        NDOM_nodes: dizionario xpath:label dei nodi
        NDOM_arcs: lista di oggetti Arc della libreria aipython
        NDOM_start: xpath del nodo radice
    """

    # dimensione massima dell'etichetta dei nodi
    MAX_LABEL_LENGTH = 40

    # nodi da non esplorare perche' non contengono stringhe visibili sulla pagina
    tag_blacklist = [
        'head', 'script', 'style', 'svg', 'meta', 'link', 'title', 'base',
        'noscript', 'template', 'iframe', 'canvas', 'object', 'embed', 'param',
        'applet', 'frame', 'frameset', 'map', 'area', 'track', 'data',
        'figure', 'figcaption', "\n"
    ]

    # nodi che contengono stringhe visibili sulla pagina
    tag_parents = [
        'html', 'body', 'ul', 'ol', 'nav', 'article', 'section', 'aside',
        'header', 'footer', 'table'
    ]

    def __init__(self, location, from_file=False):
        """Crea un nuovo oggetto NaiveDOM

        Args:
            location (string): Percorso o URL del codice sorgente 
            from_file (bool, optional): Specifica se creare un nuovo NaiveDOM da file. Default: False.
        """
        
        if from_file:
            with open(location) as f:
                soup = BeautifulSoup(f, 'lxml')
        else:
            # scarica il sorgente dell'URL
            r = requests.get(location, headers=headers)

            # scrivi la risposta su file
            #with open("dom.txt", "w", encoding='utf-8') as file:
            #    file.write(r.text)
            
            soup = BeautifulSoup(r.text, 'lxml')
            
        self.NDOM_nodes = {}
        self.NDOM_arcs = []
        self.NDOM_start = None

        def _browse_DOM(root, parent_xpath='', ci=0, NDOM_parent_xpath=''):
            """Sfoglia l'albero DOM radicato in root al fine di popolare gli attributi
            NDOM_nodes, NDOM_arcs, NDOM_start. Assume che:
            - root sia il ci-esimo figlio del nodo genitore parent_xpath.
            - nel caso in cui root viene aggiunto al NaiveDOM, esso avra' come genitore il
              nodo con xpath pari a NDOM_parent_xpath

            Args:
                root: nodo radice
                parent_xpath (str, optional): nodo genitore del DOM. Default: ''.
                ci (int, optional): indice di root tra tutti i suoi fratelli. Default: 0.
                NDOM_parent_xpath (str, optional): nodo genitore del NDOM. Default: ''.
            """
            
            # root e' un tag in blacklist, poto l'albero radicato in root
            if root.name in NaiveDOM.tag_blacklist or isinstance(root, Comment):
                return

            try:
                root.string = root.string.strip()
                if len(root.string) == 0:
                    return
            except Exception as e:
                print(e)
                pass
                
            ignore_NDOM_node = False
            is_NDOM_leaf = False

            if not parent_xpath and not NDOM_parent_xpath:
                # root e' la radice del codice sorgente
                # metto next_NDOM_parent_xpath = xpath perche' i prossimi figli dell'albero 
                # NDOM avranno come genitore questo nodo
                xpath = f'{parent_xpath}/{root.name}'
                next_NDOM_parent_xpath = xpath
                self.NDOM_start = xpath

            elif root.name in NaiveDOM.tag_parents:
                # root e' un tag che presuppone che contenga al suo interno, tra i discendenti,
                # almeno una stringa, ad es. body, ul, ...
                xpath = f'{parent_xpath}/*[{ci}]'
                next_NDOM_parent_xpath = xpath

            elif isinstance(root, NavigableString):
                # root non e' un tag ma una stringa
                # l'xpath di questa stringa e' quello del genitore
                is_NDOM_leaf = True
                xpath = parent_xpath

            else:
                # root e' un tag adibito alla costruzione della pagina, ad es. div, span, ...
                ignore_NDOM_node = True
                xpath = f'{parent_xpath}/*[{ci}]'
                next_NDOM_parent_xpath = NDOM_parent_xpath
            
            if not ignore_NDOM_node:
                # aggiungo nodo. la sua label e' il nome del tag oppure la stringa stessa
                if not is_NDOM_leaf:
                    label = root.name
                elif len(root.string) < NaiveDOM.MAX_LABEL_LENGTH:
                    label = root.string
                else:
                    label = root.string[:NaiveDOM.MAX_LABEL_LENGTH]

                self.NDOM_nodes[xpath] = label

                # aggiungo arco
                if NDOM_parent_xpath and parent_xpath: 
                    self.NDOM_arcs.append(Arc(NDOM_parent_xpath, xpath))

            # sfoglio ciascun sottoalbero
            if not is_NDOM_leaf: 
                i = 0
                for child in root.children:
                    _browse_DOM(child, parent_xpath=xpath, ci=i, NDOM_parent_xpath=next_NDOM_parent_xpath)
                    i+=1
        
        # uso parser html5lib
        #print(soup.html)
        _browse_DOM(soup.html)

        '''
        for child in soup.children:
            if child.name in NaiveDOM.tag_parents:
                print('--------------')
                print(child.name)
                _browse_DOM(child)
                return
        '''

    
    def plot(self):
        """Renderizza il NaiveDOM.
        Documentazione:
        https://networkx.org/documentation/latest/reference/generated/networkx.drawing.nx_pylab.draw.html
        """

        NDOM = nx.Graph()
        NDOM.add_nodes_from(list(self.NDOM_nodes))
        NDOM.add_edges_from([(arc.from_node, arc.to_node) for arc in self.NDOM_arcs])

        pos = hierarchy_pos(NDOM, self.NDOM_start)
        #pos = hierarchy_pos(NDOM, self.NDOM_start)
        nx.draw(NDOM, pos=pos, with_labels=False)
        nx.draw_networkx_labels(NDOM, pos, self.NDOM_nodes)
        
        plt.show()

def hierarchy_pos(G, root=None, width=1., vert_gap = 0.2, vert_loc = 0, xcenter = 0.5):
    '''
    From Joel's answer at https://stackoverflow.com/a/29597209/2966723.  
    Licensed under Creative Commons Attribution-Share Alike 
    
    If the graph is a tree this will return the positions to plot this in a 
    hierarchical layout.
    
    G: the graph (must be a tree)
    
    root: the root node of current branch 
    - if the tree is directed and this is not given, 
      the root will be found and used
    - if the tree is directed and this is given, then 
      the positions will be just for the descendants of this node.
    - if the tree is undirected and not given, 
      then a random choice will be used.
    
    width: horizontal space allocated for this branch - avoids overlap with other branches
    
    vert_gap: gap between levels of hierarchy
    
    vert_loc: vertical location of root
    
    xcenter: horizontal location of root
    '''
    if not nx.is_tree(G):
        raise TypeError('cannot use hierarchy_pos on a graph that is not a tree')

    if root is None:
        if isinstance(G, nx.DiGraph):
            root = next(iter(nx.topological_sort(G)))  #allows back compatibility with nx version 1.11
        else:
            root = random.choice(list(G.nodes))

    def _hierarchy_pos(G, root, width=1., vert_gap = 0.2, vert_loc = 0, xcenter = 0.5, pos = None, parent = None):
        '''
        see hierarchy_pos docstring for most arguments

        pos: a dict saying where all nodes go if they have been assigned
        parent: parent of this branch. - only affects it if non-directed

        '''
    
        if pos is None:
            pos = {root:(xcenter,vert_loc)}
        else:
            pos[root] = (xcenter, vert_loc)
        children = list(G.neighbors(root))
        if not isinstance(G, nx.DiGraph) and parent is not None:
            children.remove(parent)  
        if len(children)!=0:
            dx = width/len(children) 
            nextx = xcenter - width/2 - dx/2
            for child in children:
                nextx += dx
                pos = _hierarchy_pos(G,child, width = dx, vert_gap = vert_gap, 
                                    vert_loc = vert_loc-vert_gap, xcenter=nextx,
                                    pos=pos, parent = root)
        return pos
 
    return _hierarchy_pos(G, root, width, vert_gap, vert_loc, xcenter)

if __name__ == '__main__':
    #d = NaiveDOM('source.html', from_file=True)
    d = NaiveDOM('https://www.devitidemarco.edu.it/')
    #print(d.NDOM_nodes)
    d.plot()
    #print(d.NDOM_nodes)
    #print(d.NDOM_arcs)
