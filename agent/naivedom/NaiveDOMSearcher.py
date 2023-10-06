from agent.libs.aipython.searchGeneric import Path, FrontierPQ
from agent.libs.aipython.searchProblem import Search_problem


class NaiveDOMSearcher():
    """Implementazione di un algoritmo di ricerca su albero NDOM che simula il comportamento
    dell'occhio umano quando si sta visitando per la prima volta un sito web.
    Docs: report.pdf

    Attrs:
        - problem: oggetto della classe Search_problem (o sua sottoclasse) contenente il grafo
            su cui fare la ricerca
        - frontier (list): lista di percorsi da esplorare
        - num_expanded (int): numero di percorsi analizzati
    """

    def __init__(self, problem:Search_problem):
        """Crea un nuovo oggetto NaiveDOMSearcher

        Args:
            problem (SearchProblem): oggetto della classe Search_problem (o sua sottoclasse)
        """

        self.problem = problem
        self.num_expanded = 0

        self.initialize_frontier()
        self.add_to_frontier(Path(problem.start_node()), 0)

        super().__init__()

    def initialize_frontier(self):
        self.frontier = []
        
    def empty_frontier(self):
        return self.frontier == []
        
    def add_to_frontier(self, path, append_at_first=False):
        if append_at_first:
            self.frontier.insert(0, path)
        else:
            self.frontier.append(path)

    def search(self) -> Path:
        """Restituisce il (prossimo) path dal nodo iniziale a un nodo obiettivo.
        Restituisce None se non esiste un tale path.
        """

        while not self.empty_frontier():

            path = self.frontier.pop()
            current_depth = len(list(path.nodes())) - 1 #radice -> 0, figli diretti di radice -> 1

            #self.display(2, "Expanding:",path,"(cost:",path.cost,")")
            self.num_expanded += 1

            if self.problem.is_goal(path.end()):    # solution found
                #self.display(1, self.num_expanded, "paths have been expanded and",
                #            len(self.frontier), "paths remain in the frontier")
                self.solution = path   # store the solution found
                return path

            else:
                neighs = self.problem.neighbors(path.end())
                #self.display(3,"Neighbors are", neighs)

                if current_depth == 0:

                    frontier0 = FrontierPQ()
                    for arc in neighs:
                        p = Path((path, arc))
                        frontier0.add(p, p.cost)

                    while not frontier0.empty():
                        self.add_to_frontier(frontier0.pop(), append_at_first=True)         
                else:
                    for arc in reversed(list(neighs)):
                        self.add_to_frontier(Path(path,arc))
                #self.display(3,"Frontier:",self.frontier)
