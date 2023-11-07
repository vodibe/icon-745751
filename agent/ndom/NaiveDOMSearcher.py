from agent.libs.aipython.searchGeneric import Path, FrontierPQ
from agent.libs.aipython.searchProblem import Search_problem


class NaiveDOMSearcher:
    """Implementazione di un algoritmo di ricerca su albero NDOM che simula il comportamento
    dell'occhio umano quando si sta visitando per la prima volta un sito web.
    Docs: report.pdf

    Attrs:
        - problem: oggetto della classe Search_problem (o sua sottoclasse) contenente il grafo
            su cui fare la ricerca
        - frontier (list): lista di percorsi da esplorare
        - num_expanded (int): numero di percorsi analizzati
        - solution (Path): percorso soluzione
    """

    def __init__(self, problem: Search_problem):
        """Crea un nuovo oggetto NaiveDOMSearcher.

        Args:
            - problem (SearchProblem): oggetto della classe Search_problem (o sua sottoclasse)
        """

        self.problem = problem
        self.solution = None

        self.frontier = []
        self.frontier.append(Path(problem.start_node()))

        self.num_expanded = 0
        # self.add_to_frontier(Path(problem.start_node()))

        super().__init__()

    def set_problem_goals(self, goals):
        """Modifica i nodi obiettivo del problema, senza modificare gli altri attributi

        Args:
            - goals (list): Lista dei nuovi nodi obiettivo
        """

        self.problem.set_goals(goals)

    def search(self) -> Path:
        """Restituisce il (prossimo) path dal nodo iniziale a un nodo obiettivo.
        Restituisce None se non esiste un tale path.
        """

        while not self.frontier == []:
            # preleva un percorso dalla frontiera (il primo da destra della lista)
            path = self.frontier.pop()
            self.num_expanded += 1

            # se l'ultimo nodo del percorso prelevato ha profondità 0 o 1
            # lo metto in una coda con priorità (peso = costo), in modo che il
            # primo da destra della lista (cioe' il prossimo ad essere esaminato) sia
            # il percorso meno costoso
            current_depth = len(list(path.nodes())) - 1

            if self.problem.is_goal(path.end()):
                # nodo obiettivo trovato
                self.solution = path  # store the solution found
                return path

            else:
                neighs = self.problem.neighbors(path.end())

                if current_depth == 0:
                    frontier_depth_1 = FrontierPQ()
                    for arc in neighs:
                        path_depth_1 = Path(path, arc)
                        frontier_depth_1.add(path_depth_1, path_depth_1.cost)

                    while not frontier_depth_1.empty():
                        self.frontier.insert(0, frontier_depth_1.pop())
                else:
                    for arc in reversed(list(neighs)):
                        self.frontier.append(Path(path, arc))
