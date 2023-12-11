from pgmpy.estimators import K2Score, HillClimbSearch, BayesianEstimator
from pgmpy.models import BayesianNetwork, BayesianModel
from pgmpy.inference import VariableElimination
import warnings

from networkx.drawing.nx_pydot import graphviz_layout


# %%% Rete Bayesiana %%%
dataset = pd.read_csv("breast-cancer.csv")
dataset = dataset.drop("id", axis=1)
dataset["diagnosis"] = dataset["diagnosis"].map({"B": 0, "M": 1})


def converti_float_in_interi(dataset):
    for colonna in dataset.columns:
        if dataset[colonna].dtype == "float64":
            dataset[colonna] = dataset[colonna].astype(int)
    return dataset


dataset = converti_float_in_interi(dataset)
df_RBayes = pd.DataFrame(np.array(dataset.copy(), dtype=int), columns=dataset.columns)

k2 = K2Score(df_RBayes)
hc_k2 = HillClimbSearch(df_RBayes)
modello_k2 = hc_k2.(scoring_method=k2)


print(modello_k2)


# ----------------apprende--------------------

rete_bayesiana = BayesianNetwork(modello_k2.edges())
rete_bayesiana.fit(df_RBayes)


# Stampa dei nomi dei nodi
print("Nodi della rete bayesiana:")
for node in rete_bayesiana.nodes():
    print(node)

# Stampa degli archi nella rete bayesiana
print("\nArchi nella rete bayesiana:")
for edge in rete_bayesiana.edges():
    print(edge)


def visualizza_rete_bayesiana(nodi, archi):
    # Creazione del grafo della rete bayesiana
    grafo = nx.DiGraph()
    grafo.add_nodes_from(nodi)
    grafo.add_edges_from(archi)

    # Stampa della rete bayesiana
    plt.figure(figsize=(10, 8))
    pos = graphviz_layout(grafo, prog="dot")
    nx.draw_networkx(
        grafo,
        pos,
        node_color="lightblue",
        node_size=500,
        alpha=0.8,
        arrows=True,
        arrowstyle="->",
        arrowsize=10,
        font_size=10,
        font_family="sans-serif",
    )

    plt.title("Rete Bayesiana")
    plt.axis("off")
    plt.show()


# -----------------------------------------------
# Esempio di utilizzo
nodi = [
    "diagnosis",
    "concavity_mean",
    "radius_mean",
    "compactness_se",
    "symmetry_mean",
    "perimeter_mean",
    "texture_se",
    "fractal_dimension_se",
    "compactness_worst",
    "concavity_se",
    "concave points_se",
    "concavity_worst",
    "perimeter_se",
    "area_worst",
    "compactness_mean",
    "fractal_dimension_mean",
    "area_se",
    "area_mean",
    "concave points_worst",
    "concave points_mean",
    "smoothness_mean",
    "radius_worst",
    "symmetry_worst",
    "smoothness_se",
    "symmetry_se",
    "perimeter_worst",
    "texture_worst",
    "smoothness_worst",
    "texture_mean",
    "fractal_dimension_worst",
    "radius_se",
]

archi = [
    ("diagnosis", "concavity_mean"),
    ("concavity_mean", "concavity_se"),
    ("concavity_mean", "concave points_se"),
    ("concavity_mean", "concavity_worst"),
    ("radius_mean", "compactness_se"),
    ("radius_mean", "symmetry_mean"),
    ("compactness_se", "area_se"),
    ("compactness_se", "area_mean"),
    ("perimeter_mean", "texture_se"),
    ("perimeter_mean", "fractal_dimension_se"),
    ("perimeter_mean", "compactness_worst"),
    ("concavity_se", "concave points_worst"),
    ("concave points_se", "concave points_mean"),
    ("concave points_se", "smoothness_mean"),
    ("concavity_worst", "perimeter_worst"),
    ("concavity_worst", "radius_worst"),
    ("perimeter_se", "area_worst"),
    ("perimeter_se", "compactness_mean"),
    ("perimeter_se", "fractal_dimension_mean"),
    ("area_worst", "fractal_dimension_worst"),
    ("area_worst", "texture_worst"),
    ("area_worst", "radius_se"),
    ("radius_worst", "symmetry_worst"),
    ("radius_worst", "perimeter_se"),
    ("radius_worst", "smoothness_se"),
    ("radius_worst", "symmetry_se"),
    ("radius_worst", "perimeter_worst"),
    ("symmetry_worst", "radius_mean"),
    ("perimeter_worst", "smoothness_mean"),
    ("texture_worst", "smoothness_worst"),
    ("texture_worst", "texture_mean"),
    ("smoothness_worst", "perimeter_mean"),
]

# Chiamata alla funzione per visualizzare il grafico
visualizza_rete_bayesiana(nodi, archi)

modello_bayesiano = BayesianModel(archi)

# Aggiungi le variabili al modello
for column in dataset.columns:
    if column != "diagnosis":
        modello_bayesiano.add_node(column)

# -------------------------------------------------------------------

bayes_estimator = BayesianEstimator  # io devo usare il mle

# Aggiorna il modello con le nuove CPD
modello_bayesiano.fit(
    dataset, estimator=bayes_estimator, prior_type="BDeu", equivalent_sample_size=10
)

# Esempio di inferenza sulla rete bayesiana
inferenza = VariableElimination(modello_bayesiano)

# Stampa i valori limite (massimo e minimo) per ogni variabile del modello
for variable in modello_bayesiano.nodes:
    cpd = modello_bayesiano.get_cpds(variable)
    min_value = cpd.values.min()
    max_value = cpd.values.max()
    print(f"Valori limite per la variabile '{variable}':")
    print(f"Minimo: {min_value}")
    print(f"Massimo: {max_value}")
    print("\n")


maligno = inferenza.query(
    variables=["diagnosis"],
    evidence={
        "radius_mean": 17.99,
        "texture_mean": 10.38,
        "perimeter_mean": 122.8,
        "area_mean": 1001,
        "smoothness_mean": 0.1184,
        "compactness_mean": 0.2776,
        "concavity_mean": 0.300,
        "concave points_mean": 0.1471,
        "symmetry_mean": 0.2419,
        "fractal_dimension_mean": 0.07871,
        "radius_se": 1.095,
        "texture_se": 0.9053,
        "perimeter_se": 8.589,
        "area_se": 153.4,
        "smoothness_se": 0.006399,
        "compactness_se": 0.04904,
        "concavity_se": 0.05373,
        "concave points_se": 0.01587,
        "symmetry_se": 0.03003,
        "fractal_dimension_se": 0.006193,
        "radius_worst": 25.38,
        "texture_worst": 17.33,
        "perimeter_worst": 184.6,
        "area_worst": 2019,
        "smoothness_worst": 0.1622,
        "compactness_worst": 0.6656,
        "concavity_worst": 0.7119,
        "concave points_worst": 0.2654,
        "symmetry_worst": 0.4601,
        "fractal_dimension_worst": 0.1189,
    },
)

print("\nProbabilità per una donna di avere un tumore maligno al seno: ")
print(maligno, "\n")

benigno = inferenza.query(
    variables=["diagnosis"],
    evidence={
        "radius_mean": 12,
        "texture_mean": 15.65,
        "perimeter_mean": 76.95,
        "area_mean": 443.3,
        "smoothness_mean": 0.09723,
        "compactness_mean": 0.07165,
        "concavity_mean": 0.04151,
        "concave points_mean": 0.01863,
        "symmetry_mean": 0.2079,
        "fractal_dimension_mean": 0.05968,
        "radius_se": 0.2271,
        "texture_se": 1.255,
        "perimeter_se": 1.441,
        "area_se": 16.16,
        "smoothness_se": 0.005969,
        "compactness_se": 0.01812,
        "concavity_se": 0.02007,
        "concave points_se": 0.007027,
        "symmetry_se": 0.01972,
        "fractal_dimension_se": 0.002607,
        "radius_worst": 13.67,
        "texture_worst": 24.9,
        "perimeter_worst": 87.78,
        "area_worst": 1603,
        "smoothness_worst": 0.1398,
        "compactness_worst": 0.2089,
        "concavity_worst": 0.3157,
        "concave points_worst": 0.1642,
        "symmetry_worst": 0.3695,
        "fractal_dimension_worst": 0.08579,
    },
)

print("\nProbabilità per una donna di avere un tumore benigno al seno: ")
print(benigno, "\n\n")
