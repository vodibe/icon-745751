import agent.definitions as defs
import numpy as np
import pandas as pd
from pandas import DataFrame
from pgmpy.models import BayesianNetwork
from pgmpy.factors.discrete import TabularCPD
from pgmpy.readwrite import BIFReader


# nodi bn
BN_NODES_DEFAULT = [
    "_designer_taste",  # aggiuntiva
    "page_template",
    "page_menu_or",
    "page_ungrouped_multim",
    "metric",
    # "page_load_time_ms",
    # "page_width",
    "page_height",
    "NDOM_nodes",
    "NDOM_height",
    "task1",
    "task2",
    "task3",
]

# archi bn, il primo elemento della tupla è l'elemento da cui parte l'arco
BN_EDGES_DEFAULT = [
    ("_designer_taste", "page_ungrouped_multim"),
    ("page_template", "page_menu_or"),
    ("page_template", "page_height"),
    ("page_template", "page_ungrouped_multim"),
    ("page_ungrouped_multim", "page_height"),
    ("page_height", "NDOM_nodes"),
    ("page_height", "NDOM_height"),
    ("NDOM_nodes", "task1"),
    ("NDOM_nodes", "task2"),
    ("NDOM_nodes", "task3"),
    ("NDOM_height", "task1"),
    ("NDOM_height", "task2"),
    ("NDOM_height", "task3"),
    ("page_menu_or", "metric"),
    ("page_ungrouped_multim", "metric"),
    ("page_height", "metric"),
]

# tabelle probabilità condizionata
BN_CPTS_DEFAULT = [
    # fmt: off
    TabularCPD(
        variable="_designer_taste",
        variable_card=2,
        values=[
            [0.5],
            [0.5]
        ]
    ),
    TabularCPD(
        variable="page_menu_or",
        variable_card=4,
        values=[  # P(page_menu_or = x | page_template)
            [0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.16],
            [0.97, 0.02, 0.04, 0.02, 0.97, 0.93, 0.93, 0.97, 0.28],
            [0.01, 0.02, 0.05, 0.02, 0.01, 0.01, 0.01, 0.01, 0.28],
            [0.01, 0.95, 0.90, 0.95, 0.01, 0.05, 0.05, 0.01, 0.28],
        ],
        evidence=["page_template"],
        evidence_card=[9]
    ),
    # 0-5   1
    # 6-10  2
    # 11-20 3
    # 21+   4
    TabularCPD(
        variable="page_ungrouped_multim",
        variable_card=4,
        values=[
            # 11     12   21     22   31    32     41   42    51     52   61    62    71    72    81    82    91     92
            [0.45, 0.70, 0.22, 0.65, 0.22, 0.65, 0.22, 0.65, 0.60, 0.85, 0.30, 0.50, 0.42, 0.78, 0.08, 0.08, 0.2, 0.3],
            [0.45, 0.20, 0.55, 0.25, 0.55, 0.25, 0.55, 0.25, 0.40, 0.15, 0.50, 0.48, 0.48, 0.21, 0.42, 0.50, 0.2, 0.3],
            [0.05, 0.07, 0.22, 0.05, 0.22, 0.05, 0.22, 0.05, 0.00, 0.00, 0.15, 0.01, 0.05, 0.01, 0.42, 0.40, 0.3, 0.2],
            [0.05, 0.03, 0.01, 0.05, 0.01, 0.05, 0.01, 0.05, 0.00, 0.00, 0.05, 0.01, 0.05, 0.00, 0.08, 0.02, 0.3, 0.2]
        ],
        evidence=["page_template", "_designer_taste"],
        evidence_card=[9, 2]
    )
    # fmt: on
]


def get_features_types(feature_domains: dict):
    """Restituisce le features discrete e continue.

    Args:
        - features_domains: Dizionario feature:dominio che rispetta la sintassi.

    Returns:
        - features_d: Lista di features discrete.
        - features_c: Lista di features continue.
    """

    features_d = []
    features_c = []

    for feature, domain in feature_domains.items():
        if isinstance(domain, list):  # or (domain is str)
            features_d.append(feature)
        elif callable(domain) and isinstance(domain, type(lambda x: x)):
            features_c.append(feature)
        else:
            pass

    return features_d, features_c


def discretize_dataset(ds: DataFrame, feature_domains: dict, mapping: dict):
    """Discretizza dataset (composto da feature il cui dominio è memorizzato in
    feature_domains) secondo mapping.

    Args:
        ds (DataFrame): Dataset.
        feature_domains (dict): domini feature.
        mapping (dict): dizionario task->(bins, labels) con len(labels) = len(bins)-1.

    Returns:
        DataFrame: Dataframe discretizzato.
    """

    features_discrete, features_continuous = get_features_types(feature_domains)

    # discretizza variabili continue
    for feature_c in features_continuous:
        if feature_c in mapping:
            bins, labels = mapping[feature_c]

            ds[feature_c] = pd.cut(
                x=ds[feature_c], bins=bins, labels=labels, include_lowest=True, right=False
            )
            ds[feature_c] = ds[feature_c].astype("int64")

    # rendi int variabili discrete
    float64 = np.dtype("float64")
    for feature_d in features_discrete:
        if ds[feature_d].dtype is float64:
            ds[feature_d] = ds[feature_d].astype(np.int64)


def create_default_bn(
    path=None,
    nodes: list = BN_NODES_DEFAULT,
    edges: list = BN_EDGES_DEFAULT,
    cpds: list = BN_CPTS_DEFAULT,
) -> BayesianNetwork:
    """Crea una BN con i nodi, archi e CPT specificate. In alternativa, legge una BN
    da file ignorando i nodi archi e CPT specificate.

    Args:
        path (optional): None se la BN non è stata ancora salvata su file. Altrimenti il percorso. Default: None.
        nodes (list, optional): Nodi della BN. Default: BN_NODES_DEFAULT.
        edges (list, optional): Archi della BN. Default: BN_EDGES_DEFAULT.
        cpds (list, optional): CPT della BN. Default: BN_CPTS_DEFAULT.

    Returns:
        BayesianNetwork: BN creata.
    """

    if path:
        # leggi bn da file
        bif_reader = BIFReader(path)
        bn = bif_reader.get_model()

    else:
        # crea nuova bn
        bn = BayesianNetwork()
        bn.add_nodes_from(nodes)
        bn.add_edges_from(edges)
        bn.add_cpds(*cpds)

        # controlla le cpt
        if bn.check_model():
            # salva su file .bif
            bn.save(f"./bif/{bn.name}.bif")

    return bn


if __name__ == "__main__":
    # leggi dataset
    ds = pd.read_csv(defs.ds3_gt_no_noise_path)

    # discretizza perchè pgmpy support inferenza su distribuzione discrete
    discretize_dataset(
        ds=ds, feature_domains=defs.ds3_gt_feature_domains, mapping=defs.DISCRETE_MAPPING_DEFAULT
    )

    # crea bn
    bn = create_default_bn()

    print(type(bn))
