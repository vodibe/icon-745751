import agent.definitions as defs
import numpy as np
import pandas as pd
from pandas import DataFrame

from pgmpy.models import BayesianNetwork
from pgmpy.inference import VariableElimination
from pgmpy.readwrite import BIFWriter


# archi bn, il primo elemento della tupla è l'elemento da cui parte l'arco
BN_EDGES_DEFAULT = [
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
    print("Discretizing dataset...")
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


if __name__ == "__main__":
    # leggi dataset
    print("Reading dataset...")
    ds = pd.read_csv(defs.ds3_gt_no_noise_path)

    # discretizza perchè pgmpy support inferenza su distribuzione discrete
    discretize_dataset(
        ds=ds, feature_domains=defs.ds3_gt_feature_domains, mapping=defs.DISCRETE_MAPPING_DEFAULT
    )

    # crea bn (aggiunge automaticamente i nodi)
    print("Creating BN structure...")
    bn = BayesianNetwork(ebunch=BN_EDGES_DEFAULT)
    bn.name = "bn_gt"

    # usa stimatore mle
    bn.fit(ds)

    writer = BIFWriter(bn)
    writer.write_bif(f"./bif/{bn.name}.bif")

    # infer_engine = VariableElimination()

    print(bn.nodes())
