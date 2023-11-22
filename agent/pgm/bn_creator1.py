import sys
import agent.definitions as defs
import numpy as np
import pandas as pd
from pandas import DataFrame

from pgmpy.models import BayesianNetwork
from pgmpy.estimators import MaximumLikelihoodEstimator
from pgmpy.inference import VariableElimination
from pgmpy.readwrite import BIFWriter
from pgmpy.factors.discrete import TabularCPD

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

BN_LATENT_VARS = ["_designer_taste"]


BN_ALREADY_KNOWN_CPTS = {
    "page_ungrouped_multim": TabularCPD(),
}

# queries da sottoporre alla bn
BN_QUERIES_DEFAULT = [
    {
        "query_desc": "P(page_template | metric=4)",
        "variables": ["page_template"],
        "evidence": {
            "metric": 4,
        },
    },
    {
        "query_desc": "P(page_template | metric=3)",
        "variables": ["page_template"],
        "evidence": {
            "metric": 3,
        },
    },
    {
        "query_desc": "P(page_ungrouped_multim)",
        "variables": ["page_ungrouped_multim"],
        "evidence": {
            "metric": 4,
        },
    },
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
        - ds (DataFrame): Dataset.
        - feature_domains (dict): domini feature.
        - mapping (dict): dizionario task->(bins, labels) con len(labels) = len(bins)-1.

    Returns:
        - DataFrame: Dataframe discretizzato.
    """
    print("Discretizing dataset...")
    features_discrete, features_continuous = get_features_types(feature_domains)

    # discretizza variabili continue
    for feature_c in features_continuous:
        if feature_c in mapping:
            bins, labels = mapping[feature_c]

            ds[feature_c] = pd.cut(
                x=ds[feature_c],
                bins=bins,
                labels=labels,
                include_lowest=True,
                right=False,
            )
            ds[feature_c] = ds[feature_c].astype(int)  # int64

    # rendi int variabili discrete
    float64 = np.dtype("float64")
    for feature_d in features_discrete:
        if ds[feature_d].dtype is float64:
            ds[feature_d] = ds[feature_d].astype(int)  # np.int64


def bn_kf_cv(ds, bn):
    pass


if __name__ == "__main__":
    # leggi dataset
    print("Reading dataset...")
    ds = pd.read_csv(defs.ds3_gt_no_noise_path)
    ds = ds.drop(defs.ds3_features_excluded, axis=1)

    # discretizza perchè pgmpy supporta inferenza su distribuzione discrete
    discretize_dataset(
        ds=ds,
        feature_domains=defs.ds3_gt_feature_domains,
        mapping=defs.DISCRETE_MAPPING_DEFAULT,
    )

    # -----crea bn (aggiunge automaticamente i nodi)
    print("Creating BN structure...")

    for latent_var in BN_LATENT_VARS:
        ds[latent_var] = np.nan

    bn = BayesianNetwork(ebunch=BN_EDGES_DEFAULT, latents=BN_LATENT_VARS)
    bn.name = "bn_gt"

    # apprendimento parametri (cpd) della rete bayesiana
    print("Learning BN parameters (CPDs)...")
    unknown_cpts_vars = list(set(bn.nodes()) - set(BN_ALREADY_KNOWN_CPTS.keys()))

    bn_mle = MaximumLikelihoodEstimator(bn, ds)
    bn_cpts = [bn_mle.estimate_cpd(var) for var in unknown_cpts_vars]

    bn.add_cpds(bn_cpts)
    bn.add_cpds(BN_ALREADY_KNOWN_CPTS.values())

    """
    print("Learning BN parameters (CPDs)...")
    bn_state_names = {
        feature: [state_name for state_name in state_names]
        for feature, (bins, state_names) in defs.DISCRETE_MAPPING_DEFAULT.items()
    }
    bn.fit(ds, state_names=bn_state_names)
    """

    # controlla bn
    if bn.check_model():
        print("Saving...")
        writer = BIFWriter(bn)
        writer.write_bif(f"./bif/{bn.name}.bif")

    # query
    infer_engine = VariableElimination(bn)
    i = 1
    for query_args in BN_QUERIES_DEFAULT:
        # mostra la descrizione e rimuovila dal dizionario degli argomenti della query
        query_desc = query_args.pop("query_desc", None)
        print(f"- Query #{i}: {query_desc}")
        query_obj = infer_engine.query(**query_args)
        print(query_obj)
        i = i + 1

    print("Done.")
