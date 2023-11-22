import sys
import agent.definitions as defs
import numpy as np
import pandas as pd
from pandas import DataFrame

from pgmpy.models import BayesianNetwork
from pgmpy.estimators import MaximumLikelihoodEstimator, ExpectationMaximization
from pgmpy.inference import VariableElimination
from pgmpy.readwrite import BIFWriter
from pgmpy.factors.discrete import TabularCPD

# variabili latenti
BN_LATENT_VARS = ["_designer_taste"]

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

# cpt già conosciute
BN_KNOWN_CPTS = {
    # fmt: off
    # 0-5   1
    # 6-10  2
    # 11-20 3
    # 21+   4
    TabularCPD(
        variable="page_ungrouped_multim",
        variable_card=len(defs.DISCRETE_MAPPING_DEFAULT["page_ungrouped_multim"][1]),
        values=[
            # 11     12   21     22   31    32     41   42    51     52   61    62    71    72    81    82    91     92
            [0.45, 0.70, 0.22, 0.65, 0.22, 0.65, 0.22, 0.65, 0.60, 0.85, 0.30, 0.50, 0.42, 0.78, 0.08, 0.08, 0.2, 0.3],
            [0.45, 0.20, 0.55, 0.25, 0.55, 0.25, 0.55, 0.25, 0.40, 0.15, 0.50, 0.48, 0.48, 0.21, 0.42, 0.50, 0.2, 0.3],
            [0.05, 0.07, 0.22, 0.05, 0.22, 0.05, 0.22, 0.05, 0.00, 0.00, 0.15, 0.01, 0.05, 0.01, 0.42, 0.40, 0.3, 0.2],
            [0.05, 0.03, 0.01, 0.05, 0.01, 0.05, 0.01, 0.05, 0.00, 0.00, 0.05, 0.01, 0.05, 0.00, 0.08, 0.02, 0.3, 0.2]
        ],
        evidence=["page_template", "_designer_taste"],
        evidence_card=[
            len(defs.DISCRETE_MAPPING_DEFAULT["page_template"][1]),
            len(defs.DISCRETE_MAPPING_DEFAULT["_designer_taste"][1])
        ]
    ),
    TabularCPD(
        variable="page_menu_or",
        variable_card=len(defs.DISCRETE_MAPPING_DEFAULT["page_menu_or"][1]),
        values=[  # P(page_menu_or = x | page_template)
            [0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.16],
            [0.97, 0.02, 0.04, 0.02, 0.97, 0.93, 0.93, 0.97, 0.28],
            [0.01, 0.02, 0.05, 0.02, 0.01, 0.01, 0.01, 0.01, 0.28],
            [0.01, 0.95, 0.90, 0.95, 0.01, 0.05, 0.05, 0.01, 0.28],
        ],
        evidence=["page_template"],
        evidence_card=[
            len(defs.DISCRETE_MAPPING_DEFAULT["page_template"][1])
        ]
    ),
    # fmt: on
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
        if feature_c in mapping and feature_c in ds:
            bins, labels = mapping[feature_c]

            ds[feature_c] = pd.cut(
                x=ds[feature_c],
                bins=bins,
                labels=labels,
                include_lowest=True,
                right=False,
            )
            ds[feature_c] = ds[feature_c].astype("int64")  # int64

    # rendi int variabili discrete
    float64 = np.dtype("float64")
    for feature_d in features_discrete:
        if feature_d in ds and ds[feature_d].dtype is float64:
            ds[feature_d] = ds[feature_d].astype(np.int64)  # np.int64


def bn_kf_cv(ds, bn):
    pass


if __name__ == "__main__":
    # ----- operazioni su dataset
    # leggi dataset
    print("Reading dataset...")
    ds = pd.read_csv(defs.ds3_gt_no_noise_path)
    bn_features_excluded = defs.ds3_features_excluded + [
        "page_load_time_ms",
        "task4",
        "task5",
        "task6",
        "task7",
        "task8",
    ]
    ds = ds.drop(bn_features_excluded, axis=1)

    # discretizza perchè pgmpy supporta inferenza su distribuzioni discrete
    discretize_dataset(
        ds=ds,
        feature_domains=defs.ds3_gt_feature_domains,
        mapping=defs.DISCRETE_MAPPING_DEFAULT,
    )

    # ----- crea bn
    print("Creating BN structure...")
    bn = BayesianNetwork(ebunch=BN_EDGES_DEFAULT, latents=BN_LATENT_VARS)
    bn.name = "bn_gt.bif"
    bn_state_names = {
        feature: state_names
        for feature, (bins, state_names) in defs.DISCRETE_MAPPING_DEFAULT.items()
    }

    # ----- apprendimento parametri (cpd) della rete bayesiana
    print("Learning BN parameters (CPTs)...")
    bn.add_cpds(*BN_KNOWN_CPTS)
    bn.fit(
        ds,
        estimator=ExpectationMaximization,
        state_names=bn_state_names,
    )

    # non serve
    unknown_cpt_vars = list(
        set(bn.nodes()) - set({cpt.variable for cpt in BN_KNOWN_CPTS})
    )

    # -----------controlla bn
    if bn.check_model():
        print("Saving...")
        bif_writer = BIFWriter(bn)
        bif_writer.write_bif(defs.DIR_BIF / bn.name)

    for cpd in bn.get_cpds():
        print(cpd)

    sys.exit(0)

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
