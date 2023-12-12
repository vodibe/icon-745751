import sys
import agent.definitions as defs
import numpy as np
import pandas as pd
from pandas import DataFrame

from pgmpy.models import BayesianNetwork
from pgmpy.estimators import MaximumLikelihoodEstimator, BayesianEstimator
from pgmpy.inference import VariableElimination
from pgmpy.readwrite import BIFWriter


# dizionario feature:dominio discreto
BN_STATE_NAMES = {
    feature: state_names
    for feature, (bins, state_names) in defs.DS_DISCRETE_MAPPING_DEFAULT.items()
}

# archi bn, il primo elemento della tupla è l'elemento da cui parte l'arco
BN_EDGES_DEFAULT = [
    ("page_template", "page_menu_or"),
    ("page_template", "page_height"),
    ("page_template", "page_ungrouped_multim"),
    ("page_ungrouped_multim", "page_height"),
    ("page_height", "NDOM_nodes"),
    ("page_height", "NDOM_height"),
    ("page_menu_or", "metric"),
    ("page_ungrouped_multim", "metric"),
    ("page_height", "metric"),
]

# queries da sottoporre alla bn
BN_QUERIES_DEFAULT = [
    # -----
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
        "query_desc": "P(page_template | metric=2)",
        "variables": ["page_template"],
        "evidence": {
            "metric": 2,
        },
    },
    # -----
    {
        "query_desc": "P(page_template | page_ungrouped_multim=1)",
        "variables": ["page_template"],
        "evidence": {
            "page_ungrouped_multim": 1,
        },
    },
    {
        "query_desc": "P(page_template | page_ungrouped_multim=2)",
        "variables": ["page_template"],
        "evidence": {
            "page_ungrouped_multim": 2,
        },
    },
    {
        "query_desc": "P(page_template | page_ungrouped_multim=3)",
        "variables": ["page_template"],
        "evidence": {
            "page_ungrouped_multim": 3,
        },
    },
    # -----
    {
        "query_desc": "P(NDOM_nodes, NDOM_height | page_template=4)",
        "variables": ["NDOM_nodes", "NDOM_height"],
        "evidence": {
            "page_template": 4,
        },
    },
]

"""
https://pgmpy.org/param_estimator/bayesian_est.html

prior_type: dirichlet
    
    pseudo_counts = dirichlet hyperparameters; a single number or 2-D array of shape
    (node_card, product of parents_card) with a "virtual" count for each variable state in the CPD.
    The virtual counts are added to the actual state counts found in the data.
    (if a list is provided, a lexicographic ordering of states is assumed)

prior_type: BDeu (Bayesian Dirichlet equivalent uniform)

    equivalent_sample_size must be specified instead of pseudo_counts.
    This is equivalent to prior_type=dirichlet and using uniform pseudo_counts of
    equivalent_sample_size/(node_cardinality*np.prod(parents_cardinalities)).

prior_type: K2 is a shorthand for dirichlet + setting every pseudo_count to 1,
regardless of the cardinality of the variable.
"""
# fmt:off
BN_MAP_ESTIMATOR_PARAMS = [
    {
        "node": "page_template",
        "prior_type": "K2"
        # "pseudo_counts": [
        #    [1, 1, 1, 1, 1, 1, 1, 1, 1]
        # ]
    },
    {
        "node": "page_menu_or",
        "prior_type": "dirichlet",
        "pseudo_counts": [
            [1, 1, 1, 1, 1, 1, 1, 1, 2],  # 0 nessuno
            [4, 2, 2, 2, 4, 4, 4, 4, 2],  # 1 solo orizzontale
            [1, 1, 1, 1, 1, 1, 1, 1, 2],  # 2 solo verticale
            [1, 4, 4, 4, 1, 1, 1, 1, 2],  # 3 entrambi
        ]
    },
    {
        "node": "page_ungrouped_multim",
        "prior_type": "dirichlet",
        "pseudo_counts": [
            [3, 2, 4, 3, 4, 2, 4, 1, 2],  # 1 (0-5)
            [2, 4, 3, 3, 2, 3, 2, 3, 2],  # 2 (6-10)
            [1, 3, 1, 2, 1, 3, 1, 4, 2],  # 3 (11-20)
            [1, 1, 1, 1, 1, 1, 1, 4, 2],  # 4 (21+)
        ]
    },
    {
        "node": "page_height",
        "prior_type": "dirichlet",
        "pseudo_counts": [
            # 1 blocco = page_template, page_ungrouped_multim
            [1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   2, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   1, 1, 1, 1,   2, 2, 2, 2],  # 1 (0-2000)
            [2, 2, 2, 2,   2, 2, 2, 2,   2, 2, 2, 2,   3, 3, 3, 2,   1, 1, 1, 1,   2, 2, 3, 3,   2, 2, 2, 2,   2, 3, 4, 4,   2, 2, 2, 2],  # 2 (2001-4000)
            [3, 4, 4, 4,   2, 3, 4, 4,   2, 3, 4, 4,   2, 3, 3, 4,   3, 3, 3, 3,   2, 2, 3, 3,   3, 3, 4, 4,   4, 4, 4, 4,   2, 2, 2, 2],  # 3 (4001-6000)
            [2, 2, 3, 4,   2, 3, 3, 4,   2, 3, 3, 4,   2, 2, 3, 4,   2, 2, 2, 3,   1, 2, 2, 3,   3, 3, 3, 4,   4, 4, 4, 4,   2, 2, 2, 2],  # 4 (6001+)
        ]
    },
    {
        "node": "metric",
        "prior_type": "dirichlet",
        "pseudo_counts": [
            # 1 blocco = page_height, page_menu_or, page_ungrouped_multim
            # 101   #104   #111          #121          #131            #201                                                         #301                                                          #401
            [2, 2, 3, 4,   1, 1, 3, 3,   1, 1, 3, 3,   1, 1, 2, 3,     2, 3, 4, 4,   1, 2, 3, 3,   1, 3, 3, 3,   1, 2, 2, 3,        3, 3, 4, 4,   1, 2, 3, 4,   1, 3, 3, 3,   1, 2, 2, 3,         3, 3, 4, 4,   1, 2, 3, 4,   1, 3, 3, 3,   1, 2, 2, 3],  # 1
            [2, 4, 2, 4,   2, 3, 2, 4,   2, 3, 2, 4,   2, 3, 3, 3,     2, 4, 3, 4,   2, 3, 3, 4,   2, 3, 2, 4,   2, 3, 3, 3,        3, 4, 3, 4,   2, 3, 4, 4,   2, 3, 2, 4,   2, 3, 3, 3,         3, 4, 3, 4,   2, 3, 4, 4,   2, 3, 2, 4,   2, 3, 3, 3],  # 2
            [3, 2, 2, 2,   4, 3, 3, 3,   4, 3, 3, 3,   4, 3, 3, 3,     3, 2, 2, 2,   4, 3, 3, 3,   4, 3, 3, 2,   4, 2, 3, 2,        1, 2, 1, 1,   4, 3, 3, 3,   4, 3, 3, 2,   4, 2, 3, 2,         1, 2, 1, 1,   4, 3, 3, 3,   4, 3, 3, 2,   4, 2, 3, 2],  # 3
            [1, 1, 1, 1,   4, 3, 1, 1,   4, 3, 1, 1,   4, 3, 1, 1,     1, 1, 1, 1,   3, 3, 1, 1,   3, 2, 1, 1,   4, 3, 1, 1,        1, 1, 1, 1,   3, 3, 1, 1,   3, 2, 1, 1,   3, 3, 1, 1,         1, 1, 1, 1,   3, 3, 1, 1,   3, 2, 1, 1,   3, 3, 1, 1],  # 4
        ]
    },
    {
        "node": "NDOM_nodes",
        "prior_type": "dirichlet",
        "pseudo_counts": [
            [4, 2, 1, 1],  # 1
            [3, 3, 2, 1],  # 2
            [1, 2, 2, 2],  # 3
            [1, 2, 3, 4],  # 4
        ]
    },
    {
        "node": "NDOM_height",
        "prior_type": "dirichlet",
        "pseudo_counts": [
            [3, 2, 1, 1],  # 1
            [1, 2, 3, 2],  # 2
            [1, 1, 3, 3],  # 3
        ]
    },
]
# fmt:on


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


def create_bn_and_query(ds: DataFrame, estimator_type: str, bn_out_path, query_out_path):
    # crea bn
    print(f"Creating BN structure (estimator type = {estimator_type})...")
    bn = BayesianNetwork(ebunch=BN_EDGES_DEFAULT)

    print("Learning BN parameters...")
    if estimator_type == "MLE":
        # https://pgmpy.org/examples/Learning%20Parameters%20in%20Discrete%20Bayesian%20Networks.html
        # https://pgmpy.org/param_estimator/mle.html
        bn.fit(data=ds, estimator=MaximumLikelihoodEstimator, state_names=BN_STATE_NAMES)

    elif estimator_type == "MAP":
        # apprendimento con stimatore bayesiano
        # https://pgmpy.org/param_estimator/bayesian_est.html
        bayesian_estimator = BayesianEstimator(
            model=bn, data=ds, state_names=BN_STATE_NAMES
        )
        for var_args in BN_MAP_ESTIMATOR_PARAMS:
            cpt = bayesian_estimator.estimate_cpd(**var_args)
            bn.add_cpds(cpt)

    else:
        raise ValueError("Incorrect estimator type. Allowed: 'MLE', 'MAP'.")

    if bn.check_model():
        print("Saving...")
        bif_writer = BIFWriter(bn)
        bif_writer.write_bif(bn_out_path)

    # query
    print("Querying with VE infer engine...")
    infer_engine_ex = VariableElimination(bn)

    with open(query_out_path, "w") as query_out:
        i = 1
        for query_args in BN_QUERIES_DEFAULT:
            # mostra la descrizione e rimuovila dal dizionario degli argomenti della query
            query_desc = query_args.pop("query_desc", None)
            query_out.write(f"\nQuery #{i}: {query_desc}")

            query_obj = infer_engine_ex.query(**query_args)
            query_out.write("\n")
            query_out.write(str(query_obj))

            i = i + 1


if __name__ == "__main__":
    # leggi dataset
    print("Reading dataset...")
    ds = pd.read_csv(defs.ds3_gt_no_noise_path)
    bn_features_excluded = defs.ds3_features_excluded + [
        "page_load_time_ms",
        "page_width",
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
        mapping=defs.DS_DISCRETE_MAPPING_DEFAULT,
    )

    # crea bn e fai le query
    create_bn_and_query(
        ds=ds,
        estimator_type="MLE",
        bn_out_path=defs.bn_mle_path,
        query_out_path=defs.bn_mle_query_path,
    )

    create_bn_and_query(
        ds=ds,
        estimator_type="MAP",
        bn_out_path=defs.bn_map_path,
        query_out_path=defs.bn_map_query_path,
    )


"""
    values=[
            # 11     12   21     22   31    32     41   42    51     52   61    62    71    72    81    82    91     92
            [0.45, 0.70, 0.22, 0.65, 0.22, 0.65, 0.22, 0.65, 0.60, 0.85, 0.30, 0.50, 0.42, 0.78, 0.08, 0.08, 0.2, 0.3],
            [0.45, 0.20, 0.55, 0.25, 0.55, 0.25, 0.55, 0.25, 0.40, 0.15, 0.50, 0.48, 0.48, 0.21, 0.42, 0.50, 0.2, 0.3],
            [0.05, 0.07, 0.22, 0.05, 0.22, 0.05, 0.22, 0.05, 0.00, 0.00, 0.15, 0.01, 0.05, 0.01, 0.42, 0.40, 0.3, 0.2],
            [0.05, 0.03, 0.10, 0.05, 0.10, 0.05, 0.10, 0.05, 0.00, 0.00, 0.05, 0.01, 0.05, 0.00, 0.08, 0.02, 0.3, 0.2]
        ],
"""
