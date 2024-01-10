import sys
import agent.definitions as defs
import numpy as np
import pandas as pd
from pandas import DataFrame

from agent.preproc.utils import discretize_dataset

from pgmpy.models import BayesianNetwork
from pgmpy.estimators import MaximumLikelihoodEstimator, BayesianEstimator
from pgmpy.metrics import log_likelihood_score
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
        "query_desc": "P(page_template | NDOM_nodes=1, NDOM_height=1, metric=4)",
        "variables": ["page_template"],
        "evidence": {"NDOM_nodes": 1, "NDOM_height": 1, "metric": 4},
    },
    {
        "query_desc": "P(page_template | NDOM_nodes=4)",
        "variables": ["page_template"],
        "evidence": {
            "NDOM_nodes": 4,
        },
    },
    {
        "query_desc": "P(page_template | NDOM_height=2, metric=2)",
        "variables": ["page_template"],
        "evidence": {"NDOM_height": 2, "metric": 2},
    },
    # -----
    {
        "query_desc": "P(metric | page_template=1)",
        "variables": ["metric"],
        "evidence": {
            "page_template": 1,
        },
    },
    {
        "query_desc": "P(metric | page_template=2)",
        "variables": ["metric"],
        "evidence": {
            "page_template": 2,
        },
    },
    {
        "query_desc": "P(metric | page_template=3)",
        "variables": ["metric"],
        "evidence": {
            "page_template": 3,
        },
    },
    {
        "query_desc": "P(metric | page_template=4)",
        "variables": ["metric"],
        "evidence": {
            "page_template": 4,
        },
    },
    {
        "query_desc": "P(metric | page_template=5)",
        "variables": ["metric"],
        "evidence": {
            "page_template": 5,
        },
    },
    {
        "query_desc": "P(metric | page_template=6)",
        "variables": ["metric"],
        "evidence": {
            "page_template": 6,
        },
    },
    {
        "query_desc": "P(metric | page_template=7)",
        "variables": ["metric"],
        "evidence": {
            "page_template": 7,
        },
    },
    {
        "query_desc": "P(metric | page_template=8)",
        "variables": ["metric"],
        "evidence": {
            "page_template": 8,
        },
    },
    # -----
    {
        "query_desc": "P(NDOM_nodes, NDOM_height | page_template=1)",
        "variables": ["NDOM_nodes", "NDOM_height"],
        "evidence": {
            "page_template": 1,
        },
    },
    # -----
    {
        "query_desc": "P(page_template | page_ungrouped_multim=3)",
        "variables": ["page_template"],
        "evidence": {
            "page_ungrouped_multim": 3,
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
BN_MAP_PRIORS = [
    {
        "node": "page_template",
        "prior_type": "dirichlet",
        "pseudo_counts": [
           [2], # template 1
           [2], # 2
           [2], # 3
           [2], # 4
           [2], # 5
           [2], # 6
           [2], # 7
           [2], # 8
        ]
        
    },
    {
        "node": "page_menu_or",
        "prior_type": "dirichlet",
        "pseudo_counts": [
            [ 1,  1,  1,  1,  1,  1,  1,  1],  # 0:nessuno
            [11,  3,  3,  3, 11, 11, 11, 11],  # 1:solo orizzontale
            [ 2,  5,  5,  5,  2,  2,  2,  2],  # 2:solo verticale
            [ 2, 11, 11, 11,  2,  2,  2,  2],  # 3:entrambi
        ]
    },
    {
        "node": "page_ungrouped_multim",
        "prior_type": "dirichlet",
        "pseudo_counts": [
            [8,  5,  8,  11, 16,  7, 11,  3],  # 1:(0-5)
            [7, 11, 11,  11,  5,  8,  5,  5],  # 2:(6-10)
            [2,  8,  2,  4,  1,   5,  2, 16],  # 3:(11-20)
            [1,  2,  5,  2,  1,   2,  1, 9],   # 4:(21+)
        ]
    },
    {
        "node": "page_height",
        "prior_type": "dirichlet",
        "pseudo_counts": [
            # 2 blocco = page_template, page_ungrouped_multim
            [5, 2, 2, 2,     2, 2, 2, 2,     2, 2, 2, 2,     8, 2, 2, 2,    5, 2, 2, 2,   2, 2, 2, 2,   2, 2, 2, 2,     2, 2, 2, 2],      # 1:(0-2600)
            [5, 5, 5, 5,     5, 5, 5, 5,     5, 5, 5, 5,     5, 8, 8, 5,    5, 2, 2, 2,   5, 5, 8, 8,   5, 5, 5, 5,     5, 8, 11, 11],    # 2:(2601-5200)
            [8, 11, 8, 11,   5, 8, 11, 11,   5, 8, 11, 11,   5, 8, 8, 11,   8, 8, 8, 8,   5, 5, 8, 8,   8, 8, 11, 11,   11, 11, 11, 11],  # 3:(5201-7200)
            [5, 5, 8, 11,    5, 8, 8, 11,    5, 8, 8, 11,    5, 5, 8, 11,   2, 5, 5, 8,   2, 5, 5, 8,   8, 8, 8, 11,    11, 11, 11, 11],  # 4:(7201+)
        ]
    },
    {
        "node": "metric",
        "prior_type": "dirichlet",
        "pseudo_counts": [
            # blocco = page_height, page_menu_or, page_ungrouped_multim
            # 101   #104   #111          #121              #131              #201                                                                     #301                                                              #401
            [2, 2, 2, 8,    1, 1, 1, 4,     1, 1, 1, 4,     1, 1, 1, 4,        3, 3, 3, 8,    1, 1, 1, 4,     1, 1, 1, 4,     1, 1, 1, 4,          2, 2, 11, 11,   1, 1, 4, 4,     1, 1, 4, 4,     1, 1, 4, 4,      2, 5, 11, 11,  1, 3, 4, 5,     1, 3, 4, 5,       1, 3, 4, 5],  # 1:[1-2)                                                                                                                                                                                                                                                                                                 
            [7, 7, 10, 8,   1, 2, 5, 11,    1, 2, 6, 11,    1, 2, 6, 11,       7, 7, 10, 8,   1, 2, 6, 11,    1, 2, 6, 11,    1, 2, 6, 11,         6, 9, 10, 10,    2, 2, 5, 10,   2, 2, 5, 10,    2, 2, 5, 10,     6, 9, 10, 6,   2, 8, 11, 11,   2, 8, 11, 11,     2, 8, 11, 11],  # 2:[2-3)                                                                                                                                                                                                                                                                                                                   
            [7, 7, 8, 4,    6, 10, 11, 4,   6, 10, 11, 4,   6, 10, 11, 4,      7, 6, 8, 4,    7, 10, 11, 4,   6, 10, 11, 4,   6, 10, 11, 4,        10, 7, 6, 5,    8, 9, 12, 9,     8, 9, 12, 9,   8, 9, 12, 9,     10, 7, 6, 3,   11, 13, 6, 5,   11, 13, 6, 5,     11, 13, 6, 5],  # 3:[3-4)                                                                                                                                                                                                                                                                                                 
            [2, 2, 1, 1,    11, 7, 2, 1,    11, 7, 2, 1,    11, 7, 2, 1,       2, 1, 1, 1,    10, 7, 2, 1,    11, 7, 2, 1,    11, 7, 2, 1,         3, 2, 1, 1,     10, 5, 1, 1,    10, 5, 1, 1,    10, 5, 1, 1,     3, 1, 1, 1,    11, 6, 1, 1,    11, 6, 1, 1,      11, 6, 1, 1],  # 4:[4-5]
   
        ]
    },
    {
        "node": "NDOM_nodes",
        "prior_type": "dirichlet",
        "pseudo_counts": [
            [13, 8, 2, 2],  # 1:(0-500)
            [5, 11, 8, 5],  # 2:(501-1000)
            [2, 5, 11, 8],  # 3:(1001-1500)
            [1, 2, 5, 11],  # 4:(1501+)
        ]
    },
    {
        "node": "NDOM_height",
        "prior_type": "dirichlet",
        "pseudo_counts": [
            [9, 5, 6, 4],  # 1:(0-4)
            [3, 9, 10, 10],  # 2:(5-13)
            [1, 2, 3, 6],  # 3:(14+)
        ]
    },
]
# fmt:on


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
        for var_args in BN_MAP_PRIORS:
            cpt = bayesian_estimator.estimate_cpd(**var_args)
            bn.add_cpds(cpt)

    else:
        raise ValueError("Incorrect estimator type. Allowed: 'MLE', 'MAP'.")

    if bn.check_model():
        print("Saving...")
        bif_writer = BIFWriter(bn)
        bif_writer.write_bif(bn_out_path)

    simulated_data = bn.simulate(int(1e4))
    score_ll = log_likelihood_score(bn, simulated_data)
    with open(defs.bn_ll_path, "a") as bn_ll:
        bn_ll.write(f"\nBN_{estimator_type}:\n\tLog likelihood: {score_ll}\n")

    # query
    print("Querying...")
    infer_engine_ex = VariableElimination(bn)

    with open(query_out_path, "w") as query_out:
        i = 1
        for query_args in BN_QUERIES_DEFAULT:
            # mostra la descrizione e rimuovila dal dizionario degli argomenti della query
            query_desc = query_args.pop("query_desc", None)
            query_out.write(f"\n\nQuery #{i}: {query_desc}")

            query_obj = infer_engine_ex.query(**query_args)
            query_out.write("\n")
            query_out.write(str(query_obj))

            # riaggiungo query_desc
            query_args["query_desc"] = query_desc

            i = i + 1


if __name__ == "__main__":
    # leggi dataset
    print("Reading dataset...")
    ds = pd.read_csv(defs.ds3_gt_no_noise_path)

    ds = ds.drop(defs.bn_features_excluded, axis=1)
    ds = ds[ds["page_template"] != defs.PAGE_TEMPLATE_MAX_VALUE]

    discretize_dataset(
        ds=ds,
        feature_domains=defs.ds3_gt_feature_domains,
        mapping=defs.DS_DISCRETE_MAPPING_DEFAULT,
    )

    # ds.to_csv("./test.csv", sep=",", index=False)

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
