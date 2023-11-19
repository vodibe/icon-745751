import agent.definitions as defs
import numpy as np
import pandas as pd
from pandas import DataFrame
from pgmpy.models import BayesianNetwork


_DISCRETE_MAPPING_DEFAULT = {
    "page_ungrouped_multim": ([0, 6, 11, 21, np.inf], [1, 2, 3, 4]),
    "metric": ([1, 2, 3, 4, np.inf], [1, 2, 3, 4]),
    "page_load_time_ms": ([0, 1501, 3001, np.inf], [1, 2, 3]),
    "page_width": ([0, 1800, np.inf], [1, 2]),
    "page_height": ([0, 2001, 4001, 6001, np.inf], [1, 2, 3]),
    "NDOM_nodes": (),
    "NDOM_height": (),
    "page_width": (),
    "page_width": (),
    "page_width": (),
}


def discretize_dataset(ds: DataFrame, mapping: dict) -> DataFrame:
    """_summary_

    Args:
        ds (DataFrame): _description_
        mapping (dict): _description_

    Returns:
        DataFrame: _description_
    """

    for feature, feature_mapping in mapping.items():
        # tresholds lista di n elementi, labels lista di n-1
        bins, labels = feature_mapping

        ds[feature] = pd.cut(
            x=ds[feature], bins=bins, labels=labels, include_lowest=True, right=False
        )


def create_bayesian_network():
    ds_discrete = pd.read_csv(defs.ds3_gt_no_noise_path)

    pass


if __name__ == "__main__":
    pass
