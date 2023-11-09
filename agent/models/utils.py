import agent.definitions as defs
import pandas as pd
from pandas import DataFrame
import numpy
import sys


def is_in_feature_domain(domain, value) -> bool:
    """Dato il dominio di una feature, restituisce True se value compare nel dominio,
    False altrimenti.

    Args:
        - domain: Dominio di una feature. Può essere un tipo (es. str), una lista o una funzione lambda.
        - value: Valore da controllare.
    """

    if domain is str:
        return isinstance(value, str)

    elif isinstance(domain, list):
        return value in domain

    elif callable(domain) and isinstance(domain, type(lambda x: x)):
        return domain(value)

    else:
        return False


def detect_noisy_rows_values(df: DataFrame, domains: dict):
    """Rileva quali sono le righe del DS df in cui almeno una feature non è corretta,
    cioè non rientra nel suo dominio.

    Args:
        - df: Istanza Dataframe.
        - ds_feature_domains: Dizionario feature:funzione di validazione.

    Returns:
        - noisy_rows: Righe in cui esiste almeno una colonna con valori errati.
        - noisy_cols: Colonne i cui valori in corrispondenza sono errati.
    """

    print("Detecting noisy rows...")
    noisy_rows = set()
    noisy_cols = set()

    for column in list(domains.keys()):
        serie = df[column]
        for i, el in serie.items():
            if not is_in_feature_domain(domains[column], el):
                noisy_rows.add(i)
                noisy_cols.add(df.columns.get_loc(column))

    if noisy_rows:
        print(f"{len(noisy_rows)} noisy rows exist! Please check them.")

    return list(noisy_rows), list(noisy_cols)


def load_ds(ds_path):
    """Crea un'istanza DataFrame a partire dal percorso del DS ds_path.

    Args:
        - ds_path: Percorso del DS.
        - ignore_noisy_rows: Se posto a True, restituisce un'istanza DataFrame anche in
        presenza di righe con valori errati. Default: False.
    """

    print(f"Creating DataFrame istance for {ds_path}...")
    df = pd.read_csv(ds_path)

    # rileva le righe rumorose. se esistono, restituisci None
    noisy_rows, noisy_cols = detect_noisy_rows_values(df, defs.ds3_gt_feature_domains)

    print(df.columns)

    if noisy_rows:
        print(df.iloc[noisy_rows, noisy_cols])
        return None

    return df

    """
    print("Converting some features to Int64...")
    ds3_features_Int64 = ds3_features_askable + ds3_features_part
    ds3_features_Int64.remove("metric")

    for feature in ds3_features_Int64:
        df[feature] = df[feature].astype("Int64")

    """
    print(df.dtypes)

    return df


if __name__ == "__main__":
    load_ds(defs.test_path)

    # stampa le righe con rumore
    pass
