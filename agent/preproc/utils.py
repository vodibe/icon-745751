import agent.definitions as defs
import requests
from selenium import webdriver
import pandas as pd
from pandas import DataFrame
from io import StringIO
import numpy as np


def _create_driver(browser_width, browser_height) -> webdriver:
    """Restituisce un'istanza della classe webdriver, cioè un browser avente una scheda
    aperta al sito location.
    Vedi: https://stackoverflow.com/a/55878622

    Args:
        - browser_width: lunghezza finestra browser.
        - browser_height: altezza finestra browser.

    Returns:
        - webdriver: istanza webdriver
    """

    options = webdriver.FirefoxOptions()
    # options.add_argument("--headless")
    options.add_argument(f'--user-agent={defs.headers["User-Agent"]}')
    options.add_argument(f"--width={browser_width}")
    options.add_argument(f"--height={browser_height}")

    driver = webdriver.Firefox(options=options)

    return driver


def _pl_str(v) -> str:
    """Crea una stringa accettata dalla sintassi Prolog a partire da un qualsiasi input.

    Args:
        - input_str: Stringa di input

    Returns:
        - str: Stringa Prolog
    """
    if not isinstance(v, str):
        return _pl_str(str(v))
    else:
        # escape degli apici
        prolog_string = v.replace("'", "''")

    # una stringa prolog è contornata dagli apici
    return f"'{prolog_string}'"


def _query_wikidata_kb(query, endpoint=defs.KB_WIKIDATA_ENDPOINT) -> DataFrame:
    """Interroga l'endpoint SPARQL di Wikidata e ottiene il risultato in forma DataFrame.

    Args:
        - query: Query SPARQL.
        - endpoint (optional): Endpoint Wikidata. Default: defs.KB_WIKIDATA_ENDPOINT.

    Returns:
        - DataFrame.
    """

    params = {"query": query, "format": "json"}

    response = requests.post(endpoint, headers=defs.headers, data=params)
    response.raise_for_status()

    data = response.json()
    bindings = data.get("results", {}).get("bindings", [])

    query_df = DataFrame(bindings)
    query_df = query_df.apply(lambda x: x.str.get("value") if x.name == "value" else x)
    query_df = query_df.map(lambda x: x.get("value") if isinstance(x, dict) else x)

    return query_df


def _query_miur_kb(query, endpoint) -> DataFrame:
    """Interroga l'endpoint del MIUR sottoponendo la query. Ottiene dall'endpoint dati
    in formato csv. Restituisce una tabella DataFrame.

    Args:
        - query: Query SPARQL.
        - endpoint (optional): Endpoint MIUR.

    Returns:
        - DataFrame: Risultati della query.
    """
    # print("Querying remote endpoint...")
    params = {"query": query, "dataType": "csv"}  # urllib.parse.quote_plus(query)

    response = requests.post(endpoint, data=params)
    response.raise_for_status()

    query_df = pd.read_csv(StringIO(response.text), sep=",")

    return query_df


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
