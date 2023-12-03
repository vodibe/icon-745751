import agent.definitions as defs
import requests
from selenium import webdriver
import pandas as pd
from pandas import DataFrame
from io import StringIO


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


def _query_wikidata_sparql(query, endpoint=defs.KB_WIKIDATA_ENDPOINT) -> DataFrame:
    """Interroga l'endpoint SPARQL di Wikidata e ottiene il risultato in forma DataFrame.

    Args:
        - query: Query SPARQL.
        - endpoint (optional): Endpoint Wikidata. Default: defs.KB_WIKIDATA_ENDPOINT.

    Returns:
        - DataFrame.
    """

    params = {"query": query, "format": "json"}

    headers = {"User-Agent": "MyWikidataQuery/1.0 (your@email.com)"}

    response = requests.post(endpoint, headers=headers, data=params)
    response.raise_for_status()

    data = response.json()
    bindings = data.get("results", {}).get("bindings", [])

    df = DataFrame(bindings)
    df = df.apply(lambda x: x.str.get("value") if x.name == "value" else x)
    df = df.map(lambda x: x.get("value") if isinstance(x, dict) else x)

    return df


def _query_miur_sparql(query, endpoint) -> DataFrame:
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
