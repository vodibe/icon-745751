import sys
import agent.definitions as defs
import csv
import pandas as pd
from pandas import DataFrame
from pyswip import Prolog
import requests
from io import StringIO

# istanza singleton del bridge SWI prolog - python
prolog = Prolog()


def _pl_str(v) -> str:
    """Crea una stringa accettata dalla sintassi Prolog a partire da un qualsiasi input s.

    Args:
        - input_str: Stringa di input

    Returns:
        - str: Stringa Prolog
    """
    if not isinstance(v, str):
        return str(v)

    # escape degli apici
    prolog_string = v.replace("'", "''")
    # una stringa prolog è contornata dagli apici
    return f"'{prolog_string}'"


def create_page_facts_from_ds(ds_path):
    """Crea una lista di fatti, ciascuno rappresentante semanticamente una pagina del
    dataset ds_path.

    Args:
        - ds_path: Percorso dataset di partenza.
    """
    print("Creating page facts from dataset...")
    with open(defs.kb_shared_facts_path, "a") as pl_out:
        with open(ds_path, "r") as csv_in:
            csv_reader = csv.DictReader(csv_in, delimiter=",")

            for row in csv_reader:
                # crea fatto prolog che definisce la pagina
                page_fact = (
                    f"\npage("
                    f'schoolassoc({_pl_str(row["page_url"])}, {_pl_str(row["school_id"])}), '
                    f'details({row["page_width"]}, {row["page_height"]}, {row["page_load_time_ms"]}, {int(float(row["page_template"]))}, {int(float(row["page_menu_or"]))}, {int(float(row["page_ungrouped_multim"]))}), '
                    f'ndom({row["NDOM_nodes"]}, {row["NDOM_height"]}, {[float(row[f"task{i}"]) for i in range(1, len(list(defs.TASKS_DEFAULT.keys()))+1)]}), '
                    f'{row["metric"]}).'
                )
                pl_out.write(page_fact)
    print("Done.")


def query_kb_miur(query, endpoint) -> DataFrame:
    """Interroga l'endpoint del MIUR sottoponendo la query. Ottiene dall'endpoint dati
    in formato csv. Restituisce una tabella DataFrame.

    Args:
        - query: Query SPARQL.
        - endpoint (optional): Endpoint MIUR.

    Returns:
        - DataFrame: Risultati della query.
    """
    print("Querying remote endpoint...")
    query_payload = dict()
    query_payload["query"] = query  # urllib.parse.quote_plus(query)
    query_payload["dataType"] = "csv"

    query_response = requests.post(endpoint, data=query_payload)
    query_response.raise_for_status()

    query_df = pd.read_csv(StringIO(query_response.text), sep=",")

    return query_df


def run_job1():
    """Procedura Job1. Vedi: report.pdf."""

    print("Running Job#1...")

    # individuo le scuole che hanno siti con redirect errati
    prolog.consult(f"./{defs.kb_shared_facts_path.name}")
    prolog.consult(f"./{defs.kb_shared_rules_path.name}")
    query_kb = "page_wrongly_redirects(schoolassoc(Url, School_ID))."

    for result in prolog.query(query_kb):
        # per ciascuna scuola, ottengo dall'endpoint l'istituto di cui fa parte e l'elenco delle
        # scuole (su ogni riga) associate a quell'istituto.
        # nota: ricordare che per scuola si intende liceo artistico, istituto tecnico ecc...
        query_sp = """
            PREFIX miur: <http://www.miur.it/ns/miur#>
            Select ?CodiceIstitutoRiferimento ?DenominazioneIstitutoRiferimento ?CodiceScuola {
                graph ?g {
                    ?S miur:CODICEISTITUTORIFERIMENTO ?CodiceIstitutoRiferimento.
                    ?S miur:DENOMINAZIONEISTITUTORIFERIMENTO ?DenominazioneIstitutoRiferimento.
                    ?S miur:CODICESCUOLA "@".
                    ?C miur:CODICEISTITUTORIFERIMENTO ?CodiceIstitutoRiferimento.
                    ?C miur:CODICESCUOLA ?CodiceScuola.
                }
            }
            LIMIT 50
        """
        query_sp = query_sp.replace("@", result["School_ID"])
        institute_is_related_facts_df = query_kb_miur(query_sp, defs.KB_MIUR_ENDPOINT1)

        # creo i fatti prolog a partire dal dataframe
        with open(defs.kb_shared_facts_path, "a") as pl_out:
            for idx, row in institute_is_related_facts_df.iterrows():
                institute_is_related_fact = (
                    f"\ninstitute_is_related("
                    f'{_pl_str(row["CodiceIstitutoRiferimento"])}, '
                    f'{_pl_str(row["DenominazioneIstitutoRiferimento"])}, '
                    f'{_pl_str(row["CodiceScuola"])}).'
                )
                pl_out.write(institute_is_related_fact)

    # individua gli istituti a cui fanno capo le scuole con redirect errato.
    prolog.consult(f"./{defs.kb_shared_facts_path.name}")
    query_kb = "institute_is_related_for_job1(institute(Institute_ID, Institute_Name, Institute_Schools_IDs), schoolassoc(Url, School_ID))."

    # scrivi i risultati (fatti) inerenti al job
    with open(defs.kb_job1_out_path, "w") as job_out:
        for result in prolog.query(query_kb):
            job_fact_p2 = ""

            i = 1
            for school_ID in result["Institute_Schools_IDs"]:
                query_sp = """
                    PREFIX miur: <http://www.miur.it/ns/miur#>
                    Select ?CodiceScuola ?DenominazioneScuola ?IndirizzoScuola ?DescrizioneComune ?CapScuola {
                        graph ?g {
                            ?S miur:CODICESCUOLA "@".
                            ?S miur:CODICESCUOLA ?CodiceScuola.
                            ?S miur:DENOMINAZIONESCUOLA ?DenominazioneScuola.
                            ?S miur:INDIRIZZOSCUOLA ?IndirizzoScuola.
                            ?S miur:DESCRIZIONECOMUNE ?DescrizioneComune.
                            ?S miur:CAPSCUOLA ?CapScuola.
                        }
                    }
                    LIMIT 1
                """
                query_sp = query_sp.replace("@", school_ID)
                job_fact_p2_df = query_kb_miur(query_sp, defs.KB_MIUR_ENDPOINT1)

                job_fact_p2_ = (
                    "schooladdress("
                    + ", ".join([f"{_pl_str(v)}" for v in job_fact_p2_df.iloc[0].values])
                    + ")"
                )
                job_fact_p2 = job_fact_p2 + job_fact_p2_

                if i < len(result["Institute_Schools_IDs"]):
                    job_fact_p2 = job_fact_p2 + ", "

                i += 1

            job_fact_p1 = (
                f"\nis_valid_report_for_job1("
                f'schoolassoc({_pl_str(result["Url"])}, {_pl_str(result["School_ID"])}), '
                f'institute({_pl_str(result["Institute_ID"])}, {_pl_str(result["Institute_Name"])}, {(result["Institute_Schools_IDs"])}), '
                f"[{job_fact_p2}])."
            )

            job_out.write(job_fact_p1)


if __name__ == "__main__":
    create_page_facts_from_ds(defs.ds3_gt_no_noise_path)

    run_job1()
