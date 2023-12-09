import agent.definitions as defs
from agent.preproc.utils import _pl_str, _query_miur_kb
from agent.kb.geofacts import create_geofacts3_from_ds, create_geofacts4_from_ds
import csv
from pyswip import Prolog
import sys

# istanza singleton del bridge SWI prolog - python
prolog = Prolog()


def create_page_facts_from_ds(ds_path):
    """Crea una lista di fatti, ciascuno rappresentante semanticamente una pagina del
    dataset ds_path.

    Args:
        - ds_path: Percorso dataset di partenza.
    """
    print("Creating page facts from dataset...")
    with open(defs.kb_shared_facts_path, "w") as pl_out:
        with open(ds_path, "r") as csv_in:
            csv_reader = csv.DictReader(csv_in, delimiter=",")

            for row in csv_reader:
                # page(schoolassoc('https://liceo.edu.it/', 'XXXX03000B'), details(1587, 3999, 4551, 5, 1, 3), ndom(306, 7, [3.1, 2.79, 3.1, 3.29, 6.5, 10.01, 7.0, 2.79]), 4.3).
                page_fact = (
                    f"\npage("
                    f'schoolassoc({_pl_str(row["page_url"])}, {_pl_str(row["school_id"])}), '
                    f'details({row["page_width"]}, {row["page_height"]}, {row["page_load_time_ms"]}, {int(float(row["page_template"]))}, {int(float(row["page_menu_or"]))}, {int(float(row["page_ungrouped_multim"]))}), '
                    f'ndom({row["NDOM_nodes"]}, {row["NDOM_height"]}, {[float(row[f"task{i}"]) for i in range(1, len(list(defs.TASKS_DEFAULT.keys()))+1)]}), '
                    f'{row["metric"]}).'
                )
                pl_out.write(page_fact)
    print("Done.")


def run_job1(private_clauses_path=defs.job1_clauses_path, out_path=defs.job1_output_path):
    """Procedura Job1. Vedi: report.pdf."""

    print("Running Job 1...")

    # step:
    # 1) result = singola scuola con redirect errato
    # 2) scuola -> istituto a cui fa capo
    # 3) istituto a cui fa capo -> tutte le scuole associate all'istituto (compresa quella di partenza)

    # -----
    print("Creating necessary facts...")
    # 1)
    query_kb = "page_wrongly_redirects(schoolassoc(Url, School_ID))."

    for result in prolog.query(query_kb):
        # 2)
        # fmt:off
        query_rem = (
            """
            PREFIX miur: <http://www.miur.it/ns/miur#>
            Select ?CodiceIstitutoRiferimento ?DenominazioneIstitutoRiferimento ?CodiceScuola {
                graph ?g {
                    ?S miur:CODICESCUOLA """ + _pl_str(result["School_ID"]) + """.
                    ?S miur:CODICEISTITUTORIFERIMENTO ?CodiceIstitutoRiferimento.
                    ?S miur:DENOMINAZIONEISTITUTORIFERIMENTO ?DenominazioneIstitutoRiferimento.
                    ?C miur:CODICEISTITUTORIFERIMENTO ?CodiceIstitutoRiferimento.
                    ?C miur:CODICESCUOLA ?CodiceScuola.
                }
            }
            LIMIT 50
            """
        )  # fmt:on
        query_df = _query_miur_kb(query_rem, defs.KB_MIUR_ENDPOINT1)

        # institute_has_school(institute('istituto', 'nomeistituto'), 'scuola').
        with open(private_clauses_path, "a") as pl_out:
            for idx, row in query_df.iterrows():
                f = (
                    f"\ninstitute_has_school("
                    f'institute({_pl_str(row["CodiceIstitutoRiferimento"])}, '
                    f'{_pl_str(row["DenominazioneIstitutoRiferimento"])}), '
                    f'{_pl_str(row["CodiceScuola"])}).'
                )
                pl_out.write(f)

    # -----
    print("Running query on local KB...")
    # individua gli istituti a cui fanno capo le scuole con redirect errato.
    prolog.consult(f"./jobs/{private_clauses_path.name}")
    query_kb = "is_partial_report1(schoolassoc(Url, School_ID), institute_with_all_schools(institute(Institute_ID, Institute_Name), Institute_Schools_IDs))."

    with open(out_path, "w") as pl_out:
        for result in prolog.query(query_kb):
            # per ogni partial report, costruisco un full report.
            # la costruzione di un full report non richiede ulteriori query sulla kb locale,
            # ma solamente l'interrogazione della kb remota.

            list_school_contacts = ""

            i = 1
            for school_ID in result["Institute_Schools_IDs"]:
                # recupero contatti scuola
                # fmt:off
                query_rem = (
                    """
                    PREFIX miur: <http://www.miur.it/ns/miur#>
                    Select ?CodiceScuola ?DenominazioneScuola ?IndirizzoScuola ?DescrizioneComune ?CapScuola ?IndirizzoEmailScuola {
                        graph ?g {
                            ?S miur:CODICESCUOLA """ + _pl_str(school_ID) + """.
                            ?S miur:CODICESCUOLA ?CodiceScuola.
                            ?S miur:DENOMINAZIONESCUOLA ?DenominazioneScuola.
                            ?S miur:INDIRIZZOSCUOLA ?IndirizzoScuola.
                            ?S miur:DESCRIZIONECOMUNE ?DescrizioneComune.
                            ?S miur:CAPSCUOLA ?CapScuola.
                            ?S miur:INDIRIZZOEMAILSCUOLA ?IndirizzoEmailScuola.
                        }
                    }
                    LIMIT 1
                    """
                )  # fmt:on
                query_df = _query_miur_kb(query_rem, defs.KB_MIUR_ENDPOINT1)

                # creazione ultima parte (lista di simboli di funzione schooladdress)
                school_contact = (
                    "schoolcontact("
                    + ", ".join([f"{_pl_str(v)}" for v in query_df.iloc[0].values])
                    + ")"
                )
                list_school_contacts = list_school_contacts + school_contact
                if i < len(result["Institute_Schools_IDs"]):
                    list_school_contacts = list_school_contacts + ", "
                i += 1

            full_report = (
                f"\nis_full_report1("
                f'schoolassoc({_pl_str(result["Url"])}, {_pl_str(result["School_ID"])}), '
                f'institute_with_all_schools(institute({_pl_str(result["Institute_ID"])}, {_pl_str(result["Institute_Name"])}), {(result["Institute_Schools_IDs"])}), '
                f"[{list_school_contacts}])."
            )

            pl_out.write(full_report)

    print(f"Done. Output generated at {out_path}.")


def run_job2(private_clauses_path=defs.job2_clauses_path, out_path=defs.job2_output_path):
    """Procedura Job2. Vedi: report.pdf."""

    print("Running Job 2...")

    # -----
    print("Creating necessary facts...")
    # individuo le scuole che hanno siti da migliorare
    query_kb = "page_needs_improvement(schoolassoc(Url, School_ID))."

    for result in prolog.query(query_kb):
        # fmt:off
        query_rem = (
            """
            PREFIX miur: <http://www.miur.it/ns/miur#>
            Select ?CodiceIstitutoRiferimento ?DenominazioneIstitutoRiferimento ?CodiceScuola {
                graph ?g {
                    ?S miur:CODICEISTITUTORIFERIMENTO ?CodiceIstitutoRiferimento.
                    ?S miur:DENOMINAZIONEISTITUTORIFERIMENTO ?DenominazioneIstitutoRiferimento.
                    ?S miur:CODICESCUOLA """ + _pl_str(result["School_ID"]) + """.
                    ?C miur:CODICEISTITUTORIFERIMENTO ?CodiceIstitutoRiferimento.
                    ?C miur:CODICESCUOLA ?CodiceScuola.
                }
            }
            LIMIT 50
            """
        )  # fmt:on
        query_df = _query_miur_kb(query_rem, defs.KB_MIUR_ENDPOINT1)

        # institute_has_school(institute('istituto', 'nomeistituto'), 'scuola').
        with open(private_clauses_path, "a") as pl_out:
            for idx, row in query_df.iterrows():
                f = (
                    f"\ninstitute_has_school("
                    f'institute({_pl_str(row["CodiceIstitutoRiferimento"])}, '
                    f'{_pl_str(row["DenominazioneIstitutoRiferimento"])}), '
                    f'{_pl_str(row["CodiceScuola"])}).'
                )
                pl_out.write(f)

    # -----
    print("Running query on local KB...")
    # individua gli istituti a cui fanno capo le scuole con sito da migliorare.
    prolog.consult(f"./jobs/{private_clauses_path.name}")
    query_kb = "is_partial_report2(institute_with_all_schools(institute(Institute_ID, Institute_Name), Institute_Schools_IDs), schoolassoc(Url, School_ID))."

    with open(out_path, "w") as pl_out:
        for result in prolog.query(query_kb):
            list_school_contacts = ""

            i = 1
            for school_ID in result["Institute_Schools_IDs"]:
                # fmt:off
                query_rem = (
                    """
                    PREFIX miur: <http://www.miur.it/ns/miur#>
                    Select ?CodiceScuola ?DenominazioneScuola ?IndirizzoScuola ?DescrizioneComune ?CapScuola ?IndirizzoEmailScuola {
                        graph ?g {
                            ?S miur:CODICESCUOLA """ + _pl_str(school_ID) + """.
                            ?S miur:CODICESCUOLA ?CodiceScuola.
                            ?S miur:DENOMINAZIONESCUOLA ?DenominazioneScuola.
                            ?S miur:INDIRIZZOSCUOLA ?IndirizzoScuola.
                            ?S miur:DESCRIZIONECOMUNE ?DescrizioneComune.
                            ?S miur:CAPSCUOLA ?CapScuola.
                            ?S miur:INDIRIZZOEMAILSCUOLA ?IndirizzoEmailScuola.
                        }
                    }
                    LIMIT 1
                    """
                )  # fmt:on
                query_df = _query_miur_kb(query_rem, defs.KB_MIUR_ENDPOINT1)

                # creazione ultima parte (lista di simboli di funzione schooladdress)
                school_contact = (
                    "schoolcontact("
                    + ", ".join([f"{_pl_str(v)}" for v in query_df.iloc[0].values])
                    + ")"
                )
                list_school_contacts = list_school_contacts + school_contact
                if i < len(result["Institute_Schools_IDs"]):
                    list_school_contacts = list_school_contacts + ", "
                i += 1

            full_report = (
                f"\nis_full_report2("
                f'schoolassoc({_pl_str(result["Url"])}, {_pl_str(result["School_ID"])}), '
                f'institute_with_all_schools(institute({_pl_str(result["Institute_ID"])}, {_pl_str(result["Institute_Name"])}), {(result["Institute_Schools_IDs"])}), '
                f"[{list_school_contacts}])."
            )

            pl_out.write(full_report)

    print(f"Done. Output generated at {out_path}.")


def run_job3(private_clauses_path=defs.job3_clauses_path, out_path=defs.job3_output_path):
    """Procedura Job3. Vedi: report.pdf."""

    print("Running Job 3...")

    # -----
    print("Creating necessary facts...")
    create_geofacts3_from_ds(defs.ds3_gt_no_noise_path, private_clauses_path)
    prolog.consult(f"./jobs/{private_clauses_path.name}")

    # -----
    print("Running query on local KB...")
    query_kb = "is_rank_of_places(Rank)."

    with open(out_path, "w") as pl_out:
        for result in prolog.query(query_kb):
            pl_out.write(str(result))

    print(f"Done. Facts generated at {out_path}.")


def run_job4(
    private_clauses_path=defs.job4_clauses_path,
    out_path=defs.job4_output_path,
    geofacts_created=False,
):
    """Procedura Job4. Vedi: report.pdf."""

    print("Running Job 4...")

    # -----
    if not geofacts_created:
        print("Creating necessary facts...")
        create_geofacts4_from_ds(defs.ds3_gt_no_noise_path, private_clauses_path)
    prolog.consult(f"./jobs/{private_clauses_path.name}")

    # -----
    print("Running query on local KB...")
    query_kb = "is_rank_of_places(Rank)."

    with open(out_path, "w") as pl_out:
        for result in prolog.query(query_kb):
            pl_out.write(str(result))

    print(f"Done. Facts generated at {out_path}.")


def run_job5(
    private_clauses_path=defs.job5_clauses_path,
    out_path=defs.job5_output_path,
    geofacts_created=False,
):
    """Procedura Job5. Vedi: report.pdf."""

    print("Running Job 5...")

    # -----
    if not geofacts_created:
        print("Creating necessary facts...")
        create_geofacts3_from_ds(defs.ds3_gt_no_noise_path, private_clauses_path)

    prolog.consult(f"./jobs/{private_clauses_path.name}")

    # -----
    print("Running query on local KB...")
    query_kb = "most_popular_templates_for_region(Template_Rank_For_Each_Region)"

    with open(out_path, "w") as pl_out:
        for result in prolog.query(query_kb):
            pl_out.write(repr(result["Template_Rank_For_Each_Region"]))

    print(f"Done. Facts generated at {out_path}.")


if __name__ == "__main__":
    # tutti i job consultano i fatti e regole condivise
    # create_page_facts_from_ds(defs.ds3_gt_no_noise_path)

    prolog.consult(f"./{defs.kb_shared_facts_path.name}")
    prolog.consult(f"./{defs.kb_shared_rules_path.name}")

    # job
    # run_job1()

    # run_job2()

    # run_job3()

    # run_job4(geofacts_created=True)

    # run_job5(geofacts_created=True)
