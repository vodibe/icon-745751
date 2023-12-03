import agent.definitions as defs
from agent.preproc.utils import _pl_str, _query_miur_sparql, _query_wikidata_sparql
import csv
from pyswip import Prolog

# istanza singleton del bridge SWI prolog - python
prolog = Prolog()


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


def create_geo_facts_from_ds(ds_path):
    """Crea una lista di fatti, ciascuno rappresentante semanticamente una pagina del
    dataset ds_path.

    Args:
        - ds_path: Percorso dataset di partenza.
    """
    print("Creating geo-facts from dataset...")
    with open(defs.kb_shared_facts_path, "a") as pl_out:
        with open(ds_path, "r") as csv_in:
            csv_reader = csv.DictReader(csv_in, delimiter=",")

            for row in csv_reader:
                #

                # fmt:off
                query_rem = (
                    """
                    PREFIX miur: <http://www.miur.it/ns/miur#>
                    Select ?CodiceCatastaleComune {
                        graph ?g {
                            ?S miur:CODICESCUOLA """ + _pl_str(row["school_id"]) + """.
                            ?S miur:CODICECOMUNESCUOLA ?CodiceCatastaleComune.
                        }
                    }
                    LIMIT 1
                    """
                )  # fmt:on
                query_df1 = _query_miur_sparql(query_rem, defs.KB_MIUR_ENDPOINT1)

                # fmt:off
                query_rem = (
                    """
                    SELECT ?ProvinciaCodice ?RegioneLabel ?RegionePopolazione
                    WHERE {
                        ?Citta wdt:P806 """ + _pl_str(query_df1.loc[0, "CodiceCatastaleComune"]) + """.
                        ?Citta wdt:P131 ?Provincia.
                        ?Provincia wdt:P131 ?Regione.
                        ?Provincia wdt:P395 ?ProvinciaCodice.
                        ?Regione wdt:P1082 ?RegionePopolazione.
                        
                        SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],it". }
                    }
                    LIMIT 1
                    """
                )  # fmt:on
                query_df2 = _query_wikidata_sparql(query_rem)

                geofact = (
                    f'\nschool_is_in_place({_pl_str(row["school_id"])}, '
                    f'city({_pl_str(query_df1.loc[0, "CodiceCatastaleComune"])}), '
                    f'province({_pl_str(query_df2.loc[0, "ProvinciaCodice"])}), '
                    f'region({_pl_str(query_df2.loc[0, "RegioneLabel"])}, {_pl_str(query_df2.loc[0, "RegionePopolazione"])}) ).'
                )
                pl_out.write(geofact)
    print("Done.")


def run_job1():
    """Procedura Job1. Vedi: report.pdf."""

    print("Running Job 1...")

    # individuo le scuole che hanno siti con redirect errati
    query_kb = "page_wrongly_redirects(schoolassoc(Url, School_ID))."

    for result in prolog.query(query_kb):
        # step:
        # result = singola scuola con redirect errato
        # scuola -> istituto a cui fa capo
        # istituto a cui fa capo -> tutte le scuole associate all'istituto (compresa quella di partenza)

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
        query_df = _query_miur_sparql(query_rem, defs.KB_MIUR_ENDPOINT1)

        # institute_has_school(institute('istituto', 'nomeistituto'), 'scuola').
        with open(defs.kb_job1_facts_i_path, "a") as pl_out:
            for idx, row in query_df.iterrows():
                f = (
                    f"\ninstitute_has_school("
                    f'institute({_pl_str(row["CodiceIstitutoRiferimento"])}, '
                    f'{_pl_str(row["DenominazioneIstitutoRiferimento"])}), '
                    f'{_pl_str(row["CodiceScuola"])}).'
                )
                pl_out.write(f)

    # individua gli istituti a cui fanno capo le scuole con redirect errato.
    prolog.consult(f"./jobs/{defs.kb_job1_facts_i_path.name}")
    query_kb = "is_partial_report1(institute_with_all_schools(institute(Institute_ID, Institute_Name), Institute_Schools_IDs), schoolassoc(Url, School_ID))."

    with open(defs.kb_job1_facts_o_path, "w") as pl_out:
        for result in prolog.query(query_kb):
            # per ogni partial report, costruisco un full report.
            # la costruzione di un full report non richiede ulteriori query sulla kb locale,
            # ma solamente l'interrogazione della kb remota.

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
                query_df = _query_miur_sparql(query_rem, defs.KB_MIUR_ENDPOINT1)

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

    print(f"Done. Facts generated at {defs.kb_job1_facts_o_path}.")


def run_job2():
    """Procedura Job2. Vedi: report.pdf."""

    print("Running Job 2...")

    # individuo le scuole che hanno siti da migliorare
    query_kb = "page_needs_improvement(schoolassoc(Url, School_ID))."

    for result in prolog.query(query_kb):
        # step:
        # result = singola scuola con sito da migliorare
        # scuola -> istituto a cui fa capo
        # istituto a cui fa capo -> tutte le scuole associate all'istituto (compresa quella di partenza)

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
        query_df = _query_miur_sparql(query_rem, defs.KB_MIUR_ENDPOINT1)

        # institute_has_school(institute('istituto', 'nomeistituto'), 'scuola').
        with open(defs.kb_job2_facts_i_path, "a") as pl_out:
            for idx, row in query_df.iterrows():
                f = (
                    f"\ninstitute_has_school("
                    f'institute({_pl_str(row["CodiceIstitutoRiferimento"])}, '
                    f'{_pl_str(row["DenominazioneIstitutoRiferimento"])}), '
                    f'{_pl_str(row["CodiceScuola"])}).'
                )
                pl_out.write(f)

    # individua gli istituti a cui fanno capo le scuole con sito da migliorare.
    prolog.consult(f"./jobs/{defs.kb_job2_facts_i_path.name}")
    query_kb = "is_partial_report2(institute_with_all_schools(institute(Institute_ID, Institute_Name), Institute_Schools_IDs), schoolassoc(Url, School_ID))."

    with open(defs.kb_job2_facts_o_path, "w") as pl_out:
        for result in prolog.query(query_kb):
            # per ogni partial report, costruisco un full report.
            # la costruzione di un full report non richiede ulteriori query sulla kb locale,
            # ma solamente l'interrogazione della kb remota.

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
                query_df = _query_miur_sparql(query_rem, defs.KB_MIUR_ENDPOINT1)

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

    print(f"Done. Facts generated at {defs.kb_job2_facts_o_path}.")


def run_job3():
    """Procedura Job3. Vedi: report.pdf."""

    print("Running Job 3...")

    query_kb = "is_partial_report2(institute_with_all_schools(institute(Institute_ID, Institute_Name), Institute_Schools_IDs), schoolassoc(Url, School_ID))."

    with open(defs.kb_job3_facts_o_path, "w") as pl_out:
        for result in prolog.query(query_kb):
            pl_out.write(result)

    print(f"Done. Facts generated at {defs.kb_job3_facts_o_path}.")


if __name__ == "__main__":
    create_page_facts_from_ds(defs.ds3_gt_no_noise_path)
    create_geo_facts_from_ds(defs.ds3_gt_no_noise_path)

    prolog.consult(f"./{defs.kb_shared_facts_path.name}")
    prolog.consult(f"./{defs.kb_shared_rules_path.name}")

    run_job1()

    run_job2()

    run_job3()
