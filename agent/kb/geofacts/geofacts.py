import csv
import agent.definitions as defs
from agent.preproc.utils import _pl_str, _query_miur_kb, _query_wikidata_kb


def create_geofacts3_from_ds(ds_path, out_path):
    """Crea una lista di fatti, ciascuno rappresentante semanticamente le informazioni
    geografiche di una pagina del dataset ds_path.

    Args:
        - ds_path: Percorso dataset di partenza.
        - out_path: Percorso di salvataggio.
    """

    _kb_remote_err = 0

    with open(out_path, "w") as pl_out:
        with open(ds_path, "r") as csv_in:
            csv_reader = csv.DictReader(csv_in, delimiter=",")

            for row in csv_reader:
                # fmt:off
                query_rem = (
                    """
                    PREFIX miur: <http://www.miur.it/ns/miur#>
                    Select ?CodiceCatastaleComune ?Provincia ?Regione {
                        graph ?g {
                            ?S miur:CODICESCUOLA """ + _pl_str(row["school_id"]) + """.
                            ?S miur:CODICECOMUNESCUOLA ?CodiceCatastaleComune.
                            ?S miur:PROVINCIA ?Provincia.
                            ?S miur:REGIONE ?Regione.
                        }
                    }
                    LIMIT 1
                    """
                )  # fmt:on
                query_df = _query_miur_kb(query_rem, defs.KB_MIUR_ENDPOINT1)

                if not query_df.empty:
                    geofact = (
                        f'\nschool_geofact({_pl_str(row["school_id"])}, '
                        f'city({_pl_str(query_df.loc[0, "CodiceCatastaleComune"])}), '
                        f'province({_pl_str(query_df.loc[0, "Provincia"])}), '
                        f'region({_pl_str(query_df.loc[0, "Regione"])})).'
                    )
                    pl_out.write(geofact)

        clause_place = """
        \nschool_is_in_place(School_ID, Place) :- school_geofact(School_ID, _, _, region(Place)).
        """
        pl_out.write(clause_place)

    print(f"(_kb_remote_err: {_kb_remote_err})")


def create_geofacts4_from_ds(ds_path, out_path):
    """Crea una lista di fatti, ciascuno rappresentante semanticamente le informazioni
    geografiche di una pagina del dataset ds_path.

    Args:
        - ds_path: Percorso dataset di partenza.
        - out_path: Percorso di salvataggio.
    """

    _kb_remote_err = 0

    with open(out_path, "w") as pl_out:
        with open(ds_path, "r") as csv_in:
            csv_reader = csv.DictReader(csv_in, delimiter=",")

            for row in csv_reader:
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
                query_df1 = _query_miur_kb(query_rem, defs.KB_MIUR_ENDPOINT1)

                if not query_df1.empty:
                    # fmt:off
                    query_rem = ("""
                        SELECT ?ProvinciaLabel
                        WHERE {
                            ?Citta wdt:P806 """ + _pl_str(query_df1.loc[0, "CodiceCatastaleComune"]) + """;
                            wdt:P31 wd:Q747074;
                            wdt:P131 ?Provincia.
                            ?Provincia wdt:P131 ?Regione.
                            ?Regione wdt:P36 ?CapoluogoDiRegione.
                            ?Provincia wdt:P36 ?CapoluogoDiRegione.

                            SERVICE wikibase:label { bd:serviceParam wikibase:language "it,en". }
                        }
                        LIMIT 1
                    """)  # fmt:on
                    query_df2 = _query_wikidata_kb(query_rem)

                    if not query_df2.empty:
                        # la scuola si trova in una provincia e la città "capitale" di questa
                        # provincia è capoluogo della sua regione
                        geofact = (
                            f'\nschool_geofact({_pl_str(row["school_id"])}, '
                            f'province({_pl_str(query_df2.loc[0, "ProvinciaLabel"])})).'
                        )
                        pl_out.write(geofact)
                else:
                    _kb_remote_err = _kb_remote_err + 1

        clause_place = """
        \nschool_is_in_place(School_ID, Place) :- school_geofact(School_ID, province(Place)).
        """
        pl_out.write(clause_place)

    print(f"(_kb_remote_err: {_kb_remote_err})")
