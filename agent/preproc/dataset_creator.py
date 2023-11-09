import csv
import requests
import agent.definitions as defs
from agent.ndom.NaiveDOM import NaiveDOM
from utils import _create_driver


def process_url(url):
    """Analizza l'URL della scuola e restituisce l'URL valido se questo esiste;

    Args:
        - url (string): URL della scuola

    Returns:
        - string: URL valido della scuola; None se non esiste
    """

    print(f"Processing: {url}")
    s = url.split(".")

    if len(s) == 1:
        return None

    # ottimizzazione url
    if s[0][len(s[0]) - 1].lower() != "w":
        s.insert(0, "https://www")
    elif s[0].lower() != "https://www":
        s[0] = "https://www"

    # controllo se e' del tipo http://www.liceo.gov.it
    # https://www.miur.gov.it/nuovo-dominio-edu.it
    tld_gov = True if len(s) == 4 and s[2].lower() == "gov" else False

    url = ".".join(s)

    try:
        # richiesta al server, header necessari per non avere 403
        r = requests.get(url, headers=defs.headers, allow_redirects=False)
    except:
        if tld_gov:
            # sito non valido con tld .gov.it, riprovo con tld .edu.it
            s[2] = "edu"
            return process_url(".".join(s))
        else:
            # sito non valido con tld .edu.it
            return None

    if not r.ok:
        if tld_gov:
            # sito non valido con tld .gov.it, riprovo con tld .edu.it
            s[2] = "edu"
            return process_url(".".join(s))
        else:
            # sito non valido con tld .edu.it
            return None
    elif r.status_code == 301:
        # sito valido ma redireziona a un sito aggiornato
        return r.headers["location"]
    else:
        # sito valido
        return url


def create_ds1_clean(useful_TGIS):
    """A partire dal CSV memorizzato in ds1_path, crea un CSV
    memorizzato in DATASET_DIR contenente solo le scuole utili
    (nel nostro caso le scuole superiori)

    Args:
        - useful_TGIS (list): tipologie di scuole utili (TipologiaGradoIstruzioneScuola)
    """

    print("Creating ds1_clean (useful schools + URL fix)...")

    with open(defs.ds1_path, "r") as csv_in:
        csv_reader = csv.DictReader(csv_in)

        # features del nuovo file CSV
        ds1_clean_features = [
            "CODICESCUOLA",
            "DENOMINAZIONESCUOLA",
            "CODICEISTITUTORIFERIMENTO",
            "DENOMINAZIONEISTITUTORIFERIMENTO",
            "AREAGEOGRAFICA",
            "REGIONE",
            "PROVINCIA",
            "CAPSCUOLA",
            "DESCRIZIONECOMUNE",
            "DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA",
            "SITOWEBSCUOLA",
        ]

        # features che non vanno inserite nel nuovo file CSV
        ds1_clean_features_excluded = [
            "ANNOSCOLASTICO",
            "INDIRIZZOSCUOLA",
            "CODICECOMUNESCUOLA",
            "DESCRIZIONECARATTERISTICASCUOLA",
            "INDICAZIONESEDEDIRETTIVO",
            "INDICAZIONESEDEOMNICOMPRENSIVO",
            "INDIRIZZOEMAILSCUOLA",
            "INDIRIZZOPECSCUOLA",
            "SEDESCOLASTICA",
        ]

        with open(defs.ds1_clean_path, "w", newline="") as csv_out:
            csv_writer = csv.DictWriter(csv_out, fieldnames=ds1_clean_features)
            csv_writer.writeheader()

            for row in csv_reader:
                # inserisce nel nuovo CSV solo le scuole utili e che hanno un URL valido
                if (
                    row["DESCRIZIONECARATTERISTICASCUOLA"] == "NORMALE"
                    and row["DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA"] in useful_TGIS
                ):
                    school_url = process_url(row["SITOWEBSCUOLA"])

                    if school_url is not None:
                        new_row = row
                        new_row["SITOWEBSCUOLA"] = school_url
                        for feature in ds1_clean_features_excluded:
                            del new_row[feature]

                        csv_writer.writerow(new_row)

    print("Done.")


def create_ds2_gt(i_resume=1):
    """Crea il dataset ground truth delle feature per ciascun sito.

    Args:
        - i_resume (int, optional): Numero del sito a partire dal quale procedere. Default: 1.
    """

    # validazione indice di ripresa
    i_resume = 0 if i_resume <= 1 else (i_resume - 1)

    print(f"Creating ds2_gt (user ground truth) (starting at {i_resume})...")

    # creo browser per far visualizzare la pagina all'utente
    browser = _create_driver(defs.BROWSER_WIDTH, defs.BROWSER_HEIGHT)

    with open(defs.ds1_clean_unique_path, "r") as csv_in:
        csv_reader = csv.DictReader(csv_in, delimiter=";")

        with open(defs.ds2_gt_path, "a", newline="") as csv_out:
            csv_writer = csv.DictWriter(
                csv_out, fieldnames=defs.ds3_features_pk + defs.ds3_features_askable
            )

            if i_resume == 0:
                csv_writer.writeheader()

            i = 0
            for row in csv_reader:
                if i < i_resume:
                    pass
                else:
                    # l'utente visualizza il sito
                    print(f"\nWebsite #{(i+1)}")
                    browser.get(row["SITOWEBSCUOLA"])

                    new_row = dict()

                    # features pk assegnate automaticamente per identificare il sito corrente
                    new_row["school_id"] = row["CODICESCUOLA"]
                    new_row["page_url"] = row["SITOWEBSCUOLA"]

                    # richiedi le altre features
                    for feature_askable in defs.ds3_features_askable:
                        new_row[feature_askable] = float(input("- " + feature_askable + ": "))

                    csv_writer.writerow(new_row)
                i = i + 1
    print("Done.")


def create_ds3_gt():
    """Crea il dataset di tutte le feature per ciascun sito.
    Necessario per addestrare i modelli di SL e UL.
    """

    print("Creating ds3_gt (features for each website)...")

    # creo driver condiviso da tutti i NDOM che vado a creare
    driver = _create_driver(defs.BROWSER_WIDTH, defs.BROWSER_HEIGHT)

    with open(defs.ds2_gt_path, "r") as csv_in:
        csv_reader = csv.DictReader(csv_in)

        with open(defs.ds3_gt_full_path, "w", newline="") as csv_out:
            csv_writer = csv.DictWriter(csv_out, fieldnames=defs.ds3_features)
            csv_writer.writeheader()

            for row in csv_reader:
                # analisi sito
                NDOM_website = NaiveDOM(
                    location=row["page_url"],
                    alias=row["school_id"],
                    driver=driver,
                    driver_close_at_end=False,
                )

                # a partire dal NDOM, ottieni le features
                new_row = NDOM_website.get_features()

                # ottieni le altre features
                for feature_askable in defs.ds3_features_askable:
                    new_row[feature_askable] = row[feature_askable]

                csv_writer.writerow(new_row)
    print("Done.")


def create_ds3_gt_final():
    """Rimuove i siti con metrica -1, ovvero quei siti che risultano raggiungibili ma
    non sono validi (es. cambio dominio, in manutenzione, siti di scuole non correttamente
    catalogate nel ds1.)
    """

    print("Creating ds3_gt_final (removing invalid websites)...")

    with open(defs.ds3_gt_path, "r") as csv_in:
        csv_reader = csv.DictReader(csv_in)

        with open(defs.ds3_gt_final_path, "w", newline="") as csv_out:
            csv_writer = csv.DictWriter(csv_out, fieldnames=defs.ds3_features)
            csv_writer.writeheader()

            for row in csv_reader:
                metric = float(row["metric"])
                if metric >= defs.METRIC_MIN_VALUE and metric <= defs.METRIC_MAX_VALUE:
                    csv_writer.writerow(row)
    print("Done.")


if __name__ == "__main__":
    """'
    with open("useful_TGIS.txt") as f:
        useful_TGIS = f.read().splitlines()

    create_ds1_clean(useful_TGIS=useful_TGIS)
    """

    # crea ds1_clean_unique (ds senza siti duplicati) (fatto con excel)

    # create_ds2_gt(i_resume=1)

    # create_ds3_gt()

    # create_ds3_gt_final()
