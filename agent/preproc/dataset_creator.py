import csv
import requests
import agent.definitions as defs
from agent.ndom.NaiveDOM import ds2_features, ds2_features_askable, NaiveDOM
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


def create_dataset_schools(useful_TGIS):
    """A partire dal CSV memorizzato in ds1_path, crea un CSV
    memorizzato in DATASET_DIR contenente solo le scuole utili
    (nel nostro caso le scuole superiori)

    Args:
        - useful_TGIS (list): tipologie di scuole utili (TipologiaGradoIstruzioneScuola)
    """

    print("Creating dataset of useful schools...")
    with open(defs.ds1_path, "r") as csv_in:
        csv_reader = csv.DictReader(csv_in)

        # features del nuovo file CSV
        ds1_features = [
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
        ds1_features_excluded = [
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
            csv_writer = csv.DictWriter(csv_out, fieldnames=ds1_features)
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
                        for feature in ds1_features_excluded:
                            del new_row[feature]

                        csv_writer.writerow(new_row)

    print(f"Done. {defs.ds1_clean_path}")


def create_dataset_features(i_resume=1):
    """Crea il dataset delle feature per ciascun sito. Necessario per addestrare i modelli
    di SL e UL.

    Args:
        i_resume (int, optional): Numero del sito a partire dal quale procedere. Default: 1.
    """

    print("Creating dataset of features for each website...")
    # validazione indice di ripresa
    i_resume = 0 if i_resume <= 1 else (i_resume - 1)
    print(f"(Starting at {i_resume})")

    # creo driver condiviso da tutti i NDOM che vado a creare
    driver = _create_driver(defs.BROWSER_WIDTH, defs.BROWSER_HEIGHT)

    with open(defs.ds1_clean_unique_path, "r") as csv_in:
        csv_reader = csv.DictReader(csv_in, delimiter=";")

        with open(defs.ds2_path, "a", newline="") as csv_out:
            csv_writer = csv.DictWriter(csv_out, fieldnames=ds2_features)

            if i_resume == 0:
                csv_writer.writeheader()

            i = 0
            for row in csv_reader:
                if i < i_resume:
                    pass
                else:
                    # analisi sito
                    print(f"\nWebsite #{(i+1)}")
                    NDOM_website = NaiveDOM(
                        location=row["SITOWEBSCUOLA"],
                        alias=row["CODICESCUOLA"],
                        driver=driver,
                        driver_close_at_end=False,
                    )

                    # a partire dal NDOM, ottieni le features
                    new_row = NDOM_website.get_features()

                    # ottieni le altre features
                    for feature_askable in ds2_features_askable:
                        new_row[feature_askable] = float(input("- " + feature_askable + ": "))

                    csv_writer.writerow(new_row)
                i = i + 1
    print(f"Done.")


if __name__ == "__main__":
    # crea dataset degli URL validi
    """
    with open("useful_TGIS.txt") as f:
        useful_TGIS = f.read().splitlines()

    create_dataset_schools(useful_TGIS=useful_TGIS)
    """

    # crea dataset delle feature di ciascun url
    create_dataset_features(i_resume=941)

    # todo: cancellare le righe dove la metrica è -1 (siti non corretti)
    # todo: cancellare duplicati, cioè scuole con stesso sito. far rimanere solo 1.
