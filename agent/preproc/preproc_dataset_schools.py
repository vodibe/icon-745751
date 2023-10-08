import csv
import requests
from agent.definitions import ds1_path, ds_schools1_path, headers


def create_dataset_schools(useful_TGIS):
    """A partire dal CSV memorizzato in ds1_path, crea un CSV
    memorizzato in DATASET_DIR contenente solo le scuole utili
    (nel nostro caso le scuole superiori)

    Args:
        - useful_TGIS (list): tipologie di scuole utili (TipologiaGradoIstruzioneScuola)
    """

    print("Creating dataset of useful schools...")
    with open(ds1_path, "r") as csv_in:
        csv_reader = csv.DictReader(csv_in)

        # Features del nuovo file CSV
        fieldnames = [
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
        with open(ds_schools1_path, "w", newline="") as csv_out:
            csv_writer = csv.DictWriter(csv_out, fieldnames=fieldnames)
            csv_writer.writeheader()

            for row in csv_reader:
                # inserisce nel nuovo CSV solo le scuole utili e che hanno un URL valido
                if (
                    row["DESCRIZIONECARATTERISTICASCUOLA"] == "NORMALE"
                    and row["DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA"] in useful_TGIS
                ):
                    school_url = process_url(row["SITOWEBSCUOLA"])

                    if school_url is not None:
                        new_row = {
                            "CODICESCUOLA": row["CODICESCUOLA"],
                            "DENOMINAZIONESCUOLA": row["DENOMINAZIONESCUOLA"],
                            "CODICEISTITUTORIFERIMENTO": row[
                                "CODICEISTITUTORIFERIMENTO"
                            ],
                            "DENOMINAZIONEISTITUTORIFERIMENTO": row[
                                "DENOMINAZIONEISTITUTORIFERIMENTO"
                            ],
                            "AREAGEOGRAFICA": row["AREAGEOGRAFICA"],
                            "REGIONE": row["REGIONE"],
                            "PROVINCIA": row["PROVINCIA"],
                            "CAPSCUOLA": row["CAPSCUOLA"],
                            "DESCRIZIONECOMUNE": row["DESCRIZIONECOMUNE"],
                            "DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA": row[
                                "DESCRIZIONETIPOLOGIAGRADOISTRUZIONESCUOLA"
                            ],
                            "SITOWEBSCUOLA": school_url,
                        }
                        csv_writer.writerow(new_row)

    print(f"Done. {ds_schools1_path}")


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
        r = requests.get(url, headers=headers, allow_redirects=False)
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


if __name__ == "__main__":
    with open("useful_TGIS.txt") as f:
        useful_TGIS = f.read().splitlines()

    # Crea dataset degli URL validi
    create_dataset_schools(useful_TGIS=useful_TGIS)
