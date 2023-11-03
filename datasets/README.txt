ID: 1
Name: SCUANAGRAFESTAT20232420230901.csv
PK: CODICESCUOLA
Description: Dataset delle scuole pubbliche italiane (a.s. 2023/2024)
Url: https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Scuole
Notes: Le features sono descritte qui: https://dati.istruzione.it/opendata/opendata/catalogo/elements1/leaf/?area=Scuole&datasetId=DS0400SCUANAGRAFESTAT
==========
ID: 1_clean
Name: SCUANAGRAFE202324_clean.csv
PK: CODICESCUOLA
Description: Dataset delle scuole pubbliche italiane (a.s. 2023/2024) in cui sono state rimosse features inutili e sostituiti i link non validi.
Notes: Tempo di creazione: 2 ore circa
==========
ID: 1_clean_unique
Name: SCUANAGRAFE202324_clean_unique.csv
PK: CODICESCUOLA
Description: Dataset delle scuole pubbliche italiane (a.s. 2023/2024) in cui sono state rimosse features inutili e sostituiti i link non validi. Rimossi i siti duplicati.
==========
ID: 2
Name: SCUANAGRAFE202324_features.csv
PK: CODICESCUOLA
Description: Dataset delle nuove feature assegnate a ciascun sito web.
Notes: Le features sono elencate nella costante ds2_features (ndom/NaiveDOM.py)
==========
