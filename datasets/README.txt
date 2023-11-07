ID: ds1
Name: SCUANAGRAFESTAT202324_1.csv
PK: CODICESCUOLA
Description: Dataset delle scuole pubbliche italiane (a.s. 2023/2024)
Url: https://dati.istruzione.it/opendata/opendata/catalogo/elements1/?area=Scuole
Notes: Le features sono descritte qui: https://dati.istruzione.it/opendata/opendata/catalogo/elements1/leaf/?area=Scuole&datasetId=DS0400SCUANAGRAFESTAT
==========
ID: ds1_clean
Name: SCUANAGRAFE202324_1_clean.csv
PK: CODICESCUOLA
Description: Dataset delle scuole pubbliche italiane (a.s. 2023/2024) in cui sono state rimosse features inutili e sostituiti i link non validi.
Notes: Tempo di creazione: 2 ore circa
==========
ID: ds1_clean_unique
Name: SCUANAGRAFE202324_1_clean_unique.csv
PK: CODICESCUOLA
Description: Dataset delle scuole pubbliche italiane (a.s. 2023/2024) in cui sono state rimosse features inutili e sostituiti i link non validi. Rimossi i siti duplicati.
==========
ID: ds2_gt
Name: SCUANAGRAFE202324_2_gt.csv
PK: CODICESCUOLA
Description: Dataset delle feature che l'utente assegna a ciascun sito guardando la pagina.
==========
ID: ds3_gt
Name: SCUANAGRAFE202324_3_gt.csv
PK: CODICESCUOLA
Description: Dataset delle feature che l'utente assegna a ciascun sito guardando la pagina + features di ds1_clean.
==========
ID: ds3_gt_final
Name: SCUANAGRAFE202324_3_gt_final.csv
PK: CODICESCUOLA
Description: Dataset delle feature che l'utente assegna a ciascun sito guardando la pagina + features di ds1_clean. Rimossi i siti non validi.
==========
