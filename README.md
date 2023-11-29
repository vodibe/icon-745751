# Metodi di Ingegneria della Conoscenza applicati alle homepage delle scuole superiori italiane

## Idea del progetto
L’idea di fondo da cui si è partiti per lo sviluppo di questo progetto è l’applicazione di alcuni metodi di Ingegneria della Conoscenza su un dominio di interesse, in questo caso l’usabilità di una pagina web.   Questo richiede che prima si vada a circoscrivere un ambito di riferimento, che nel nostro caso, è l’insieme delle Homepage delle scuole superiori pubbliche italiane (aggiornate a settembre 2023).

## Download
1. Download [SWI-Prolog](https://www.swi-prolog.org/Download.html).
2. ```git clone https://github.com/vodibe/icon-745751.git```
3. ```cd path/to/icon-745751```
4. ```python -m venv icon-745751```
5. ```pip install -r requirements.txt```
6. ???

## Struttura

- `/agent`: Contiene il codice sorgente dell'agente. Non esiste un entry-point, ogni argomento trattato ne ha uno.
  - `/kb`: Knowledge Base in Prolog.
  - `/libs`: Librerie esterne (non scaricate con `pip`). 
  - `/models`: Modelli SL discussi in un notebook Python.
  - `/ndom`: Modello di rappresentazione tramite grafo delle homepage.
  - `/pgm`: Rete Bayesiana.
  - `/preproc`: Script di creazione dei dataset.
  - `definitions.py`: Variabili e costanti importate dai moduli appena elencati.
  - `requirements.txt`: Requisiti.
- `/datasets`: Cartella dei dataset.
- `/docs`: Documentazione, immagini, note utili.
- `setup.py`: Script di installazione del pacchetto `agent`.

