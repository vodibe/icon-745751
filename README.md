# Metodi di Ingegneria della Conoscenza applicati alle homepage delle scuole superiori italiane

## Idea del progetto
L’idea di fondo da cui si è partiti per lo sviluppo di questo progetto è l’applicazione di alcuni metodi di Ingegneria della Conoscenza su un dominio di interesse, in questo caso l’usabilità di una pagina web. Questo richiede che prima si vada a circoscrivere un ambito di riferimento, che nel nostro caso, è l’insieme delle Homepage delle scuole superiori pubbliche italiane (aggiornate a settembre 2023).

## Documentazione e Risultati
Vedere `docs/report.pdf`. In ogni sezione si rimanda ai percorsi dei vari output.

## Download
1. Download [Python](https://www.python.org/downloads/) e [SWI-Prolog](https://www.swi-prolog.org/Download.html).
2. Clonare la repo.
   ```bash
   git clone https://github.com/vodibe/icon-745751.git
   ```
3. Posizionarsi nella cartella.
   ```bash
   cd path/to/icon-745751
   ```
4. Creare e abilitare ambiente virtuale Python.
   ```bash
   python -m venv venv

   venv\Scripts\Activate # Windows
   source venv/bin/activate # Linux
   ```
6. Installare pacchetto del progetto nell'ambiente virtuale. 
   ```bash
   pip install -e .
   ```
7. Installare dipendenze.
   ```bash
   pip install -r requirements.txt
   ```
   
## Esecuzione
Questo repository contiene già tutti i risultati e i file di output. Se si vuole esplorare le varie sezioni trattate nel progetto, seguire questi passi.

### :one: Se si vuole creare e visualizzare un NDOM di un sito web o di un file HTML...
1. ```bash
   cd agent/ndom
   ```
2. (Facoltativo) Modificare `NaiveDOM.py`
   ```python
   if __name__ == "__main__":
      # sito web -> NDOM
      myNDOM = NaiveDOM("https://www.liceofermicanosa.edu.it/")
      # sorgente locale -> NDOM
      # myNDOM = NaiveDOM(location="mysource.html", from_file=True)

      # leggiamo le features estratte
      print(myNDOM.get_features())
      # visualizziamo
      myNDOM.plot()
   ```
3. ```bash
   python NaiveDOM.py
   ```
4. **Output:** 
   ```
   Building NDOM for https://www.liceofermicanosa.edu.it/
   Reading HTML...
   Cleaning HTML...
   Parsing HTML <body> tag...
   Populating features...
   {'page_width': 1587, 'page_height': 4145, 'NDOM_height': 7, ...}
   ```
   ![ndom1](./docs/img/ndom_out.png)
### :two: Se si vuole costruire dei modelli di apprendimento per l'emulazione del GT...
1. Aprire la cartella del progetto con un qualsiasi IDE che supporti la visualizzazione di notebook `.ipynb` , ad es. [VS Code](https://code.visualstudio.com/). 
   ```bash
   cd path/to/icon-745751/
   code .
   ```
2. ```bash
   pip install ipykernel
   ```
3. Aprire `agent/models/nb_supervised_learning.ipynb`.
4. Eseguire in ordine tutte le celle di codice.

### :three: Se si vuole consultare la KB per eseguire i Job...
1. ```bash
   cd agent/kb
   ```
2. (Facoltativo) Modificare `kb_creator.py`
   ```python
   ...
   if __name__ == "__main__":
      ...

      # Specificare i Job da eseguire
      run_job1()

      run_job2()

      run_job3()

      run_job4(geofacts_created=True)

      run_job5(geofacts_created=True)
   ```
3. ```bash
   python kb_creator.py
   ```
4. **Percorso risultati**: `agent/kb/jobs/`

### :four: Se si vuole apprendere i parametri della Rete Bayesiana, visualizzarli, ed eseguire delle query...
1. ```bash
   cd agent/pgm
   ```
2. (Facoltativo) Modificare `bn_creator.py`
   ```python
   ...
   BN_QUERIES_DEFAULT = [
   {
      # Specificare una query in questo formato
      "query_desc": "P(page_template | metric=4)",
      "variables": ["page_template"],
      "evidence": {
            "metric": 4,
      },
      ...
   },
   ...
   ```
3. ```bash
   python bn_creator.py
   ```
4. **Percorso risultati**: `agent/pgm/bif/`
   