# Metodi di Ingegneria della Conoscenza applicati alle homepage delle scuole superiori italiane

## Idea del progetto
L’idea di fondo da cui si è partiti per lo sviluppo di questo progetto è l’applicazione di alcuni metodi di Ingegneria della Conoscenza su un dominio di interesse, in questo caso l’usabilità di una pagina web. Questo richiede che prima si vada a circoscrivere un ambito di riferimento, che nel nostro caso, è l’insieme delle Homepage delle scuole superiori pubbliche italiane (aggiornate a settembre 2023).

## Download
1. Download [SWI-Prolog](https://www.swi-prolog.org/Download.html).
2. Clonare la repo.
   ```bash
   git clone https://github.com/vodibe/icon-745751.git
   ```
3. Posizionarsi nella cartella.
   ```bash
   cd path/to/icon-745751
   ```
4. Creare ambiente virtuale Python.
   ```bash
   python -m venv venv
   ```
5. Abilitare l'ambiente virtuale Python. 
   ```bash
   .\venv_run.ps1
   ```
6. Installare pacchetto del progetto nell'ambiente virtuale. 
   ```bash
   pip install -e .
   ```
   
## Documentazione e risultati
Vedere `docs/report.pdf`. Questo file rimanda ai percorsi dove sono memorizzati i vari output delle sezioni del progetto.

## Esecuzione

**Tutti i seguenti step sono facoltativi, in quanto il progetto include già i vari risultati.**

1. Aprire la cartella del progetto con un qualsiasi IDE, ad es. [VS Code](https://code.visualstudio.com/). 
   ```bash
   cd path/to/icon-745751/
   code .
   ```
2. Creazione dei dataset (già inclusi).
3. Creare ed esplorare un NDOM per un qualsiasi sito web o file sorgente locale.
   ```bash
   cd agent/ndom
   ```
   Modificare il file `agent/ndom/NaiveDOM.py`.
   ```python
   # sito web -> NDOM
   myNDOM = NaiveDOM("https://www.liceofermicanosa.edu.it/")
   # sorgente locale -> NDOM
   myNDOM = NaiveDOM(location="mysource.html",
                     from_file=True)

   # leggiamo le features estratte
   print(myNDOM.get_features())
   # visualizziamo
   myNDOM.plot()
   ```
   Eseguirlo.
   ```bash
   python NaiveDOM.py
   ```
4. Esplorare il procedimento di costruzione dei modelli di apprendimento.
   Eseguire (in ordine) ogni cella di codice del file `agent/models/nb_supervised_learning.ipynb`.

5. Sfruttare la base di conoscenza per creare i report dei Job. Questa fase impiegherà qualche minuto, si effettueranno diverse richieste al server del MIUR. I report dei job sono salvati in `agent/kb/jobs`.
   ```bash
   cd agent/kb
   python kb_creator.py
   ```
6. Creare la Rete Bayesiana.
   ```bash
   cd agent/pgm
   python bn_creator.py
   ```
     
   