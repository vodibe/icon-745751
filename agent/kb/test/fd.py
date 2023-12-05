from pandas import DataFrame
from agent.preproc.utils import _query_wikidata_kb, _query_miur_kb
import agent.definitions as defs


f = _query_wikidata_kb(
    """SELECT ?Citta ?ProvinciaLabel
WHERE {
  ?Citta wdt:P806 "C983";
         wdt:P31 wd:Q747074; # se in una scuola è in una frazione, fa parte della città 
         wdt:P131 ?Provincia.
  ?Provincia wdt:P131 ?Regione.
  ?Regione wdt:P36 ?CapoluogoDiRegione.
  ?Provincia wdt:P36 ?CapoluogoDiRegione. # la provincia ha come capitale il capoluogo di regione?
  #?Provincia wdt:P395 ?ProvinciaCodice. # se sì, dammi il codice della provincia

  SERVICE wikibase:label { bd:serviceParam wikibase:language "it,en". }
}
LIMIT 1"""
)
if f.empty:
    print("vuoto porcodio")
else:
    print(f)
