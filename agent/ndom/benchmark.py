import agent.definitions as defs
from agent.ndom.NaiveDOM import NaiveDOM
from agent.preproc.utils import _create_driver
import pandas as pd
from pandas import Series


def benchmark_search_algs(ds_path, out_path, n=0):
    """Crea un file xlsx in cui per ciascuno dei n siti e per ciascun algoritmo di ricerca
    calcola il numero di nodi esaminati per arrivare a un nodo obiettivo.

    Args:
        - ds_path: Percorso del DS dove sono contenuti i siti.
        - out_path: Percorso di output dei risultati.
        - n (int): Numero di siti da testare. 0=tutto il dataset. Default: 0.
    """

    ds = pd.read_csv(ds_path)

    if n > 0:
        # campiona dal dataset
        ds = ds.sample(n=n, random_state=8)

    urls = Series(ds["page_url"])

    driver = _create_driver(defs.BROWSER_WIDTH, defs.BROWSER_HEIGHT)

    invalid_urls = []
    benchmark_table = pd.DataFrame()

    for idx, page_url in urls.items():
        try:
            NDOM_website = NaiveDOM(
                location=page_url,
                driver=driver,
                driver_close_at_end=False,
            )
        except Exception:
            invalid_urls.append(page_url)
            continue

        for search_alg in defs.list_benchmark_search_algs:
            print(f"\n(Setting {search_alg} NDOM search algorithm)")
            # cambia algoritmo
            NDOM_website.search_alg = search_alg
            # calcola dizionario task->nodi espansi
            NDOM_website._calc_task_cost()

            if all(v <= 1 for v in NDOM_website.nodes_expanded_per_task.values()):
                break
            else:
                # ottieni elementi da inserire nel benchmark_table
                benchmark_row = {"page_url": page_url, "search_alg": search_alg}
                benchmark_row.update(NDOM_website.nodes_expanded_per_task)

                benchmark_table = pd.concat(
                    [benchmark_table, pd.DataFrame([benchmark_row])], axis=0
                )

    benchmark_table.reset_index(drop=True, inplace=True)
    benchmark_table.to_excel(out_path, sheet_name="benchmarks")
    print(f"Warning: {invalid_urls}")
    print(f"Done. {out_path}")


if __name__ == "__main__":
    benchmark_search_algs(defs.ds3_gt_final_path, defs.ndom_benchmark_full_path)
