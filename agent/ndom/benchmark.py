import agent.definitions as defs
from agent.ndom.NaiveDOM import NaiveDOM
from agent.preproc.utils import _create_driver
import pandas as pd
from pandas import Series


def benchmark_search_algs(ds_path, out_path, n=400):
    """Crea un file xlsx in cui per ciascuno dei n siti e per ciascun algoritmo di ricerca
    calcola il numero di nodi esaminati per arrivare a un nodo obiettivo.

    Args:
        - ds_path: Percorso del DS dove sono contenuti i siti.
        - out_path: Percorso di output dei risultati.
        - n (int): Numero di siti da testare. Default: 400.
    """

    ds = pd.read_csv(ds_path)
    random_urls = ds.sample(n=n, random_state=8)
    random_urls = Series(random_urls["page_url"])

    driver = _create_driver(defs.BROWSER_WIDTH, defs.BROWSER_HEIGHT)

    invalid_urls = []

    benchmark_table = pd.DataFrame()
    for search_alg in defs.benchmark_search_algs_:
        for idx, page_url in random_urls.items():
            skip_row = False
            try:
                NDOM_website = NaiveDOM(
                    location=page_url,
                    driver=driver,
                    driver_close_at_end=False,
                    search_alg=search_alg,
                )
            except Exception:
                invalid_urls.append(page_url)
                skip_row = True

            if not skip_row:
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
    benchmark_search_algs(
        defs.ds3_gt_final_path, defs.DIR_GRAPH_BENCHMARK / "benchmark2.xlsx", n=400
    )
