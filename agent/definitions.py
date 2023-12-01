from pathlib import Path
import numpy as np

# ----- percorsi
# icon-745751/
DIR_ROOT = Path(__file__).parent.parent

# path datasets
DIR_DATASETS = DIR_ROOT / "datasets"

ds1_path = DIR_DATASETS / "SCUANAGRAFE202324_1.csv"  # originale
ds1_clean_path = DIR_DATASETS / "SCUANAGRAFE202324_1_clean.csv"  # no altre scuole+urlfix
ds1_clean_unique_path = DIR_DATASETS / "SCUANAGRAFE202324_1_clean_unique.csv"  # no dupl.

ds2_gt_path = DIR_DATASETS / "SCUANAGRAFE202324_2_gt.csv"  # gt con solo feature richieste

ds3_gt_path = DIR_DATASETS / "SCUANAGRAFE202324_3_gt.csv"  # gt con tutte feature
ds3_gt_final_path = DIR_DATASETS / "SCUANAGRAFE202324_3_gt_final.csv"  # gt senza siti nv.
ds3_gt_no_noise_path = DIR_DATASETS / "SCUANAGRAFE202324_3_gt_no_noise.csv"  # gt no rum.

test_path = DIR_DATASETS / "test.csv"

# path benchmark ndom
DIR_GRAPH_BENCHMARK = DIR_ROOT / "ndom" / "benchmark"

# path knowledge base
DIR_KB = DIR_ROOT / "agent" / "kb"
DIR_KB_JOBS = DIR_KB / "jobs"

kb_facts_path = DIR_KB / "kb_facts.pl"
kb_rules_path = DIR_KB / "kb_rules.pl"

kb_job1_facts = DIR_KB_JOBS / "job1_facts.pl"

# path rete bayesiana
DIR_BIF = DIR_ROOT / "agent" / "pgm" / "bif"

# ----- costanti ndom
# larghezza, altezza, diagonale schermo
BROWSER_WIDTH = 1600
BROWSER_HEIGHT = 900
BROWSER_DIAG = 1836

# User-Agent
headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.3;) AppleWebKit/536.10 (KHTML, like Gecko) Chrome/55.0.3745.151 Safari/603"
}

# algoritmi di ricerca non informati di default
uninformed_search_algs = ["DFS", "BFS", "LCFS"]
benchmark_search_algs_ = ["NaiveDOMSearcher", "DFS", "BFS", "LCFS"]

# ----- costanti modelli
# features
ds3_features_pk = ["school_id", "page_url"]
ds3_features_askable = [
    "page_template",
    "page_menu_or",
    "page_ungrouped_multim",
    "metric",
]
ds3_features_part = [
    "page_load_time_ms",
    "page_width",
    "page_height",
    "NDOM_nodes",
    "NDOM_height",
]
ds3_target = "metric"

# dizionario dei target predefinito
TASKS_DEFAULT = {
    "task1": ["circolari", "comunicazioni", "circolare"],
    "task2": ["organigramma", "organizzazione", "schema organizzativo", "persone"],
    "task3": ["notizie", "news", "eventi"],
    "task4": ["progetti", "progetto", "projects"],
    "task5": ["regolamento", "regolamenti", "regolamentazione"],
    "task6": ["amministrazione trasparente", "ammin. trasparente"],
    "task7": ["registro"],
    "task8": ["indirizzo", "i luoghi", "dove siamo", "contatti"],
}

ds3_features = (
    ds3_features_pk
    + ds3_features_part
    + ds3_features_askable
    + list(TASKS_DEFAULT.keys())
)

# features da escludere per i modelli di sl e per la bn
ds3_features_excluded = ds3_features_pk

# intervalli dei domini di alcune feature
PAGE_TEMPLATE_MIN_VALUE = 1
PAGE_TEMPLATE_MAX_VALUE = 9

PAGE_MENU_OR_MIN_VALUE = 0
PAGE_MENU_OR_MAX_VALUE = 3

METRIC_MIN_VALUE = 1
METRIC_MAX_VALUE = 5
METRIC_STEP = 0.1

ds3_gt_feature_domains = {
    "school_id": str,
    "page_url": str,
    "page_template": [
        x for x in range(PAGE_TEMPLATE_MIN_VALUE, PAGE_TEMPLATE_MAX_VALUE + 1)
    ],
    "page_menu_or": [
        x for x in range(PAGE_MENU_OR_MIN_VALUE, PAGE_MENU_OR_MAX_VALUE + 1)
    ],
    "page_ungrouped_multim": lambda v: v >= 0 and v % 1 == 0,
    "metric": [
        x / 10
        for x in range(
            (METRIC_MIN_VALUE * 10), (METRIC_MAX_VALUE * 10 + int(METRIC_STEP * 10))
        )
    ],
    "page_load_time_ms": lambda v: v >= 0,
    "page_width": lambda v: v >= 0,
    "page_height": lambda v: v >= 0,
    "NDOM_nodes": lambda v: v >= 0 and v % 1 == 0,
    "NDOM_height": lambda v: v >= 0 and v % 1 == 0,
    "task1": lambda v: v >= 0,
    "task2": lambda v: v >= 0,
    "task3": lambda v: v >= 0,
    "task4": lambda v: v >= 0,
    "task5": lambda v: v >= 0,
    "task6": lambda v: v >= 0,
    "task7": lambda v: v >= 0,
    "task8": lambda v: v >= 0,
}

# ----- costanti KB
kb_miur_endpoint = "https://dati.istruzione.it/opendata/SCUANAGRAFESTAT/query"

# ----- costanti BN

DS_DISCRETE_MAPPING_DEFAULT = {
    "page_template": ([1, 2, 3, 4, 5, 6, 7, 8, 9, np.inf], [1, 2, 3, 4, 5, 6, 7, 8, 9]),
    "page_menu_or": ([0, 1, 2, 3, np.inf], [0, 1, 2, 3]),
    "page_ungrouped_multim": ([0, 6, 11, 21, np.inf], [1, 2, 3, 4]),
    "metric": ([1, 2, 3, 4, np.inf], [1, 2, 3, 4]),
    "page_load_time_ms": ([0, 1501, 3001, np.inf], [1, 2, 3]),
    "page_width": ([0, 1800, np.inf], [1, 2]),
    "page_height": ([0, 2001, 4001, 6001, np.inf], [1, 2, 3, 4]),
    "NDOM_nodes": ([0, 501, 1001, 1501, np.inf], [1, 2, 3, 4]),
    "NDOM_height": ([0, 5, 14, np.inf], [1, 2, 3]),
    "task1": ([0, 5, 10, 20, np.inf], [1, 2, 3, 4]),
    "task2": ([0, 5, 10, 20, np.inf], [1, 2, 3, 4]),
    "task3": ([0, 5, 10, 20, np.inf], [1, 2, 3, 4]),
    "task4": ([0, 5, 10, 20, np.inf], [1, 2, 3, 4]),
    "task5": ([0, 5, 10, 20, np.inf], [1, 2, 3, 4]),
    "task6": ([0, 5, 10, 20, np.inf], [1, 2, 3, 4]),
    "task7": ([0, 5, 10, 20, np.inf], [1, 2, 3, 4]),
    "task8": ([0, 5, 10, 20, np.inf], [1, 2, 3, 4]),
}
