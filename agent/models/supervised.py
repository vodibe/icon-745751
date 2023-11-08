import agent.definitions as defs
import pandas as pd

# valore massimo della feature page_template
PAGE_TEMPLATE_MAX_VALUE = 9
# valore massimo della feature page_menu_or
PAGE_MENU_OR_MAX_VALUE = 3

# domini di ciascuna feature
ds3_gt_feature_domains = {
    "school_id": lambda v: isinstance(v, str),
    "page_url": lambda v: isinstance(v, str),
    "page_template": lambda v: v in range(1, PAGE_TEMPLATE_MAX_VALUE + 1),
    "page_menu_or": lambda v: v in range(0, PAGE_MENU_OR_MAX_VALUE + 1),
    "page_ungrouped_multim": lambda v: v >= 0,
    "metric": lambda v: v
    in range(defs.METRIC_MIN_VALUE, defs.METRIC_MAX_VALUE + defs.METRIC_STEP, defs.METRIC_STEP),
    "page_load_time_ms": lambda v: v >= 0,
    "page_width": lambda v: v >= 0,
    "page_height": lambda v: v >= 0,
    "NDOM_nodes": lambda v: v >= 0,
    "NDOM_height": lambda v: v >= 0,
    "task1": lambda v: v >= 0,
    "task2": lambda v: v >= 0,
    "task3": lambda v: v >= 0,
    "task4": lambda v: v >= 0,
    "task5": lambda v: v >= 0,
    "task6": lambda v: v >= 0,
    "task7": lambda v: v >= 0,
    "task8": lambda v: v >= 0,
}


def detect_noisy_rows(ds_path, ds_feature_domains: dict):
    """Rileva quali sono le righe del DS ds_path in cui almeno una feature non è corretta,
    cioè non rientra nel suo dominio.

    Args:
        - ds_path: Percorso del dataset.
        - ds_feature_domains: Dizionario feature:funzione di validazione.
    """

    noisy_indexes = []

    df = pd.read_csv(ds_path)

    for i, row in df.iterrows():
        pass

    """
    filtered_data = df[
        (df["Pclass"] == 2) |
        (df["Pclass"] == 3) |
        ((df["Age"] >= 18) & (df["Age"] <= 50)) |
        (df["Sex"] == "female")
        # Aggiungi altre condizioni qui
    ]
    """


if __name__ == "__main__":
    detect_noisy_rows(ds_path=defs.test_path, ds_feature_domains=ds3_gt_feature_domains)
