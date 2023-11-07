from pathlib import Path

# larghezza e altezza schermo
BROWSER_WIDTH = 1600
BROWSER_HEIGHT = 900
BROWSER_DIAG = 1836  # diagonale schermo

# User-Agent
headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.3;) AppleWebKit/536.10 (KHTML, like Gecko) Chrome/55.0.3745.151 Safari/603"
}

# icon-745751/
DIR_ROOT = Path(__file__).parent.parent

# icon-745751/datasets
DIR_DATASETS = DIR_ROOT / "datasets"

ds1_path = DIR_DATASETS / "SCUANAGRAFE202324_1.csv"  # ds originale
ds1_clean_path = DIR_DATASETS / "SCUANAGRAFE202324_1_clean.csv"  # ds senza altre scuole + url fix
ds1_clean_unique_path = DIR_DATASETS / "SCUANAGRAFE202324_1_clean_unique.csv"  # ds senza duplicati

ds2_gt_path = DIR_DATASETS / "SCUANAGRAFE202324_2_gt.csv"  # ds gt (solo feature richieste)

ds3_gt_path = DIR_DATASETS / "SCUANAGRAFE202324_3_gt.csv"  # ds gt con tutte le features
ds3_gt_final_path = DIR_DATASETS / "SCUANAGRAFE202324_3_gt_final.csv"  # ds gt senza siti non validi

# metrica
METRIC_MIN_VALUE = 1
METRIC_MAX_VALUE = 5
