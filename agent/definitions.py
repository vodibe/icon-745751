from pathlib import Path

# icon-745751/
ROOT_DIR = Path(__file__).parent.parent

# icon-745751/datasets
DATASETS_DIR = ROOT_DIR / "datasets"

ds1_path = DATASETS_DIR / "SCUANAGRAFESTAT20232420230901.csv"
ds3_path = DATASETS_DIR / "URLS.csv"
ds_schools1_path = DATASETS_DIR / "SCUANAGRAFE202324_clean.csv"

# User-Agent
headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.3;) AppleWebKit/536.10 (KHTML, like Gecko) Chrome/55.0.3745.151 Safari/603"
}

# Browser width and height
BROWSER_WIDTH = 1600
BROWSER_HEIGHT = 900
