import agent.definitions as defs
from selenium import webdriver


def _create_driver(browser_width, browser_height) -> webdriver:
    """Restituisce un'istanza della classe webdriver, cioè un browser avente una scheda
    aperta al sito location.
    Vedi: https://stackoverflow.com/a/55878622

    Args:
        - browser_width: lunghezza finestra browser.
        - browser_height: altezza finestra browser.

    Returns:
        webdriver: istanza webdriver
    """

    options = webdriver.FirefoxOptions()
    # options.add_argument("--headless")
    options.add_argument(f'--user-agent={defs.headers["User-Agent"]}')
    options.add_argument(f"--width={browser_width}")
    options.add_argument(f"--height={browser_height}")

    driver = webdriver.Firefox(options=options)

    return driver
