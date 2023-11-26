import csv
from pyswip import Prolog

# https://www.swi-prolog.org/pldoc/man?section=sparqlclient


def create_kb_facts(ds_path, out_path):
    with open(ds_path, "r") as csv_in:
        csv_reader = csv.DictReader(csv_in)

        for row in csv_reader:
            pass


if __name__ == "__main__":
    pass
