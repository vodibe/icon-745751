import agent.definitions as defs
from owlready2 import *

if __name__ == "__main__":
    for imported_onto_path in defs.imported_onto_paths:
        onto_path.append(str(imported_onto_path) + "\\")

    o = get_ontology(str(defs.my_onto_path)).load(only_local=True)

    print(type(o))
