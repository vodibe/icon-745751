import agent.definitions as defs

import owlready2

if __name__ == "__main__":
    o = owlready2.get_ontology(str(defs.onto_path))

    print(list(o.classes()), "\n")
