import pandas as pd
from pgmpy.inference import VariableElimination
from pgmpy.readwrite import XMLBIFReader


class BeliefNetwork:

    def __init__(self, maxNofParents):
        if maxNofParents == 1:
            reader = XMLBIFReader('..\Datasets\BN_structure_1.xml')

        if maxNofParents == 2:
            reader = XMLBIFReader('..\Datasets\BN_structure_2.xml')

        if maxNofParents == 3:
            reader = XMLBIFReader('..\Datasets\BN_structure_3.xml')

        self.model = reader.get_model()

    def ask_queries(self, string):
        # pone la domanda al sistema
        inference = VariableElimination(self.model)
        discreteAttribute, evidences = self.compute_query(string)
        query = inference.query([discreteAttribute], evidence=evidences)
        self.print_query(discreteAttribute, query)

    def compute_query(self, input_string):
        # estrae dalla stringa le variabili per porre la query
        discreteAttribute = input_string.split(':')[0].strip()

        attribute_dict = {}
        attribute_list = input_string.split(':')[1].split(',')
        for attribute in attribute_list:
            attribute = attribute.strip()
            key, value = attribute.split('=')
            attribute_dict[key.strip()] = value.strip()

        return discreteAttribute, attribute_dict

    def print_query(self, featureName, query):
        # stampa i risultati della query con probabilità maggiore di 0
        for key, value in zip(query.state_names[featureName], query.values):
            if value > 0.009:
                print(key, ': ', value)

        print('\n')
