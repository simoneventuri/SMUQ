import os
import utilities.general_utilities as gu

class Variable:

    def __init__(self, variable):

        top_directory = gu.get_top_directory()

        filename = top_directory + '/sample_space/' + variable + '/name.dat'
        with open(filename,'r') as file_object:
            name = file_object.readline()
        self.name = name.strip()

        filename = top_directory + '/sample_space/' + variable + '/label.dat'
        with open(filename,'r') as file_object:
            label = file_object.readline()
        self.label = label.strip()

    def get_name(self):
        return self.name

    def get_label(self):
        return self.label

def get_variables():
    top_directory = gu.get_top_directory()
    variables = []
    filename = top_directory + '/sample_space/variables.dat'
    for line in open(filename,'r'):
        variable = line.strip()
        variables.append(Variable(variable))
    return variables