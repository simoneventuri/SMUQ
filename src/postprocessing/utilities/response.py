import os
import utilities.general_utilities as gu
import numpy as np

class Response:

    def __init__(self, response):

        top_directory = gu.get_top_directory()

        filename = top_directory + '/responses/' + response \
                   + '/coordinates.dat'
        self.coords = np.genfromtxt(filename, dtype=float)
        self.nb_coords = self.coords.shape[0]
        self.nb_coords_dim = 1
        if len(self.coords.shape) > 1:
            self.nb_coords_dim = self.coords.shape[1]

        filename = top_directory + '/responses/' + response \
                   + '/coordinate_labels.dat'
        self.coords_labels = []
        for line in open(filename,'r'):
            if (len(line.strip()) > 0):
                self.coords_labels.append(line.strip())


        filename = top_directory + '/responses/' + response + '/name.dat'
        with open(filename,'r') as file_object:
            name = file_object.readline()
        self.name = name.strip()

        filename = top_directory + '/responses/' + response + '/label.dat'
        with open(filename,'r') as file_object:
            label = file_object.readline()
        self.label = label.strip()

    def get_coords(self):
        return self.coords

    def get_nb_coords(self):
        return self.nb_coords

    def get_nb_coords_dim(self):
        return self.nb_coords_dim

    def get_name(self):
        return self.name

    def get_label(self):
        return self.label

    def get_coords_labels(self):
        return self.coords_labels

    def get_coords_label(self, i_coord):
        return self.coords_labels[i_coord]

def get_responses():
    top_directory = gu.get_top_directory()
    responses = []
    filename = top_directory + '/responses/responses.dat'
    for line in open(filename,'r'):
        response = line.strip()
        responses.append(Response(response))
    return responses