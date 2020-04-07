import os
import utilities.general_utilities as gu

class Response:

    def __init__(self, response):

        top_directory = gu.get_top_directory()

        filename = top_directory + '/responses/' + response + '/coordinates.dat'
        self.coords = []
        for line in open(filename,'r'):
            self.coords.append(line.strip())
        self.nb_coords = len(self.coords)

        filename = top_directory + '/responses/' + response + '/name.dat'
        with open(filename,'r') as file_object:
            name = file_object.readline()
        self.name = name.strip()

        filename = top_directory + '/responses/' + response + '/label.dat'
        with open(filename,'r') as file_object:
            label = file_object.readline()
        self.label = label.strip()

    def get_nb_coords(self):
        return self.nb_coords

    def get_name(self):
        return self.name

    def get_label(self):
        return self.label

def get_responses():
    top_directory = gu.get_top_directory()
    responses = []
    filename = top_directory + '/responses/responses.dat'
    for line in open(filename,'r'):
        response = line.strip()
        responses.append(Response(response))
    return responses