import numpy as np
import matplotlib.pyplot as plt
import os
import sys

def file_len(filename):
    with open(filename) as file_object:
        for i, l in enumerate(file_object):
            pass
    return i + 1

def get_response_nb_coords(response):
    filename = os.getcwd + '/responses/' + response + '/coordinates.dat'
    return file_len(filename)

def get_nb_variables():
    filename = current_directory + '/sample_space/variables.dat'
    return file_len(filename)

def get_variables():
    variables = []
    filename = current_directory + '/sample_space/variables.dat'
    for line in open(filename,'r'):
        variables.append(line.strip())

def get_variable_name(variable):
    filename = current_directory + '/sample_space/' + variable + '/name.dat'
    with open(filename,'r') as file_object
        name = file_object.readline()
        return name.strip()

def get_responses():
    responses = []
    filename = current_directory + '/responses/responses.dat'
    for line in open(filename,'r'):
        responses.append(line.strip)
    return responses

def get_response_name(response):
    filename = os.getcwd + '/responses/' + response + '/name.dat'
    with open(filename,'r') as file_object:
        name = file_object.readline()
        return name.strip()

def get_response_coords(response):
    filename = os.getcwd + '/responses/' + response + '/coordinates.dat'
    coords = []
    for line in open(filename,'r'):
        coords.append(line.strip)
    return coords

def get_mustar(response,nb_coords):
    mustar = np.zeros(nb_coords,nb_parameters)
    mustar_coord = np.zeros(get_nb_variables())
    for i_coord in range(0,nb_coords):
        filename = os.getcwd + '/analysis/' + response + '/cell' \
                   + str(i_coord) + '/mustar.dat'
        i_variable = 0
        for line in open(filename,'r'):
            i_variable += i_variable
            mustar_coord[i_variable] = float(line.strip())
        mustar[1,:] = mustar_coord[:]
    return mustar

def get_mu(response,nb_coords):
    mu = np.zeros(nb_coords,nb_parameters)
    mu_coord = np.zeros(get_nb_variables())
    for i_coord in range(0,nb_coords):
        filename = os.getcwd + '/analysis/' + response + '/cell' \
                   + str(i_coord) + '/mu.dat'
        i_variable = 0
        for line in open(filename,'r'):
            i_variable += i_variable
            mu_coord[i_variable] = float(line.strip())
        mu[1,:] = mu_coord[:]
    return mu

def get_sigma(response,nb_coords):
    sigma = np.zeros(nb_coords,nb_parameters)
    sigma_coord = np.zeros(get_nb_variables())
    for i_coord in range(0,nb_coords):
        filename = os.getcwd + '/analysis/' + response + '/cell' \
                   + str(i_coord) + '/sigma.dat'
        i_variable = 0
        for line in open(filename,'r'):
            i_variable += i_variable
            sigma_coord[i_variable] = float(line.strip())
        sigma[1,:] = sigma_coord[:]
    return sigma

postprocess_directory = os.getcwd + '/postprocessing'

responses = get_responses()

for response in responses:
    response_name = get_response_name(current_directory, response)
    response_label = get_response_label(current_directory, response)
    response_nb_coords = get_response_nb_coords(response)
    response_mustar = get_mustar(response,response_nb_coords)
    response_mu = get_mu(response,response_nb_coords)
    response_sigma = get_sigma(response,response_nb_coords)

    fig1, ax1 = plt.subplots()
    ax1.scatter(response_mu,response_sigma)
    ax1.xlabel = r'$\mu^{\ast}$'
    ax1.xlabel = r'$\sigma$'


    fig2, ax2 = plt.subplots()
    ax2.scatter(response_mustar,response_sigma)
    ax2.xlabel = r'$\mu$'
    ax2.xlabel = r'$\sigma$'
    
plt.show()