from utilities.variable import Variable
from utilities.response import Response
import utilities.general_utilities as gu
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.ticker import AutoMinorLocator
from matplotlib.ticker import ScalarFormatter

def get_mustar(response):
    top_directory = gu.get_top_directory()
    nb_coords = response.get_nb_coords()
    response_label = response.get_label()
    filename = top_directory + '/analysis/' + response_label \
               + '/cell1/mu_star.dat'
    nb_variables = gu.file_len(filename)
    mustar = np.zeros((nb_coords,nb_variables))
    mustar_coord = np.zeros((nb_variables,))
    for i_coord in range(0,nb_coords):
        filename = top_directory + '/analysis/' + response_label + '/cell' \
                   + str(i_coord + 1) + '/mu_star.dat'
        for i_variable, line in enumerate(open(filename,'r')):
            mustar_coord[i_variable] = float(line.strip())
        mustar[i_coord,:] = mustar_coord[:]
    return mustar

def get_mu(response):
    top_directory = gu.get_top_directory()
    nb_coords = response.get_nb_coords()
    response_label = response.get_label()
    filename = top_directory + '/analysis/' + response_label \
               + '/cell1/mu.dat'
    nb_variables = gu.file_len(filename)
    mu = np.zeros((nb_coords,nb_variables))
    mu_coord = np.zeros((nb_variables,))
    for i_coord in range(0,nb_coords):
        filename = top_directory + '/analysis/' + response_label + '/cell' \
                   + str(i_coord + 1) + '/mu.dat'
        for i_variable, line in enumerate(open(filename,'r')):
            mu_coord[i_variable] = float(line.strip())
        mu[i_coord,:] = mu_coord[:]
    return mu

def get_sigma(response):
    top_directory = gu.get_top_directory()
    nb_coords = response.get_nb_coords()
    response_label = response.get_label()
    filename = top_directory + '/analysis/' + response_label \
               + '/cell1/sigma.dat'
    nb_variables = gu.file_len(filename)
    sigma = np.zeros((nb_coords,nb_variables))
    sigma_coord = np.zeros((nb_variables,))
    for i_coord in range(0,nb_coords):
        filename = top_directory + '/analysis/' + response_label + '/cell' \
                   + str(i_coord+1) + '/sigma.dat'
        for i_variable, line in enumerate(open(filename,'r')):
            sigma_coord[i_variable] = float(line.strip())
        sigma[i_coord,:] = sigma_coord[:]
    return sigma

def get_mustar_single(response,i_cell):
    top_directory = gu.get_top_directory()
    response_label = response.get_label()
    filename = top_directory + '/analysis/' + response_label \
               + '/cell' + str(i_cell + 1) + '/mu_star.dat'
    nb_variables = gu.file_len(filename)
    mustar = np.zeros((nb_variables,))
    for i, line in enumerate(open(filename,'r')):
        mustar[i] = float(line.strip())
    return mustar

def get_mu_single(response,i_cell):
    top_directory = gu.get_top_directory()
    response_label = response.get_label()
    filename = top_directory + '/analysis/' + response_label \
               + '/cell' + str(i_cell + 1) + '/mu.dat'
    nb_variables = gu.file_len(filename)
    mu = np.zeros((nb_variables,))
    for i, line in enumerate(open(filename,'r')):
        mu[i] = float(line.strip())
    return mu

def get_sigma_single(response,i_cell):
    top_directory = gu.get_top_directory()
    response_label = response.get_label()
    filename = top_directory + '/analysis/' + response_label \
               + '/cell' + str(i_cell + 1) + '/sigma.dat'
    nb_variables = gu.file_len(filename)
    sigma = np.zeros((nb_variables,))
    for i, line in enumerate(open(filename,'r')):
        sigma[i] = float(line.strip())
    return sigma

def plot_mu_sigma_single(response, i_cell, variables):
    response_name = response.get_name()
    response_mu = get_mu_single(response, i_cell)
    response_sigma = get_sigma_single(response, i_cell)

    fig1, ax1 = plt.subplots()
    ax1.scatter(response_mu,response_sigma)
    for i, variable in enumerate(variables):
        ax1.annotate(variable.get_name(), xy=(response_mu[i], \
                     response_sigma[i]), xytext=(5,5), \
                     textcoords="offset points")

    ax1.set_xlabel(r'$\mu$')
    ax1.set_ylabel(r'$\sigma$')
    ax1.set_title(response_name)
    ax1.set_aspect('auto')
    ax1.yaxis.set_major_formatter(ScalarFormatter())
    ax1.yaxis.major.formatter._useMathText = True
    ax1.yaxis.set_minor_locator(AutoMinorLocator(5))
    ax1.xaxis.set_minor_locator(AutoMinorLocator(5))
    ratio = 1.0
    ax1.set_aspect(1.0/ax1.get_data_ratio()*ratio)
    ax1.xaxis.set_tick_params(top='off', direction='out', width=0)
    ax1.yaxis.set_tick_params(right='off', direction='out', width=0)
    ax1.xaxis.set_tick_params(direction='out', width=1, length=5)
    ax1.yaxis.set_tick_params(direction='out', width=1, length=5)

def plot_mustar_sigma_single(response, i_cell, variables):
    response_name = response.get_name()
    response_mustar = get_mustar_single(response, i_cell)
    response_sigma = get_sigma_single(response, i_cell)

    fig1, ax1 = plt.subplots()
    ax1.scatter(response_mustar,response_sigma)
    for i, variable in enumerate(variables):
        ax1.annotate(variable.get_name(), xy=(response_mustar[i], \
                     response_sigma[i]), xytext=(5,5), \
                     textcoords="offset points")

    ax1.set_xlabel(r'$\mu^{\ast}$')
    ax1.set_ylabel(r'$\sigma$')
    ax1.set_xlim(left=0.0)
    ax1.set_ylim(bottom=0.0)
    ax1.set_title(response_name)
    ax1.set_aspect('auto')
    ax1.yaxis.set_major_formatter(ScalarFormatter())
    ax1.yaxis.major.formatter._useMathText = True
    ax1.yaxis.set_minor_locator(AutoMinorLocator(5))
    ax1.xaxis.set_minor_locator(AutoMinorLocator(5))
    ratio = 1.0
    ax1.set_aspect(1.0/ax1.get_data_ratio()*ratio)
    ax1.xaxis.set_tick_params(top='off', direction='out', width=0)
    ax1.yaxis.set_tick_params(right='off', direction='out', width=0)
    ax1.xaxis.set_tick_params(direction='out', width=1, length=5)
    ax1.yaxis.set_tick_params(direction='out', width=1, length=5)

def plot_mu(response, variables):
    exit()

def plot_mustar(response, variables):
    exit()

def plot_sigma(response, variables):
    exit()