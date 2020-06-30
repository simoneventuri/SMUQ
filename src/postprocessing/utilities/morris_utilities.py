from utilities.variable import Variable
from utilities.response import Response
import utilities.general_utilities as gu
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.ticker import AutoMinorLocator
from matplotlib.ticker import ScalarFormatter

ncolumns = 4

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
        mustar_coord = np.genfromtxt(filename, dtype=float)
        mustar[i_coord,:] = np.nan_to_num(mustar_coord[:])
    return np.nan_to_num(mustar)

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
        mu_coord = np.genfromtxt(filename, dtype=float)
        mu[i_coord,:] = mu_coord[:]
    return np.nan_to_num(mu)

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
        sigma_coord = np.genfromtxt(filename, dtype=float)
        sigma[i_coord,:] = sigma_coord[:]
    return np.nan_to_num(sigma)

def get_mustar_single(response,i_cell):
    top_directory = gu.get_top_directory()
    response_label = response.get_label()
    filename = top_directory + '/analysis/' + response_label \
               + '/cell' + str(i_cell + 1) + '/mu_star.dat'
    mustar = np.genfromtxt(filename, dtype=float)
    return np.nan_to_num(mustar)

def get_mu_single(response,i_cell):
    top_directory = gu.get_top_directory()
    response_label = response.get_label()
    filename = top_directory + '/analysis/' + response_label \
               + '/cell' + str(i_cell + 1) + '/mu.dat'
    mu = np.genfromtxt(filename, dtype=float)
    return np.nan_to_num(mu)

def get_sigma_single(response,i_cell):
    top_directory = gu.get_top_directory()
    response_label = response.get_label()
    filename = top_directory + '/analysis/' + response_label \
               + '/cell' + str(i_cell + 1) + '/sigma.dat'
    sigma = np.genfromtxt(filename, dtype=float)
    return np.nan_to_num(sigma)

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
    #ax1.set_title(response_name)
    ax1.yaxis.set_major_formatter(ScalarFormatter())
    ax1.yaxis.set_minor_locator(AutoMinorLocator(5))
    ax1.xaxis.set_minor_locator(AutoMinorLocator(5))
    ratio = 1.0
    ax1.set_aspect(1.0/ax1.get_data_ratio()*ratio)

def plot_mustar_sigma_single(response, i_cell, variables):
    response_name = response.get_name()
    response_mustar = get_mustar_single(response, i_cell)
    response_sigma = get_sigma_single(response, i_cell)

    fig1, ax1 = plt.subplots()

#    mng = plt.get_current_fig_manager()
#    mng.resize(*mng.window.maxsize())

    ax1.scatter(response_mustar,response_sigma)
    for i, variable in enumerate(variables):
        ax1.annotate(variable.get_name(), xy=(response_mustar[i], \
                     response_sigma[i]), xytext=(5,5), \
                     textcoords="offset points")

    ax1.set_xlabel(r'$\mu^{\ast}$')
    ax1.set_ylabel(r'$\sigma$')
    ax1.set_xlim(left=0.0)
    ax1.set_ylim(bottom=0.0)
    #ax1.set_title(response_name)
    ax1.yaxis.set_major_formatter(ScalarFormatter())
    ax1.yaxis.set_minor_locator(AutoMinorLocator(5))
    ax1.xaxis.set_minor_locator(AutoMinorLocator(5))
    ratio = 1.0
    ax1.set_aspect(1.0/ax1.get_data_ratio()*ratio)

def plot_mu(response, variables):
    response_name = response.get_name()
    response_mu = get_mu(response)
    xstop = response.get_nb_coords()

    if (response.get_nb_coords_dim() > 1):
        print('Response "' + response_name + '" specifies non-1D coordinates' \
              + 'which current scripts do not natively support for ' \
              + 'Morris method post-processing')
        x = np.arange(start=1, stop=xstop, step=1)
        xlabel = 'Cell'
    else:
        x = response.get_coords()
        xlabel = response.get_coords_label(0)
        
    fig1, ax1 = plt.subplots()

#    mng = plt.get_current_fig_manager()
#    mng.resize(*mng.window.maxsize())

    for i, variable in enumerate(variables):
        ax1.plot(x, response_mu[:,i],\
                 label=r'$'+variable.get_name()+r'$',\
                 markevery=0.1)

    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(r'$\mu$')
    ax1.set_xlim(left=x[0], right=x[xstop-1])
    ax1.set_ylim(bottom=response_mu.min())
    #ax1.set_title(response_name)
    ax1.yaxis.set_major_formatter(ScalarFormatter())
    ax1.yaxis.set_minor_locator(AutoMinorLocator(5))
    ax1.xaxis.set_minor_locator(AutoMinorLocator(5))
    ratio = 1.0
    ax1.set_aspect(1.0/ax1.get_data_ratio()*ratio)
    ax1.legend(ncol=ncolumns)
    return

def plot_mustar(response, variables):

    response_name = response.get_name()
    response_mustar = get_mustar(response)
    xstop = response.get_nb_coords()

    if (response.get_nb_coords_dim() > 1):
        print('Response "' + response_name + '" specifies non-1D coordinates' \
              + 'which current scripts do not natively support for ' \
              + 'Morris method post-processing')
        x = np.arange(start=1, stop=xstop, step=1)
        xlabel = 'Cell'
    else:
        x = response.get_coords()
        xlabel = response.get_coords_label(0)
        
    fig1, ax1 = plt.subplots()

#    mng = plt.get_current_fig_manager()
#    mng.resize(*mng.window.maxsize())

    for i, variable in enumerate(variables):
        ax1.plot(x, response_mustar[:,i],\
                 label=r'$'+variable.get_name()+r'$',\
                 markevery=0.1)

    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(r'$\mu^{\ast}$')
    ax1.set_xlim(left=x[0], right=x[xstop-1])
    ax1.set_ylim(bottom=0.0)
    #ax1.set_title(response_name)
    ax1.yaxis.set_major_formatter(ScalarFormatter())
    ax1.yaxis.set_minor_locator(AutoMinorLocator(5))
    ax1.xaxis.set_minor_locator(AutoMinorLocator(5))
    ratio = 1.0
    ax1.set_aspect(1.0/ax1.get_data_ratio()*ratio)
    ax1.legend(ncol=ncolumns)
    return

def plot_sigma(response, variables):
    response_name = response.get_name()
    response_sigma = get_sigma(response)
    xstop = response.get_nb_coords()

    if (response.get_nb_coords_dim() > 1):
        print('Response "' + response_name + '" specifies non-1D coordinates' \
              + 'which current scripts do not natively support for ' \
              + 'Morris method post-processing')
        
        x = np.arange(start=1, stop=xstop, step=1)
        xlabel = 'Cell'
    else:
        x = response.get_coords()
        xlabel = response.get_coords_label(0)
        
    fig1, ax1 = plt.subplots()

#    mng = plt.get_current_fig_manager()
#    mng.resize(*mng.window.maxsize())

    for i, variable in enumerate(variables):
        ax1.plot(x, response_sigma[:,i],
                 label=r'$'+variable.get_name()+r'$',\
                 markevery=0.1)

    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(r'$\sigma$')
    ax1.set_xlim(left=x[0], right=x[xstop-1])
    ax1.set_ylim(bottom=0.0)
    #ax1.set_title(response_name)
    ax1.yaxis.set_major_formatter(ScalarFormatter())
    ax1.yaxis.set_minor_locator(AutoMinorLocator(5))
    ax1.xaxis.set_minor_locator(AutoMinorLocator(5))
    ratio = 1.0
    ax1.set_aspect(1.0/ax1.get_data_ratio()*ratio)
    ax1.legend(ncol=ncolumns)
    return
