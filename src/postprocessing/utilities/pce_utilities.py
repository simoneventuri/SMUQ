from utilities.variable import Variable
from utilities.response import Response
import utilities.general_utilities as gu
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.ticker import AutoMinorLocator
from matplotlib.ticker import ScalarFormatter

ncolumns = 4

def get_indices(response, i_cell):
    top_directory = gu.get_top_directory()
    response_label = response.get_label()
    filename = top_directory + '/analysis/solver/' + response_label \
               + '/cell' + str(i_cell) + '/indices.dat'
    with open(filename) as file_object:
        text = []
        text.append(file_object.readline())
        vec = np.genfromtxt(text)
    nb_lines = gu.file_len(filename)
    indices = np.zeros((nb_lines,vec.shape[0]))
    indices = np.genfromtxt(filename, dtype=int)
    return indices

def get_coefficients(response, i_cell):
    top_directory = gu.get_top_directory()
    response_label = response.get_label()
    filename = top_directory + '/analysis/solver/' + response_label \
               + '/cell' + str(i_cell) + '/coefficients.dat'
    coefficients = np.genfromtxt(filename, dtype=float)
    return coefficients

def compute_first_sobol(coefficients, indices):
    if (indices.ndim > 1):
        nb_vars = indices.shape[1]
    else:
        nb_vars = 1

    sobol = np.zeros((nb_vars,))
    variance = compute_variance(coefficients, indices)
    if (not variance > 0.0):
        return sobol
    for i_coeff, coeff in enumerate(coefficients):

        if (np.count_nonzero(indices[i_coeff,]) != 1):
            continue

        for i_var in range(0, nb_vars):
            if (indices[i_coeff,i_var] != 0):
                sobol[i_var] += coeff**2

    sobol /= variance
    return sobol

def compute_total_sobol(coefficients, indices):
    if (indices.ndim > 1):
        nb_vars = indices.shape[1]
    else:
        nb_vars = 1
    sobol = np.zeros((nb_vars,))
    variance = compute_variance(coefficients, indices)
    if (not variance > 0.0):
        return sobol
    for i_coeff, coeff in enumerate(coefficients):

        for i_var in range(0, nb_vars):

            if (indices[i_coeff,i_var] != 0):
                sobol[i_var] += coeff**2

    sobol /= variance
    return sobol

def compute_variance(coefficients, indices):
    variance = 0.0
    shapelen = len(coefficients.shape)
    if (shapelen > 0):
        for i_coeff, coeff in enumerate(coefficients):
            if (np.count_nonzero(indices[i_coeff,:]) > 0):
                variance += coeff**2
    else:
        if (np.count_nonzero(indices[1,]) > 0):
            variance += coeff**2          
    return variance


def compute_mean(coefficients, indices):
    mean = 0.0
    shapelen = len(coefficients.shape)
    if (shapelen > 0):
        for i_coeff, coeff in enumerate(coefficients):
            if (np.count_nonzero(indices[i_coeff,:]) == 0):
                mean += coeff
    else:
        if (np.count_nonzero(indices[1,]) == 0):
            mean += coeff 
    return mean

def plot_sobol_indices_single(response, i_cell, variables):
    variable_names = []
    for variable in variables:
        variable_names.append(variable.get_name())

    response_name = response.get_name()
    response_indices = get_indices(response, i_cell)    
    response_coeffs = get_coefficients(response, i_cell)

    response_sobol_total = compute_total_sobol(response_coeffs, \
                                               response_indices)

    response_sobol_first = compute_first_sobol(response_coeffs, \
                                               response_indices)

    response_var = compute_variance(response_coeffs, response_indices)

    fig1, ax1 = plt.subplots()

    mng = plt.get_current_fig_manager()
    mng.resize(*mng.window.maxsize())

    ax1.barh(response_sobol_total, tick_label=variable_names, align='center')
    ax1.invert_yaxis()
    ax1.set_xlabel(r'$S_T$')
    ax1.yaxis.set_major_formatter(ScalarFormatter())
    ax1.yaxis.set_minor_locator(AutoMinorLocator(5))
    ax1.xaxis.set_minor_locator(AutoMinorLocator(5))
    ratio = 1.0
    ax1.set_aspect(1.0/ax1.get_data_ratio()*ratio)

    fig2, ax2 = plt.subplots()

    mng = plt.get_current_fig_manager()
    mng.resize(*mng.window.maxsize())

    ax2.barh(response_sobol_first, tick_label=variable_names, align='center')
    ax2.invert_yaxis()
    ax2.set_xlabel(r'$S_i$')
    ax2.yaxis.set_major_formatter(ScalarFormatter())
    ax2.yaxis.set_minor_locator(AutoMinorLocator(5))
    ax2.xaxis.set_minor_locator(AutoMinorLocator(5))
    ratio = 1.0
    ax2.set_aspect(1.0/ax2.get_data_ratio()*ratio)

def plot_sobol_indices(response, variables):
    response_name = response.get_name()
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

    nb_vars = len(variables)
    nb_coords = response.get_nb_coords()

    total_sobol = np.zeros((nb_coords,nb_vars))
    first_sobol = np.zeros((nb_coords,nb_vars))
    variance = np.zeros((nb_coords,))

    for i_cell in range(0,nb_coords):
        indices = get_indices(response, i_cell+1)
        coefficients = get_coefficients(response, i_cell+1)
        total_sobol[i_cell,:] = compute_total_sobol(coefficients, indices)
        first_sobol[i_cell,:] = compute_first_sobol(coefficients, indices)
        variance[i_cell] = compute_variance(coefficients, indices)

    fig1, ax1 = plt.subplots()
    ax2 = ax1.twinx()

    mng = plt.get_current_fig_manager()
    mng.resize(*mng.window.maxsize())

    for i, variable in enumerate(variables):
        ax1.plot(x, total_sobol[:,i],\
                 label=r'$'+variable.get_name()+r'$',\
                 markevery=0.1)

    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(r'$S_T$')
    ax1.set_xlim(left=x[0], right=x[xstop-1])
    ax1.set_ylim(bottom=0.0, top=1.0)
    ax1.legend(ncol=ncolumns)

    ax1.yaxis.set_major_formatter(ScalarFormatter())
    ax1.yaxis.set_minor_locator(AutoMinorLocator(5))
    ax1.xaxis.set_minor_locator(AutoMinorLocator(5))
    ratio = 1.0
    ax1.set(adjustable='box-forced', aspect=1.0/ax1.get_data_ratio()*ratio)

    ax2.plot(x, variance, color='pink', linestyle=':', marker='')

    ax2.set_ylabel('Variance', color='pink')
    ax2.set_xlim(left=x[0], right=x[xstop-1])
    ax2.set_ylim(bottom=0.0)

    ax2.spines["left"].set_position(("axes", -0.15))
    ax2.set_frame_on(True)
    ax2.patch.set_visible(False)
    for sp in ax2.spines.values():
        sp.set_visible(False)
    ax2.spines["left"].set_visible(True)
    ax2.yaxis.set_label_position('left')
    ax2.yaxis.set_ticks_position('left')
    ax2.spines['left'].set_color('pink')
    ax2.tick_params(colors='pink')
    ax2.set(adjustable='box-forced', aspect=1.0/ax2.get_data_ratio()*ratio)

    fig2, ax3 = plt.subplots()
    
    mng = plt.get_current_fig_manager()
    mng.resize(*mng.window.maxsize())

    for i, variable in enumerate(variables):
        ax3.plot(x, first_sobol[:,i],\
                 label=r'$'+variable.get_name()+r'$',\
                 markevery=0.1)

    ax3.set_xlabel(xlabel)
    ax3.set_ylabel(r'$S_i$')
    ax3.set_xlim(left=x[0], right=x[xstop-1])
    ax3.set_ylim(bottom=0.0, top=1.0)
    ax3.legend(ncol=ncolumns)

    ax3.yaxis.set_major_formatter(ScalarFormatter())
    ax3.yaxis.set_minor_locator(AutoMinorLocator(5))
    ax3.xaxis.set_minor_locator(AutoMinorLocator(5))
    ratio = 1.0
    ax3.set(adjustable='box-forced', aspect=1.0/ax3.get_data_ratio()*ratio)