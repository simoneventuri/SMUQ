from utilities.variable import Variable
from utilities.response import Response
import utilities.general_utilities as gu
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.ticker import AutoMinorLocator
from matplotlib.ticker import ScalarFormatter

ncolumns = 4

def get_variance(response):
    top_directory = gu.get_top_directory()
    nb_coords = response.get_nb_coords()
    response_label = response.get_label()
    variance = np.zeros(nb_coords)
    variance_coord = np.zeros((1,))
    for i_coord in range(0,nb_coords):
        filename = top_directory + '/analysis/' + response_label + '/cell' \
                   + str(i_coord + 1) + '/variance.dat'
        variance_coord = np.genfromtxt(filename, dtype=float)
        variance[i_coord] = variance_coord
    return np.nan_to_num(variance)

def get_variance_single(response,i_cell):
    top_directory = gu.get_top_directory()
    response_label = response.get_label()
    filename = top_directory + '/analysis/' + response_label \
               + '/cell' + str(i_cell + 1) + '/variance.dat'
    variance = np.genfromtxt(filename, dtype=float)
    return np.nan_to_num(variance)

def get_sobol_indices(response):
    top_directory = gu.get_top_directory()
    nb_coords = response.get_nb_coords()
    response_label = response.get_label()
    filename = top_directory + '/analysis/' + response_label \
               + '/cell1/sobol_total.dat'
    nb_variables = gu.file_len(filename)
    indices = np.zeros((nb_coords,nb_variables))
    indices_coord = np.zeros((nb_variables,))
    for i_coord in range(0,nb_coords):
        filename = top_directory + '/analysis/' + response_label + '/cell' \
                   + str(i_coord + 1) + '/sobol_total.dat'
        indices_coord = np.genfromtxt(filename, dtype=float)
        indices[i_coord,:] = indices_coord[:]
    return np.nan_to_num(indices)

def get_sobol_indices_single(response,i_cell):
    top_directory = gu.get_top_directory()
    response_label = response.get_label()
    filename = top_directory + '/analysis/' + response_label \
               + '/cell' + str(i_cell + 1) + '/sobol_total.dat'
    indices = np.genfromtxt(filename, dtype=float)
    return np.nan_to_num(indices)

def plot_sobol_indices_single(response, i_cell, variables):
    response_name = response.get_name()
    response_indices = get_sobol_indices_single(response, i_cell)

    fig1, ax1 = plt.subplots()

#    mng = plt.get_current_fig_manager()
#    mng.resize(*mng.window.maxsize())

    variable_names = []
    for variable in variables:
        variable_names.append(variable.get_name())

    variance = get_variance_single(response, i_cell)

    ax1.barh(response_indices, tick_label=variable_names, align='center')
    ax1.invert_yaxis()  # labels read top-to-bottom
    ax1.set_xlabel(r'$S_T$, Var=' + str(variance))
    ax1.yaxis.set_major_formatter(ScalarFormatter())
    ax1.yaxis.set_minor_locator(AutoMinorLocator(5))
    ax1.xaxis.set_minor_locator(AutoMinorLocator(5))
    ratio = 1.0
    ax1.set_aspect(1.0/ax1.get_data_ratio()*ratio)

def plot_sobol_indices(response, variables):
    response_name = response.get_name()
    response_indices = get_sobol_indices(response)
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
    ax2 = ax1.twinx()

#    mng = plt.get_current_fig_manager()
#    mng.resize(*mng.window.maxsize())

    for i, variable in enumerate(variables):
        ax1.plot(x, response_indices[:,i],\
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

    variance = get_variance(response)
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
