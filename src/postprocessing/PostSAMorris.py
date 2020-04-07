import os
import sys
import utilities.morris_utilities as mu
import utilities.general_utilities as gu
import utilities.variable as vc
import utilities.response as rc
from utilities.variable import Variable
from utilities.response import Response
import matplotlib as mpl
import matplotlib.pyplot as plt

params = {
    'xtick.labelsize': 16,\
    'ytick.labelsize': 16,\
    'font.size': 15,\
    'figure.autolayout': True,\
#    'figure.figsize': 7.2,4.45
    'axes.titlesize' : 24,\
    'axes.labelsize' : 20,\
    'lines.linewidth' : 2,\
    'lines.markersize' : 6,\
    'legend.fontsize': 13
   }
mpl.rcParams.update(params)

top_directory = gu.get_top_directory()
postprocess_directory = top_directory + '/postprocessing'

responses = rc.get_responses()
variables = vc.get_variables()

for response in responses:
    if ( response.get_nb_coords() > 1 ):
        mu.plot_mu(response, variables)
        mu.plot_mustar(response, variables)
        mu.plot_sigma(response, variables)
    else:
        mu.plot_mu_sigma_single(response,0,variables)
        mu.plot_mustar_sigma_single(response,0,variables)
plt.show()
