import os
import sys
import utilities.sobol_utilities as su
import utilities.general_utilities as gu
import utilities.variable as vc
import utilities.response as rc
from utilities.variable import Variable
from utilities.response import Response
import matplotlib as mpl
import matplotlib.pyplot as plt

gu.set_plot_options()

top_directory = gu.get_top_directory()
postprocess_directory = top_directory + '/postprocessing'

responses = rc.get_responses()
variables = vc.get_variables()

for response in responses:
    if (response.get_nb_coords() > 1):
        su.plot_sobol_indices(response, variables)
    else:
        su.plot_sobol_indices_single(response,0,variables)
plt.show()