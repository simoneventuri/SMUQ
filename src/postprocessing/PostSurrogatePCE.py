import os
import sys
import utilities.pce_utilities as pu
import utilities.general_utilities as gu
import utilities.variable as vc
import utilities.response as rc
from utilities.variable import Variable
from utilities.response import Response
import matplotlib as mpl
import matplotlib.pyplot as plt
import shutil

gu.set_plot_options()

top_directory = gu.get_top_directory()

postprocess_dir = top_directory + '/postprocessing'
if os.path.isdir(postprocess_dir):
    shutil.rmtree(postprocess_dir) 
os.mkdir(postprocess_dir)

responses = rc.get_responses()
variables = vc.get_variables()

for response in responses:
    response_dir = postprocess_dir + '/' + response.get_label()
    os.mkdir(response_dir)
    if (response.get_nb_coords() > 1):
        pu.plot_sobol_total_indices(response, variables)
        gu.maximize_figure()
        plt.savefig(response_dir + '/sobol_total.png')
        plt.close()

        pu.plot_sobol_first_indices(response, variables)
        gu.maximize_figure()
        plt.savefig(response_dir + '/sobol_first.png')
        plt.close()

        pu.plot_cv_variance(response)
        gu.maximize_figure()
        plt.savefig(response_dir + '/cv_variance.png')
        plt.close()
    else:
        pu.plot_sobol_total_indices_single(response, 1, variables)
        gu.maximize_figure()
        plt.savefig(response_dir + '/sobol_total.png')
        plt.close()

        pu.plot_sobol_first_indices_single(response, 1, variables)
        gu.maximize_figure()
        plt.savefig(response_dir + '/sobol_first.png')
        plt.close()
        
