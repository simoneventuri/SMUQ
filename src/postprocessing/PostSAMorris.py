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
        mu.plot_mu(response, variables)
        gu.maximize_figure()
        plt.savefig(response_dir + '/mu.png')
        plt.close()

        mu.plot_mustar(response, variables)
        gu.maximize_figure()
        plt.savefig(response_dir + '/mustar.png')
        plt.close()

        mu.plot_sigma(response, variables)
        gu.maximize_figure()
        plt.savefig(response_dir + '/sigma.png')
        plt.close()
    else:
        mu.plot_mu_sigma_single(response,0,variables)
        gu.maximize_figure()
        plt.savefig(response_dir + '/mu_sigma.png')
        plt.close()

        mu.plot_mustar_sigma_single(response,0,variables)
        gu.maximize_figure()
        plt.savefig(response_dir + '/mustar_sigma.png')
        plt.close()