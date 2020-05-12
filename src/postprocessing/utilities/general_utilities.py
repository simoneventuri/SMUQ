import os
import sys
from cycler import cycler
import matplotlib as mpl

def file_len(filename):
    with open(filename) as file_object:
        for i, l in enumerate(file_object):
            pass
    return i + 1

def get_top_directory():
    top_directory = os.getcwd()
    return top_directory.strip()

def set_plot_options():
    params = {
          'xtick.labelsize': 16,\
          'ytick.labelsize': 16,\
          'font.size': 15,\
          'figure.autolayout': True,\
          'axes.titlesize' : 24,\
          'axes.labelsize' : 20,\
          'lines.linewidth' : 3,\
          'lines.markersize' : 6,\
          'legend.fontsize': 13,\
          'xtick.top': False,\
          'ytick.right': False,\
          'xtick.direction': 'out',\
          'xtick.major.width': 1,\
          'xtick.major.size': 5,\
          'xtick.minor.visible': True,\
          'ytick.direction': 'out',\
          'ytick.major.width': 1,\
          'ytick.major.size': 5,\
          'ytick.minor.visible': True,\
          'text.usetex': False,\
          'axes.formatter.use_mathtext': True,\
          'axes.prop_cycle': (cycler(marker=['','x','o','v','^','s','+',\
                              '*','8'])\
                              *cycler(linestyle=['-', '--', '-.',':']) \
                              *cycler(color=['b','g','r','c','m','y','k']))
          }

    mpl.rcParams.update(params)
