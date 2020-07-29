import os
import sys
from cycler import cycler
import matplotlib as mpl
import matplotlib.pyplot as plt
import time

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

def maximize_figure():
    # source: https://stackoverflow.com/questions/12439588/how-to-maximize-a-plt-show-window-using-python
    backend = plt.get_backend()
    mng = plt.get_current_fig_manager()
    if backend == "wxAgg":
        mng.frame.Maximize(True)
    elif backend == "TkAgg":
        if system() == "win32":
            mng.window.state('zoomed')  # This is windows only
        else:
            mng.resize(*cfm.window.maxsize())
    elif backend == 'QT4Agg':
        mng.window.showMaximized()
    elif callable(getattr(mng, "full_screen_toggle", None)):
        if not getattr(mng, "flag_is_max", None):
            mng.full_screen_toggle()
            mng.flag_is_max = True
    else:
        raise RuntimeError("maximize_figure() is not implemented for current backend:", backend)
    plt.pause(0.05)