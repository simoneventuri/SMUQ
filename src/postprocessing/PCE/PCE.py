import numpy as np
import matplotlib
import os
import itertools
from collections import Counter
from math import pi
import pandas as pd
from bokeh.palettes import Category20c
from bokeh.plotting import figure, show
from bokeh.transform import cumsum


def get_cells(file_path, manager_name):
    nbcell_path = os.path.abspath(os.path.join(file_path, manager_name, "nbcells.dat"))
    for line in open(nbcell_path, "r"):
        nbcells = line.rstrip()
        nbcells = int(nbcells)
    list_cells = []
    for x in range(nbcells):
        index = x + 1
        cell_name = "cell" + str(index)
        list_cells.append(cell_name)
    return list_cells;

def get_abscissa(file_path, manager_name):
    abscissa_path = os.path.abspath(os.path.join(file_path, manager_name, "abscissa.dat"))
    abscissa = []
    for line in open(abscissa_path, "r"):
        val = line.rstrip()
        val = float(val)
        abscissa.append(val)
    return abscissa;


def indices(file_path, manager_name, cell_name, index):
    coefficients_path = os.path.abspath(os.path.join(file_path, manager_name, cell_name, "coefficients.dat"))
    indices_path = os.path.abspath(os.path.join(file_path, manager_name, cell_name, "indices.dat"))
    indices_list = []
    coefficients_list = []
    for line in open(indices_path, "r"):
        entry = line.rstrip()
        indices_list.append(entry)
    for line in open(coefficients_path, "r"):
        entry = line.rstrip()
        coefficients_list.append(entry)
    return_index_value = total_index(indices_list, coefficients_list, index)
    return return_index_value;


def variance(indices_list, coefficients_list):
    check_header = indices_list[0].split()
    tot_variance = 0.0;
    if check_header[0] == "!header":
        parameters = int(check_header[1])
    else:
        parameters = len(check_header)
    for pos in range(len(indices_list)):
        values = indices_list[pos].split()
        if values[0] == "!header":
            continue
        dim = 0
        for val in values:
            if val == '0':
                dim = dim + 1
        if dim == parameters:
            continue
        elif dim != parameters:
            tot_variance = tot_variance + (float(coefficients_list[pos]) * float(coefficients_list[pos]))
    return tot_variance;

def pieplot(dictionary):
    x = Counter(dictionary)

    data = pd.DataFrame.from_dict(dict(x), orient='index').reset_index()
    data = data.rename(index=str, columns={0:'value', 'index':'index'})
    data['angle'] = data['value']/sum(x.values()) * 2*pi
    data['color'] = Category20c[len(x)]

    p = figure(plot_height=700, title="Indices", toolbar_location=None, tools="hover", tooltips=[("Index", "@index"),("Value", "@value")])

    p.wedge(x=0, y=1, radius=0.6, start_angle=cumsum('angle', include_zero=True), end_angle=cumsum('angle'), line_color="white", fill_color='color', legend='index', source=data)

    p.axis.axis_label=None
    p.axis.visible=False
    p.grid.grid_line_color = None

    show(p)
    return;

def get_params(file_path, manager_name):
    indices_path = os.path.abspath(os.path.join(file_path, manager_name, "cell1", "indices.dat"))
    indices_list = []
    for line in open(indices_path, "r"):
        entry = line.rstrip()
        break
    indices_list = entry.split()
    if indices_list[0] == "!header":
        num_params = int(indices_list[1])
    else:
        num_params = len(indices_list)
    return_indices = range(1, num_params + 1)
    return return_indices;

def total_index(indices_list, coefficients_list, index):
    tot_variance = variance(indices_list, coefficients_list)
    tot_index_value = 0.0
    for pos in range(len(indices_list)):
        current_set = indices_list[pos].split()
        if current_set[0] == "!header":
            continue;
        else:
            if current_set[index - 1] != '0':
                x = float(coefficients_list[pos])
                tot_index_value = tot_index_value + x * x
    tot_index_value = tot_index_value / tot_variance
    return tot_index_value;

def get_variance(file_path, manager_name, cell_name):
    coefficients_path = os.path.abspath(os.path.join(file_path, manager_name, cell_name, "coefficients.dat"))
    indices_path = os.path.abspath(os.path.join(file_path, manager_name, cell_name, "indices.dat"))
    indices_list = []
    coefficients_list = []
    for line in open(indices_path, "r"):
        entry = line.rstrip()
        indices_list.append(entry)
    for line in open(coefficients_path, "r"):
        entry = line.rstrip()
        coefficients_list.append(entry)
    return_var_value = variance(indices_list, coefficients_list)
    return return_var_value;
