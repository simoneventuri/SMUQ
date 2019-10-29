import matplotlib.pyplot as plt
import numpy as np
import sys
import os
import PCE

base_path = os.path.dirname(__file__)
rel_path = os.path.abspath(os.path.join(base_path, "solver"))

nbmanager_path = os.path.abspath(os.path.join(rel_path,"nbmanagers.dat"))

for line in open(nbmanager_path, "r"):
    nbmanagers = line.rstrip()
    nbmanagers = int(nbmanagers)

list_managers = []
for x in range(nbmanagers):
    index = x + 1
    manager_name = "manager_" + str(index)
    list_managers.append(manager_name)

labels_list = [r'$\mathrm{k}_{\mathrm{cond,virgin}}$',r'$\mathrm{k}_{\mathrm{cond,char}}$',r'$\mathrm{k}_{\mathrm{rad,virgin}}$',r'$\mathrm{k}_{\mathrm{rad,char}}$', r'$\mathrm{A}_{\mathrm{m,r1}}$', r'$\mathrm{A}_{\mathrm{m,r2}}$', r'$\mathrm{m}_{\mathrm{m,r1}}$', r'$\mathrm{m}_{\mathrm{m,r2}}$']
for manager in list_managers:
    fig, ax1 = plt.subplots(figsize = (10,10))
    ax1.set_xlabel("Time (s)",fontsize=30)
    ax1.set_ylabel(r'$\mathrm{S}_{\mathrm{i,t}}$',fontsize=30)

    list_cells = PCE.get_cells(rel_path, manager)
    abscissa = PCE.get_abscissa(rel_path, manager)
    list_params = PCE.get_params(rel_path, manager)
    for param in list_params:
        index_list = []
        for cell in list_cells:
            certain_val = PCE.indices(rel_path, manager, cell, param)
            index_list.append(certain_val)
        param_name = labels_list[int(param)-1]
        ax1.plot(abscissa, index_list, label = param_name, linewidth = 4)
    ax1.tick_params(axis='y', labelcolor = 'black')
    # Shrink current axis by 20%
    box = ax1.get_position()
    ax1.set_position([box.x0, box.y0, box.width * 0.7, box.height])
    ax1.margins(x=0)
    ax1.margins(y=0)
    ax1.spines['top'].set_visible(False)
    ax1.spines['right'].set_visible(False)
    ax1.minorticks_on()
    ax1.tick_params(axis='x', labelsize=30)
    ax1.tick_params(axis='y', labelsize=30)
    ax2 = ax1.twinx()
    ax2.margins(x=0)
    ax2.margins(y=0)
    ax2.spines['top'].set_visible(False)
    ax2.spines['right'].set_color('magenta')
    ax2.minorticks_on()

    variance_list = []
    for cell in list_cells:
        variance_list.append(PCE.get_variance(rel_path, manager, cell))

    ax2.set_ylabel(r'Variance  ($\mathrm{K}^2$)', fontsize=30)
    ax2.plot(abscissa, variance_list, '--', color = 'magenta', label = r'Variance')
    ax2.tick_params(axis='y', colors = 'magenta', labelsize=30)
    # Shrink current axis by 20%
    box = ax2.get_position()
    ax2.set_position([box.x0, box.y0, box.width * 0.7, box.height])
    # Put a legend to the right of the current axis
    lines, labels = ax1.get_legend_handles_labels()
    lines2, labels2 = ax2.get_legend_handles_labels()
    ax2.legend(lines + lines2, labels + labels2, loc='center left', bbox_to_anchor=(1.15, 0.5), fontsize=25)
plt.rcParams['axes.xmargin'] = 0
plt.rcParams['axes.ymargin'] = 0
plt.show()

#if nbmanagers == 1:
#    for l in list_managers:
#        list_cells = PCE.get_cells(rel_path, l)
#    for cell in list_cells:
#        dict = PCE.indices(rel_path, l, cell)
#    PCE.pieplot(dict)
