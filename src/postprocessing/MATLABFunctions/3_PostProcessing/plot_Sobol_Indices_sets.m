function i_figure = plot_Sobol_Indices_sets(PlottingFormat, i_figure, save_fig, plot_absc_min, plot_absc_max, ...
                    x_names, y_names, x_nodes, sobol_indices, nb_dim, theta_colors, theta_names, like_node_sets,...
                    indices_case)

nb_sets = size(like_node_sets,1) - 1;

for i_sets = 1:nb_sets
    
    l_start = like_node_sets(i_sets)+1;
    l_end = like_node_sets(i_sets+1);

    i_figure = plot_Sobol_Indices_fun(PlottingFormat, i_figure, save_fig, plot_absc_min, plot_absc_max, x_names, y_names(i_sets,:), ...
                                  x_nodes(l_start:l_end), sobol_indices(l_start:l_end,:), nb_dim, theta_colors, theta_names, ...
                                  indices_case,i_sets);
                              
end

end
