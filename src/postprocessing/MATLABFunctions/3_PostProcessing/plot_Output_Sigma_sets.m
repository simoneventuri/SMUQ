function [ i_figure ] = plot_Output_Sigma_sets(PlottingFormat, i_figure, save_fig, ...
            plot_absc_min, plot_absc_max, ...
            x_names, y_names, x_nodes, like_node_sets, y_variance)

nb_sets = size(like_node_sets,1) - 1;

for i_sets = 1:nb_sets
    
    l_start = like_node_sets(i_sets)+1;
    l_end = like_node_sets(i_sets+1);

    i_figure = plot_Output_Sigma_fun(PlottingFormat, i_figure, save_fig, plot_absc_min, plot_absc_max, x_names, y_names(i_sets,:), ...
                                  x_nodes(l_start:l_end), y_variance(l_start:l_end), i_sets);
                              
end

end

