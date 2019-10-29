function i_figure = plot_Variation_Contribution_fun_sets(PlottingFormat, i_figure, save_fig, plot_absc_min, plot_absc_max, ...
                                                    x_names, y_names, x_nodes, pearson_correlation, ...
                                                    theta_variance, y_variance, UnifMin, UnifMax, nb_dim, ...
                                                    theta_colors, theta_names, like_node_sets)

nb_sets = size(like_node_sets,1) - 1;

for i_sets = 1:nb_sets
    
    l_start = like_node_sets(i_sets)+1;
    l_end = like_node_sets(i_sets+1);

        i_figure = plot_Variation_Contribution_fun(PlottingFormat, i_figure, save_fig, plot_absc_min, plot_absc_max, ...
                                                    x_names, y_names(i_sets,:), x_nodes(l_start:l_end), pearson_correlation(l_start:l_end,:), ...
                                                    theta_variance, y_variance(l_start:l_end), UnifMin, UnifMax, nb_dim, ...
                                                    theta_colors, theta_names, i_sets);
                                                
end

end
