function i_figure = plot_Output_Variation_Pcolor_fun_sets(PlottingFormat, i_figure, save_fig, ...
            plot_absc_min, plot_absc_max, plot_data_min, plot_data_max, data_scale, ...
            x_names, y_names, x_nodes,  y_Space_eff, y_Space, Hist_Matrix, hist_x_nodes, hist_node_sets);

nb_sets = size(hist_node_sets,1) - 1;

for i_sets = 1:nb_sets
    
    h_start = hist_node_sets(i_sets)+1;
    h_end = hist_node_sets(i_sets+1);

    i_figure = plot_Output_Variation_Pcolor_fun(PlottingFormat, i_figure, save_fig, ...
            plot_absc_min, plot_absc_max, plot_data_min, plot_data_max, data_scale, ...
            x_names, y_names(i_sets,:), hist_x_nodes(h_start:h_end),  y_Space_eff, y_Space, Hist_Matrix(:,h_start:h_end),i_sets);
        
end

end
