function [ i_figure ] = plot_Output_Sigma_fun(PlottingFormat, i_figure, save_fig, plot_absc_min, plot_absc_max, x_names, y_names, ...
                                  x_nodes, y_variance, i_sets)

space = {' '};
                              
figure(i_figure)

plot(x_nodes,sqrt(y_variance), 'r','LineWidth',3)

x_str = x_names;
y_str = 'Output Standard Deviation';
title_str = strcat('Standard Deviation in the Output of', space(1), strtrim(y_names), space(1),'for Set',space(1),num2str(i_sets));

if PlottingFormat(1,:) == 'Visual'
    ylabel(y_str)
    xlabel(x_str)
    title(title_str)
elseif PlottingFormat(1,:) == 'Papers'
    ylabel(y_str,'Interpreter','latex')
    xlabel(x_str,'Interpreter','latex')
    title(title_str,'Interpreter','latex')
end

set(gca,'Xlim',[plot_absc_min plot_absc_max])
if PlottingFormat(1,:) == 'Visual'
    set(gca,'FontSize',30, 'FontName','PT Sans')
    set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
elseif PlottingFormat(1,:) == 'Papers'
    set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
    set(gcf, 'PaperPositionMode', 'auto');
    set(gcf,'Renderer','painters')
end 
grid off
pbaspect([1.5 1 1])

if save_fig == 1
    str_save_fig = [strcat('./fig/Variation/sd_',num2str(i_sets),'.fig')];
    savefig(i_figure, str_save_fig);
end

xlim([plot_absc_min plot_absc_max])

i_figure = i_figure + 1;


end

