function i_figure = plot_Correlations_fun(PlottingFormat, i_figure, save_fig, plot_absc_min, plot_absc_max, ...
                    x_names, y_names, x_nodes, pearson_correlation, nb_dim, theta_colors, theta_names, i_sets)
                
space = {' '};

figure(i_figure)

hold on
for i = 1:nb_dim
    plot(x_nodes,pearson_correlation(:,i),'Color',theta_colors(i,:),'LineWidth',4)
end
hold off

legend_names = {};
for i = 1:size(theta_names,1)
    legend_names(i) = {strcat('$',theta_names(i,:),'$')};
end

if PlottingFormat(1,:) == 'Visual'
    x_str = x_names;
    y_str = 'r_{x,y}';
    title_str = strcat('r_{x,y} Between Parameters and', space(1), strtrim(y_names), space(1), 'for Set', space(1), num2str(i_sets));
    yh = ylabel(y_str);
    xh = xlabel(x_str);
    th = title(title_str);
    lh = legend(theta_names,'Location','eastoutside');
    set(lh,'FontSize',24)
elseif PlottingFormat(1,:) == 'Papers'
    x_str = x_names;
    y_str = '$r_{x,y}$';
    title_str = strcat(' $r_{x,y}$ Between Parameters and', space(1), strtrim(y_names), space(1), 'for Set', space(1), num2str(i_sets));
    yh = ylabel(y_str,'Interpreter','latex');
    xh = xlabel(x_str,'Interpreter','latex');
    th = title(title_str,'Interpreter','latex');
    lh = legend(legend_names,'Location','eastoutside','Interpreter','latex');
    set(lh,'FontSize',24)
end
grid on
set(gca,'Xlim',[plot_absc_min plot_absc_max], 'Ylim', [-1 1])
grid off
pbaspect([1.5 1 1])
if PlottingFormat(1,:) == 'Visual'
    set(gca,'FontSize',30, 'FontName','PT Sans')
    set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
elseif PlottingFormat(1,:) == 'Papers'
    set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','TickLength', [.02 .01],'XMinorTick','on',...
        'YMinorTick', 'on','TickLabelInterpreter','latex','LineWidth',1);
    set(gcf, 'PaperPositionMode', 'auto');
    set(gcf, 'Renderer', 'painters')
end 
    
if save_fig == 1
   str_save_fig = [strcat('./fig/Correlations/correlations_',num2str(i_sets),'.fig')];
   savefig(i_figure, str_save_fig);
end
    
i_figure = i_figure + 1;
                
end
