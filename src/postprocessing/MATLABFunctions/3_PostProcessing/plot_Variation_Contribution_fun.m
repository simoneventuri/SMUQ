function i_figure = plot_Variation_Contribution_fun(PlottingFormat, i_figure, save_fig, plot_absc_min, plot_absc_max, ...
                                                    x_names, y_names, x_nodes, pearson_correlation, ...
                                                    theta_variance, y_variance, UnifMin, UnifMax, nb_dim, ...
                                                    theta_colors, theta_names, i_sets)
space = {' '};

figure(i_figure)
hold on
for i = 1:nb_dim
    prior_range(i) = UnifMax(i) - UnifMin(i);
    theta_contribution(:,i) = (pearson_correlation(:,i)).* sqrt(y_variance)/sqrt(theta_variance(i)) * prior_range(i);
    plot(x_nodes,theta_contribution(:,i),'Color',theta_colors(i,:),'LineWidth',4)
end

hold off

legend_names = {};
for i = 1:size(theta_names,1)
    legend_names(i) = {strcat('$',theta_names(i,:),'$')};
end

x_str = x_names;
y_str = 'Contribution';
title_str = strcat(strtrim(y_names), space(1), 'Variation Contribution Per Parameter');
if PlottingFormat(1,:) == 'Visual'
    yh = ylabel(y_str);
    xh = xlabel(x_str);
    th = title(title_str);
    lh = legend(theta_names,'Location','eastoutside');
    set(lh,'FontSize',24)
elseif PlottingFormat(1,:) == 'Papers' 
    yh = ylabel(y_str,'Interpreter','latex');
    xh = xlabel(x_str,'Interpreter','latex');
    th = title(title_str,'Interpreter','latex');
    lh = legend(legend_names,'Location','eastoutside','Interpreter','latex');
    set(lh,'FontSize',24)
end

grid on
set(gca,'Xlim',[plot_absc_min plot_absc_max])
pbaspect([1.5 1 1])
if PlottingFormat(1,:) == 'Visual'
    set(gca,'FontSize',30, 'FontName','PT Sans')
    set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
elseif PlottingFormat(1,:) == 'Papers'
    set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
    set(gcf, 'PaperPositionMode', 'auto');
    set(gcf, 'Renderer', 'painters')
end 
if save_fig == 1
   str_save_fig = ['./fig/Correlations/Variation_Contribution.fig'];
   savefig(i_figure, str_save_fig);
end

i_figure = i_figure + 1;




for i = 1:nb_dim
    theta_contribution(:,i) = (abs(pearson_correlation(:,i))).* sqrt(y_variance)/sqrt(theta_variance(i)) * prior_range(i);
end

theta_contribution_sum = sum(theta_contribution,2);

for i = 1:size(theta_contribution,1)
    theta_contribution_ratio(i,:) = theta_contribution(i,:) / theta_contribution_sum(i) * 100;
end

for i = 1:nb_dim
   if (i == 1)
      summing_ratios(:,i) = theta_contribution_ratio(:,i);
   else
      summing_ratios(:,i) = summing_ratios(:,i-1) + theta_contribution_ratio(:,i);
   end
end


figure(i_figure)
hold on
for i = nb_dim:-1:1
   area(x_nodes,summing_ratios(:,i),'FaceColor',theta_colors(i,:))
end
hold off

x_str = x_names;
title_str = strcat(strtrim(y_names), space(1), 'Variation Contribution Per Parameter for Set', space(1),num2str(i_sets));
if PlottingFormat(1,:) == 'Visual'
    y_str = 'Contribution %';
    yh = ylabel(y_str);
    xh = xlabel(x_str);
    th = title(title_str);
    lh = legend(flipud(theta_names),'Location','eastoutside');
    set(lh,'FontSize',24)
elseif PlottingFormat(1,:) == 'Papers'
    y_str = 'Contribution \%';
    yh = ylabel(y_str,'Interpreter','latex');
    xh = xlabel(x_str,'Interpreter','latex');
    th = title(title_str,'Interpreter','latex');
    lh = legend(flip(legend_names),'Location','eastoutside','Interpreter','latex');
    set(lh,'FontSize',24)
end

grid off
pbaspect([1.5 1 1])
set(gca,'Xlim',[plot_absc_min plot_absc_max],'Ylim',[0 100])
if PlottingFormat(1,:) == 'Visual'
    set(gca,'FontSize',30, 'FontName','PT Sans')
    set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
elseif PlottingFormat(1,:) == 'Papers'
    set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
    set(gcf, 'PaperPositionMode', 'auto');
    set(gcf, 'Renderer', 'painters');
end 

if save_fig == 1
   str_save_fig = [strcat('./fig/Correlations/var_ratio_',num2str(i_sets),'.fig')];
   savefig(i_figure, str_save_fig);
end
i_figure=i_figure+1;
    
end

