function i_figure = plot_Sobol_Indices_fun(PlottingFormat, i_figure, save_fig, plot_absc_min, plot_absc_max, ...
                    x_names, y_names, x_nodes, sobol_indices, nb_dim, theta_colors, theta_names,...
                    indices_case, i_sets)
                
space = {' '};

legend_names = {};
for i = 1:size(theta_names,1)
    legend_names(i) = {strcat('$',theta_names(i,:),'$')};
end

figure(i_figure)
hold on

for i = 1:nb_dim
    plot(x_nodes,sobol_indices(:,i),'Color',theta_colors(i,:),'LineWidth',4)
end

hold off

x_str = x_names;

if PlottingFormat(1,:) == 'Visual'
    if strcmp(indices_case,'f')
        y_str = 'S_i';
        title_str = strcat('S_i Between Parameters and', space(1), y_names(1,:), 'for Set', space(1), num2str(i_sets));
    elseif strcmp(indices_case, 't')
        y_str = 'S_{Ti}';
        title_str = strcat('S_{Ti} Between Parameters and', space(1), y_names(1,:), 'for Set', space(1), num2str(i_sets));
    end
    yh = ylabel(y_str);
    xh = xlabel(x_str);
    th = title(title_str);
    lh = legend(theta_names,'Location','eastoutside');
    set(lh,'FontSize',24)
elseif PlottingFormat(1,:) == 'Papers'
    if strcmp(indices_case,'f')
        y_str = '$$S_i$$';
        title_str = strcat('$$S_i$$ Between Parameters and', space(1), y_names(1,:), 'for Set', space(1), num2str(i_sets));
    elseif strcmp(indices_case, 't')
        y_str = '$$S_{Ti}$$';
        title_str = strcat('$$S_{Ti}$$ Between Parameters and', space(1), y_names(1,:), 'for Set', space(1), num2str(i_sets));
    end
    yh = ylabel(y_str,'Interpreter','latex');
    xh = xlabel(x_str,'Interpreter','latex');
    th = title(title_str,'Interpreter','latex');
    lh = legend(legend_names,'Interpreter','latex','Location','eastoutside');
    set(lh,'FontSize',24)
    
end

grid on
set(gca,'Xlim',[plot_absc_min plot_absc_max],'Ylim',[0 1])
if PlottingFormat(1,:) == 'Visual'
    set(gca,'FontSize',30, 'FontName','PT Sans')
    set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
elseif PlottingFormat(1,:) == 'Papers'
    set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
    set(gcf,'PaperPositionMode','auto');
    set(gcf,'Renderer','painters')
end 
grid off
pbaspect([1.5 1 1])
    
if save_fig == 1
    if strcmp(indices_case,'f')
        str_save_fig = [strcat('./fig/Sobol/first_order_',num2str(i_sets),'.fig')];
    elseif strcmp(indices_case, 't')
        str_save_fig = [strcat('./fig/Sobol/total_order_',num2str(i_sets),'.fig')];
    end
    savefig(i_figure, str_save_fig);
end

i_figure = i_figure + 1;




theta_contribution_sum = sum(sobol_indices,2);

theta_contribution_ratio = zeros(size(sobol_indices,1),nb_dim);
for i = 1:size(sobol_indices,1)
    theta_contribution_ratio(i,:) = sobol_indices(i,:) / theta_contribution_sum(i) * 100;
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
if PlottingFormat(1,:) == 'Visual'
    y_str = 'Contribution %';
    if strcmp(indices_case,'f')
        title_str = strcat('S_i Ratios for', space(1), y_names(1,:), space(1), 'for Set', space(1), num2str(i_sets));
    elseif strcmp(indices_case, 't')
        title_str = strcat('S_{Ti} Ratios for', space(1), y_names(1,:), space(1), 'for Set', space(1), num2str(i_sets));
    end
    yh = ylabel(y_str);
    xh = xlabel(x_str);
    th = title(title_str);
    lh = legend(flipud(theta_names),'Location','eastoutside');
    set(lh,'FontSize',24)
elseif PlottingFormat(1,:) == 'Papers'
    y_str = 'Contribution \%';
    if strcmp(indices_case,'f')
        title_str = strcat('$$S_i$$ Ratios for', space(1), y_names(1,:), space(1), 'for Set', space(1), num2str(i_sets));
    elseif strcmp(indices_case, 't')
        title_str = strcat('$$S_{Ti}$$ Ratios for', space(1), y_names(1,:), space(1), 'for Set', space(1), num2str(i_sets));
    end
    yh = ylabel(y_str,'Interpreter','latex');
    xh = xlabel(x_str,'Interpreter','latex');
    th = title(title_str,'Interpreter','latex');
    lh = legend(flip(legend_names),'Interpreter','latex','Location','eastoutside');
    set(lh,'FontSize',24)
end

grid on
set(gca,'Xlim',[plot_absc_min plot_absc_max],'Ylim',[0 100])
if PlottingFormat(1,:) == 'Visual'
    set(gca,'FontSize',30, 'FontName','PT Sans')
    set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
elseif PlottingFormat(1,:) == 'Papers'
    set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
    set(gcf,'PaperPositionMode', 'auto');
    set(gcf,'Renderer','painters')
end 
grid off
pbaspect([1.5 1 1])

if save_fig == 1
    if strcmp(indices_case,'f')
        str_save_fig = [strcat('./fig/Sobol/first_order_contribution_',num2str(i_sets),'.fig')];
    elseif strcmp(indices_case, 't')
        str_save_fig = [strcat('./fig/Sobol/total_order_contribution_',num2str(i_sets),'.fig')];
    end
    savefig(i_figure, str_save_fig);
end

i_figure=i_figure+1;
                
end
