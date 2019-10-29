function i_figure = plot_Theta_Spaces_Pcolor_fun(PlottingFormat, i_figure, i, j, theta_names, A_x_Space, A_y_Space, A_hist)

    figure(i_figure)
    pcolor(A_x_Space,A_y_Space,A_hist);
    str_x = strtrim(theta_names(i,:));
    str_y = strtrim(theta_names(j,:));
    str_title = ['Posterior from ',theta_names(i,:),' - ',theta_names(j,:),' Sampling Histogram'];
    if PlottingFormat(1,:) == 'Visual'
        xlabel(str_x);
        ylabel(str_y);    
        title(str_title);
    elseif PlottingFormat(1,:) == 'Papers'
        xlabel(str_x,'Interpreter','latex');
        ylabel(str_y,'Interpreter','latex');    
        title(str_title,'Interpreter','latex');
    end
    if PlottingFormat(1,:) == 'Visual'
        set(gca,'FontSize',30, 'FontName','PT Sans')
        set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
    elseif PlottingFormat(1,:) == 'Papers'
        set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
        set(gcf, 'PaperPositionMode', 'auto');
    end                
    i_figure=i_figure+1;

end
