function i_figure = plot_Theta_Spaces_Hist_fun(PlottingFormat, i_figure, i, j, theta_scale, theta_names, A_theta, Theta_Spaces_pcolor_nbins)

    figure(i_figure)
    hist3(A_theta,[Theta_Spaces_pcolor_nbins,Theta_Spaces_pcolor_nbins])
    if theta_scale(i,:) == 'log'
        if theta_scale(j,:) == 'log'
            str_x = ['log_{10}(',strtrim(theta_names(i,:)),')'];
            str_y = ['log_{10}(',strtrim(theta_names(j,:)),')'];
        elseif theta_scale(j,:) == 'lin'
            str_x = ['log_{10}(',strtrim(theta_names(i,:)),')'];
            str_y = strtrim(theta_names(j,:));
        end
    elseif theta_scale(i,:) == 'lin'
        if theta_scale(j,:) == 'log'
            str_x = strtrim(theta_names(i,:));
            str_y = ['log_{10}(',strtrim(theta_names(j,:)),')'];
        elseif theta_scale(j,:) == 'lin'
            str_x = strtrim(theta_names(i,:));
            str_y = strtrim(theta_names(j,:));
        end
    end
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
