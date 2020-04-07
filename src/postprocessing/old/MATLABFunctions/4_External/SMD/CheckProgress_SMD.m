function i_figure = CheckProgress_SMD(PlottingFormat, i_figure, data_scale_plot, x_names, y_names, plot_y_data, ...
                                      plot_y_start, plot_y_current, plot_y_prop,  x_data, y_data, ...
                                      x_start, y_start, x_current, y_current, x_prop, y_prop)
 
    figure(i_figure)
    
    if plot_y_data == 1
        
        if data_scale_plot == 'lin'
            h1=plot(x_data,y_data,'go-','LineWidth',5,'MarkerSize',8);
        elseif data_scale_plot == 'log'
            h1=semilogy(x_data,y_data,'go-','LineWidth',5,'MarkerSize',8);
        end
        hold on
            
    end
    
    if plot_y_start == 1
        
        if data_scale_plot == 'lin'
            h2=plot(x_start,y_start,'bo-','LineWidth',5,'MarkerSize',8);
        elseif data_scale_plot == 'log'
            h2=semilogy(x_start,y_start,'bo-','LineWidth',5,'MarkerSize',8);
        end
        hold on
        
    end
    
    if plot_y_current == 1
        
        if data_scale_plot == 'lin'
            h3=plot(x_current,y_current,'ko-','LineWidth',5,'MarkerSize',8);
        elseif data_scale_plot == 'log'
            h3=semilogy(x_current,y_current,'ko-','LineWidth',5,'MarkerSize',8);
        end
        hold on
        
    end
    
    if plot_y_prop == 1
        
        if data_scale_plot == 'lin'
            h4=plot(x_prop,y_prop,'ro-','LineWidth',5,'MarkerSize',8);
        elseif data_scale_plot == 'log'
            h4=semilogy(x_prop,y_prop,'ro-','LineWidth',5,'MarkerSize',8);
        end
        hold on
        
    end
    
    if plot_y_start == 1
        if PlottingFormat(1,:) == 'Visual'
            legend([h1,h2,h3,h4],{'Data','Start','Current','Proposed'});
        elseif PlottingFormat(1,:) == 'Papers'
            legend([h1,h2,h3,h4],{'Data','Start','Current','Proposed'},'Interpreter','latex');
        end 
    else 
        if PlottingFormat(1,:) == 'Visual'
            legend([h1,h3,h4],'Data','Current','Proposed');
        elseif PlottingFormat(1,:) == 'Papers'
            legend([h1,h3,h4],{'Data','Current','Proposed'},'Interpreter','latex');
        end   
    end 
    str_x = x_names;
    str_y = y_names;
    %str_title = [];
    if PlottingFormat(1,:) == 'Visual'
        xlabel(str_x);
        ylabel(str_y);    
        %title(str_title);
    elseif PlottingFormat(1,:) == 'Papers'
        xlabel(str_x,'Interpreter','latex');
        ylabel(str_y,'Interpreter','latex'); 
        %title(str_title);   
    end
    if PlottingFormat(1,:) == 'Visual'
        set(gca,'FontSize',30, 'FontName','PT Sans');
    elseif PlottingFormat(1,:) == 'Papers'
        set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','TickLabelInterpreter','latex');
        set(gcf, 'PaperPositionMode', 'auto');
    end 
    i_figure=i_figure+1;
    
end