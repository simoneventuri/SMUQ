function i_figure = plot_Output_Posterior_fun(PlottingFormat, i_figure, save_fig, nb_of_sigma, plot_y_interval, plot_Max_Like, plot_Max_Post, check_Output_Posterior, ...
                                              data_scale, x_names, plot_absc_min, plot_absc_max, plot_data_min, plot_data_max, y_names, proc, ...
                                              x_check, y_sum, y_sq_sum, i_def, x_nodes, x_data, y_data, x_Max_Like, y_Max_Like, x_Max_Post, y_Max_Post, i_sets)

    figure(i_figure)
    hold on
    
    if data_scale == 'lin'
        hdata=scatter(x_data,y_data, 125, 'k', 'square', 'LineWidth', 2);
%         hdatao=scatter(x_data_orig,y_data_provv_orig, 35, 'k', 'v', 'LineWidth', 2);
    elseif data_scale == 'log'
        hdata=scatter(x_data,y_data, 125, 'k', 'square', 'LineWidth', 2);
%         hdatao=scatter(x_data_orig,y_data_orig, 35, 'k', 'v', 'LineWidth', 2);
    end
    
    if plot_y_interval == 1
                
        y_mean=y_sum./(i_def);
        y_st_deviation = sqrt(y_sq_sum ./ (i_def) - y_mean.^2);
        y_max = y_mean + nb_of_sigma*y_st_deviation;
        y_min = y_mean - nb_of_sigma*y_st_deviation;

        if data_scale == 'lin'
            plot(x_nodes,y_max,':r','LineWidth',2);
            hint=plot(x_nodes,y_min,':r','LineWidth',2);
            hmean=plot(x_nodes,y_mean,'--r','LineWidth',3);
        elseif data_scale == 'log'
            plot(x_nodes,10.^y_max,':r','LineWidth',2);
            hint=plot(x_nodes,10.^y_min,':r','LineWidth',2);
            hmean=plot(x_nodes,10.^y_mean,'--r','LineWidth',3);
        end 
            
        
    end
    
    if plot_Max_Like == 1
        if data_scale == 'lin'
            hmaxlike=plot(x_Max_Like,y_Max_Like,'^b','markers',10,'LineWidth',2.5);
%             plot(x_Max_Like(nodes_for_markers2),y_Max_Like(nodes_for_markers2),'oc','MarkerSize',12,'MarkerFaceColor','c');  
%             hmaxlike=plot(x_Max_Like(nodes_for_markers2(1)),y_Max_Like(nodes_for_markers2(1)),'-oc','MarkerSize',12,'MarkerFaceColor','c');
        elseif data_scale == 'log'
            hmaxlike=semilogy(x_Max_Like,y_Max_Like,'^b','markers',10,'LineWidth',2.5);
%             semilogy(x_Max_Like(nodes_for_markers2),y_Max_Like(nodes_for_markers2),'oc','MarkerSize',12,'MarkerFaceColor','c');  
%             hmaxlike=semilogy(x_Max_Like(nodes_for_markers2(1)),y_Max_Like(nodes_for_markers2(1)),'-oc','MarkerSize',12,'MarkerFaceColor','c');
        end
    end

    if plot_Max_Post == 1
        if data_scale == 'lin'
            hmaxpost=plot(x_Max_Post,y_Max_Post,'^c','markers',10,'LineWidth',2.5);
%             plot(x_Max_Post(nodes_for_markers),y_Max_Post(nodes_for_markers),'^b','MarkerSize',12,'MarkerFaceColor','b');
%             hmaxpost=plot(x_Max_Post(nodes_for_markers(1)),y_Max_Post(nodes_for_markers(1)),'-^b','MarkerSize',12,'MarkerFaceColor','b');
        elseif data_scale == 'log'
            hmaxpost=semilogy(x_Max_Post,y_Max_Post,'^c','markers',10,'LineWidth',2.5);
%             semilogy(x_Max_Post(nodes_for_markers),y_Max_Post(nodes_for_markers),'^b','MarkerSize',12,'MarkerFaceColor','b');
%             hmaxpost=semilogy(x_Max_Post(nodes_for_markers(1)),y_Max_Post(nodes_for_markers(1)),'-^b','MarkerSize',12,'MarkerFaceColor','b');
        end
    end

    if check_Output_Posterior == 1

        for i=1:length(x_check)

            j=1;
            while (x_nodes(j)<x_check(i)) && (j<length(x_nodes))
                j=j+1;
            end

            ylim([plot_data_min plot_data_max]);
            y1=get(gca,'ylim');
            plot([x_nodes(j), x_nodes(j)], y1,'--k','LineWidth',3);
            %plot(x(nodes(j)),N5_data(nodes(j)),'og','MarkerSize',15,'MarkerFaceColor','auto');

        end
        
    end
    
    str_x = x_names;
    str_y = y_names;
    
    if PlottingFormat(1,:) == 'Visual'
        xh = xlabel(str_x);
        yh = ylabel(str_y);    

        if plot_Max_Post == 1 && plot_Max_Like == 1
                legend([hdata,hmean,hmaxlike,hmaxpost],'Data','Mean and 3\sigma Interval for Output Posterior','Output Max Likelihood','Output Max Posterior', 'Location', 'northwest');
        elseif plot_Max_Like == 1
            legend([hdata,hmean,hint,hmaxlike],'Data','Output Posterior Mean','Output Posterior 3\sigma Interval','Output Max Likelihood');
        elseif plot_Max_Post == 1
            legend([hdata,hmean,hint,hmaxpost],'Data','Output Posterior Mean','Output Posterior 3\sigma Interval','Output Max Posterior');
        else
            legend([hdata,hmean,hint],'Data','Output Posterior Mean','Output Posterior 3\sigma Interval');
        end
    elseif PlottingFormat(1,:) == 'Papers'
        xh = xlabel(str_x,'Interpreter','latex');
        yh = ylabel(str_y,'Interpreter','latex');    

        if plot_Max_Post == 1 && plot_Max_Like == 1
                legend([hdata,hmean,hmaxlike,hmaxpost],'Data','Mean and 3\sigma Interval for Output Posterior','Output Max Likelihood','Output Max Posterior','Location','best','Interpreter','latex');
        elseif plot_Max_Like == 1
            legend([hdata,hmean,hint,hmaxlike],'Data','Output Posterior Mean','Output Posterior 3\sigma Interval','Output Max Likelihood','Location','best','Interpreter','latex');
        elseif plot_Max_Post == 1
            legend([hdata,hmean,hint,hmaxpost],'Data','Output Posterior Mean','Output Posterior 3\sigma Interval','Output Max Posterior','Location','best','Interpreter','latex');
        else
            legend([hdata,hmean,hint],'Data','Output Posterior Mean','Output Posterior 3\sigma Interval','Location','best','Interpreter','latex');
        end
    end
    
    set(gca,'Xlim',[plot_absc_min plot_absc_max],'Ylim',[plot_data_min plot_data_max])
    if PlottingFormat(1,:) == 'Visual'
        set(gca,'FontSize',30, 'FontName','PT Sans')
        set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
    elseif PlottingFormat(1,:) == 'Papers'
        set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
        set(gcf, 'PaperPositionMode', 'auto');
        set(gcf,'Renderer','painters')
    end 
    pbaspect([1.5 1 1])
    
    if save_fig == 1
        if check_Output_Posterior == 1
            str_save_fig = [strcat('./fig/Posteriors/QoI/QoI_Posterior_Mean_ConfInt_1_',num2str(i_sets),'.fig')];
        else
            str_save_fig = [strcat('./fig/Posteriors/QoI/QoI_Posterior_Mean_ConfInt_',num2str(i_sets),'.fig')];
        end
        savefig(i_figure, str_save_fig);
    end
    i_figure=i_figure+1;
    
end
