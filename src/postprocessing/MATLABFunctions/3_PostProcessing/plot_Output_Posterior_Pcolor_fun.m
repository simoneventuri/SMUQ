function i_figure = plot_Output_Posterior_Pcolor_fun(PlottingFormat, i_figure, save_fig, plot_Max_Like, plot_Max_Post, check_Output_Posterior, ...
                                                     plot_absc_min, plot_absc_max, plot_data_min, plot_data_max, data_scale, ...
                                                     x_names, y_names, x_check, x_nodes, x_Max_Like, y_Max_Like, x_Max_Post, y_Max_Post, ...
                                                     x_data, y_data, y_Space_eff, y_Space, hist_x_nodes, Hist_Matrix, , x_data_orig, y_data_orig, i_sets)
                                              
    Hist_Matrix_provv1(:,:)=Hist_Matrix(:,:);
    space = {' '};
    
    for i_line=1:size(Hist_Matrix_provv1,2)
        mean_vec(i_line) = sum(Hist_Matrix_provv1(:,i_line).*y_Space_eff(1:end)')./sum(Hist_Matrix_provv1(:,1));
        a = y_Space_eff(1:end)' - mean_vec(i_line)*ones(length(y_Space_eff(1:end)),1);
        b = a.^2;
        c = Hist_Matrix_provv1(:,i_line).*b;
        std_vec(i_line) = (sum(c)./sum(Hist_Matrix_provv1(:,1))).^0.5;
        y_max(i_line) = mean_vec(i_line)+3*std_vec(i_line);
        y_min(i_line) = mean_vec(i_line)-3*std_vec(i_line);
        y_mean(i_line) = mean_vec(i_line);
    end

    figure(i_figure)
    pcolor(hist_x_nodes,y_Space(1:end),Hist_Matrix(:,:)./sum(Hist_Matrix(:,1)));
    shading interp
    hold on

    if data_scale == 'lin'

        plot(hist_x_nodes,y_max,':r','LineWidth',3);
        hint=plot(hist_x_nodes,y_min,':r','LineWidth',3);
        hmean=plot(hist_x_nodes,y_mean,'--r','LineWidth',3);

    elseif data_scale == 'log'

        semilogy(hist_x_nodes,10.^y_max,':r','LineWidth',3);
        hint=semilogy(hist_x_nodes,10.^y_min,':r','LineWidth',3);
        hmean=semilogy(hist_x_nodes,10.^y_mean,'--r','LineWidth',3);

    end

    for j=1:length(x_data)
        i=1;
        while (y_data(j)>y_Space(i))
            i=i+1;
        end
        if data_scale == 'lin'
            y_Space_provv = y_Space;
            y_data_provv(j) = y_Space(i);
        elseif data_scale == 'log'
            y_Space_provv = y_Space;
            y_data_provv(j) = y_Space(i);
        end
    end

    for j=1:length(x_nodes)
        i=1;
        while (y_Max_Like(j)>y_Space(i))
            i=i+1;
        end
        if data_scale == 'lin'
            y_Max_Like_provv(j) = y_Space(i);
        elseif data_scale == 'log'
            y_Max_Like_provv(j) = y_Space(i);
        end
    end
    if plot_Max_Like == 1
        if data_scale == 'lin'
            hmaxlike=plot(x_Max_Like,y_Max_Like_provv,'vb','markers',10,'LineWidth',2.5);
%             plot(x_Max_Like(nodes_for_markers2),y_Max_Like_provv(nodes_for_markers2),'oc','MarkerSize',12,'MarkerFaceColor','c');  
%             hmaxlike=plot(x_Max_Like(nodes_for_markers2(1)),y_Max_Like_provv(nodes_for_markers2(1)),'-oc','MarkerSize',12,'MarkerFaceColor','c');
        elseif data_scale == 'log'
            hmaxlike=semilogy(x_Max_Like,y_Max_Like_provv,'vb','markers',10,'LineWidth',2.5);
%             semilogy(x_Max_Like(nodes_for_markers2),y_Max_Like_provv(nodes_for_markers2),'oc','MarkerSize',12,'MarkerFaceColor','c');  
%             hmaxlike=semilogy(x_Max_Like(nodes_for_markers2(1)),y_Max_Like_provv(nodes_for_markers2(1)),'-oc','MarkerSize',12,'MarkerFaceColor','c');
        end

    end 

    for j=1:length(x_nodes)
        i=1;
        while (y_Max_Post(j)>y_Space(i))
            i=i+1;
        end
        if data_scale == 'lin'
            y_Max_Post_provv(j) = y_Space(i);
        elseif data_scale == 'log'
            y_Max_Post_provv(j) = y_Space(i);
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

        end

    end
    
    if plot_Max_Post == 1
        if data_scale == 'lin'
            hmaxpost=plot(x_Max_Post,y_Max_Post_provv,'^c','markers',10,'LineWidth',2.5);
%             plot(x_Max_Post(nodes_for_markers),y_Max_Post_provv(nodes_for_markers),'^b','MarkerSize',12,'MarkerFaceColor','b');
%             hmaxpost=plot(x_Max_Post(nodes_for_markers(1)),y_Max_Post_provv(nodes_for_markers(1)),'-^b','MarkerSize',12,'MarkerFaceColor','b');
        elseif data_scale == 'log'
            hmaxpost=semilogy(x_Max_Post,y_Max_Post_provv,'^c','markers',10,'LineWidth',2.5);
%             semilogy(x_Max_Post(nodes_for_markers),y_Max_Post_provv(nodes_for_markers),'^b','MarkerSize',12,'MarkerFaceColor','b');
%             hmaxpost=semilogy(x_Max_Post(nodes_for_markers(1)),y_Max_Post_provv(nodes_for_markers(1)),'-^b','MarkerSize',12,'MarkerFaceColor','b');
        end
    end
    
    if data_scale == 'lin'
        hdata=scatter(x_data,y_data_provv, 125, 'k', 'square', 'LineWidth', 2.5);
    elseif data_scale == 'log'
        hdata=scatter(x_data,y_data_provv, 125, 'k', 'square', 'LineWidth', 2.5);
    end

    str_x = x_names;
    str_y = y_names;
    title_str = strcat(y_names,space(1),'Forward Propagation for Set',space(1),num2str(i_sets));
    
    if PlottingFormat(1,:) == 'Visual'
        xlabel(str_x);
        ylabel(str_y);
        title(title_str);
        
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
        xlabel(str_x,'Interpreter','latex');
        ylabel(str_y,'Interpreter','latex');
        title(title_str,'Interpreter','latex');
        
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
