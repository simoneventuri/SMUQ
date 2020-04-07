function i_figure = plot_Output_Variation_Pcolor_fun(PlottingFormat, i_figure, save_fig, ...
                                                     plot_absc_min, plot_absc_max, plot_data_min, plot_data_max, data_scale, ...
                                                     x_names, y_names, x_nodes,  y_Space_eff, y_Space, Hist_Matrix, i_sets)

    space = {' '};
                                                 
    Hist_Matrix_provv1(:,:)=Hist_Matrix(:,:);

    y_mean = zeros(1,size(Hist_Matrix_provv1,2));
    y_min = zeros(1,size(Hist_Matrix_provv1,2));
    y_max = zeros(1,size(Hist_Matrix_provv1,2));
    y_mp = zeros(1,size(Hist_Matrix_provv1,2));
    temp_vector = zeros(1,size(Hist_Matrix_provv1,2));
    
    for i_line=1:size(Hist_Matrix_provv1,2)
        mean_vec(i_line) = sum(Hist_Matrix_provv1(:,i_line).*y_Space_eff(1:end)')./sum(Hist_Matrix_provv1(:,1));
        temp_vector = Hist_Matrix_provv1(:,i_line);
%         a = y_Space_eff(1:end)' - mean_vec(i_line)*ones(length(y_Space_eff(1:end)),1);
%         b = a.^2;
%         c = Hist_Matrix_provv1(:,i_line).*b;
%         std_vec(i_line) = (sum(c)./sum(Hist_Matrix_provv1(:,1))).^0.5;
%         y_max(i_line) = mean_vec(i_line)+3*std_vec(i_line);
%         y_min(i_line) = mean_vec(i_line)-3*std_vec(i_line);
        [max_val,max_index] = max(temp_vector);
        y_mp(i_line) = y_Space_eff(max_index);
        temp_vector(temp_vector~=0)=1;
        temp_vector(temp_vector==0)=NaN;
        y_mean(i_line) = mean_vec(i_line);
        y_min(i_line) = min(temp_vector.*y_Space_eff(1:end)');
        y_max(i_line) = max(temp_vector.*y_Space_eff(1:end)');
    end

    figure(i_figure)
    pcolor(x_nodes,y_Space(1:end),Hist_Matrix(:,:))%./sum(Hist_Matrix(:,1)));
    shading interp
    hold on

    if data_scale == 'lin'

        plot(x_nodes,y_max,'--r','LineWidth',2);
        hint= plot(x_nodes,y_min,'--r','LineWidth',2);
        hmean= plot(x_nodes,y_mean,'--r','LineWidth',4);
        hmp = plot(x_nodes,y_mp,':b','LineWidth',4);

    elseif data_scale == 'log'

        semilogy(x_nodes,10.^y_max,'--r','LineWidth',2);
        hint=semilogy(x_nodes,10.^y_min,'--r','LineWidth',2);
        hmean=semilogy(x_nodes,10.^y_mean,'--r','LineWidth',4);
        hmp = semilogy(x_nodes,10.^y_mp,':b','LineWidth',4);

    end

    str_x = x_names;
    str_y = y_names;
    title_str = strcat('Output Histogram of', space(1), y_names, space(1), 'for Set',space(1),num2str(i_sets));
    if PlottingFormat(1,:) == 'Visual' 
        xh = xlabel(str_x);
        yh = ylabel(str_y);
        th = title(title_str);
        lh = legend([hmean,hint,hmp],{'Output Mean','Output Min/Max', 'Most Probable Output'});
        set(lh,'FontSize',24)
    elseif PlottingFormat(1,:) == 'Papers'
        xh = xlabel(str_x,'Interpreter','latex');
        yh = ylabel(str_y,'Interpreter','latex');
        th = title(title_str,'Interpreter','latex');
        lh = legend([hmean,hint,hmp],{'Output Mean','Output Min/Max', 'Most Probable Output'},'Interpreter','latex','Location','eastoutside');
        set(lh,'FontSize',24)
    end
    
    pbaspect([1.5 1 1])
    set(gca,'Xlim',[plot_absc_min plot_absc_max],'Ylim',[plot_data_min plot_data_max])
    if PlottingFormat(1,:) == 'Visual'
        set(gca,'FontSize',30, 'FontName','PT Sans')
        set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
    elseif PlottingFormat(1,:) == 'Papers'
        set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
        set(gcf, 'PaperPositionMode', 'auto');
    end 
    grid off
    
 
    %%%%% BOX PLOT ALTERNATIVE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
    %%%%% DO NOT REMOVE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
%     Hist_Matrix_provv1(:,:) = [zeros(size(Hist_Matrix(:,:),1),1),Hist_Matrix(:,:)];
%     ii_counter = 0;
%     b = 0;
%     for i_line=1:1:size(Hist_Matrix_provv1,2)
%         b = b + 1;
%     end
%     datamatrix = zeros(sum(Hist_Matrix_provv1(:,1)),b);
%     for i_line=1:1:size(Hist_Matrix_provv1,2)
%         ii_counter = ii_counter + 1;
%         for j = 1:size(Hist_Matrix_provv1,1)
%             a = sum(Hist_Matrix_provv1(1:j-1,i_line));
%             i_counter = 0;
%             for k = 1:Hist_Matrix_provv1(j,i_line)
%                 i_counter = i_counter + 1;
%                 datamatrix(a+i_counter,ii_counter) = y_Space_eff(j);
%             end
%         end
% 
%     end
%     
%     figure(i_figure)
%     boxplot(datamatrix,'Symbol','r.')
%     set(findobj(gcf,'-regexp','Tag','\w*Whisker'),'LineStyle','-')
%     lines = findobj(gcf, 'type', 'line', 'Tag', 'Median');
%     set(lines, 'LineWidth', 4);
%     set(lines, 'Color', 'm');
% 
%     str_x = x_names;
%     str_y = y_names;
%     title_str = strcat('Output Histogram of', space(1), y_names, space(1), 'for Set',space(1),num2str(i_sets));
% 
%     if PlottingFormat(1,:) == 'Visual' 
%         xh = xlabel(str_x);
%         yh = ylabel(str_y);
%         th = title(title_str);
%     elseif PlottingFormat(1,:) == 'Papers'
%         xh = xlabel(str_x,'Interpreter','latex');
%         yh = ylabel(str_y,'Interpreter','latex');
%         th = title(title_str,'Interpreter','latex');
%     end
%     
%     pbaspect([1.5 1 1])
%     set(gca,'Xlim',[plot_absc_min plot_absc_max],'Ylim',[plot_data_min plot_data_max])
%     if PlottingFormat(1,:) == 'Visual'
%         set(gca,'FontSize',30, 'FontName','PT Sans')
%         set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
%     elseif PlottingFormat(1,:) == 'Papers'
%         set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
%         set(gcf, 'PaperPositionMode', 'auto');
%     end 
%     grid off
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    if save_fig == 1
       str_save_fig = [strcat('./fig/Histogram/variation_histogram_',num2str(i_sets),'.fig')];
       savefig(i_figure, str_save_fig);
    end
    i_figure=i_figure+1;
    
end

