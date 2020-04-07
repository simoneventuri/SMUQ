function i_figure = plot_HPD_fun(PlottingFormat, i_figure, save_fig, nb_pcolor, HPD_tol, check_Output_Posterior, x_check, ...
                                 plot_absc_min, plot_absc_max, x_names, x_data, x_nodes, data_scale, ...
                                 y_data, y_Space, Hist_Matrix, i_sets)

    space = {' '};
    for i=1:length(x_nodes)
        
        jj=1;
        while (x_data(jj)<x_nodes(i)) && (jj<length(x_data))
            jj=jj+1;
        end
        
        
        k=1;
        if data_scale == 'lin'
            while (y_data(jj)>y_Space(k))
                k=k+1;
            end
        else
            while (log10(y_data(jj))>y_Space(k))
                k=k+1;
            end
        end

        
        y_Space_provv = y_Space;
        y_data_provv(i) = y_Space(k);
        
        
        pdf_vec = Hist_Matrix(:,i)./trapz(y_Space_provv,Hist_Matrix(:,i));
        
        %%% Finding the value of the pdf for the data        
        pdf_data = interp1(y_Space_provv,pdf_vec,y_data_provv(i),'linear');

        %%% Finding the number of different intervals with pdf > pdf_vec(data) and their extremes
        pdf_higher_flag = pdf_vec > pdf_data;
        pdf_temp=0;                
        i_interval_start=0;
        i_interval_end=0;
        for i_hist=1:size(Hist_Matrix,1)
            
            if pdf_temp-pdf_higher_flag(i_hist) == -1
                
                i_interval_start=i_interval_start+1;
                interval_start(i_interval_start)=i_hist;
                
            elseif pdf_temp-pdf_higher_flag(i_hist) == 1
                
                i_interval_end=i_interval_end+1;
                interval_end(i_interval_end)=i_hist-1;    
            
            end
            
            if i_interval_start~=i_interval_end
                
                interval_end(i_interval_start)=size(Hist_Matrix,1);   
                
            end
            
            pdf_temp=pdf_higher_flag(i_hist);
            
        end
        nb_interval=i_interval_start;
        
        %%% Computing Integral of pdfs > pdf_data
        integral = 0; 
        for i_interval = 1:nb_interval

          nodes_interval = [interval_start(i_interval):interval_end(i_interval)];    

          interval_int=0;
          if length(nodes_interval) > 1
            interval_int = trapz(y_Space_provv(nodes_interval),pdf_vec(nodes_interval));
          end

          pre_int=0;
          if nodes_interval(1) > 1

              pre_abscissa = (y_Space_provv(nodes_interval(1))-y_Space_provv(nodes_interval(1)-1)) * ( 1 - (pdf_data-pdf_vec(nodes_interval(1)-1)) / (pdf_vec(nodes_interval(1))-pdf_vec(nodes_interval(1)-1)) );
              pre_int = pre_abscissa*(pdf_data+pdf_vec(nodes_interval(1)))/2;

          end

          post_int=0;
          if nodes_interval(end) < length(y_Space_provv)

              post_abscissa =  (y_Space_provv(nodes_interval(end)+1)-y_Space_provv(nodes_interval(end))) * (pdf_vec(nodes_interval(end))-pdf_data) / (pdf_vec(nodes_interval(end))-pdf_vec(nodes_interval(end)+1));
              post_int = post_abscissa*(pdf_data+pdf_vec(nodes_interval(end)))/2;

          end

          integral = integral + (pre_int + interval_int + post_int);

        end
        
        %%% Computing HPD
        Gamma(i) = 1 - integral + 1.d-10;

    end 

    figure(i_figure)
    semilogy(x_nodes,Gamma','ob','markers',10,'MarkerFaceColor','b');
    hold on
    x1=get(gca,'xlim');
    semilogy(0,[HPD_tol HPD_tol],'--k');
    str_x = x_names(1,:);
    str_y = ['HPD'];
    str_title = [strcat('HPD for Set',space(1),num2str(i_sets))];
    
    if PlottingFormat(1,:) == 'Visual'
        xh = xlabel(str_x);
        yh = ylabel(str_y);
        th = title(str_title);
    elseif PlottingFormat(1,:) == 'Papers'
        xh = xlabel(str_x,'Interpreter','latex');
        yh = ylabel(str_y,'Interpreter','latex');
        th = title(str_title,'Interpreter','latex');
    end
    
    if check_Output_Posterior == 1
        for k=1:length(x_check)
            i=1;
            while x_nodes(i)<x_check(k)
                i=i+1;
            end
            y1=get(gca,'ylim');
            plot([0, 0], y1,'--k','LineWidth',3);
        end

    end
    
    set(gca,'Xlim',[plot_absc_min plot_absc_max],'CLim',[0 nb_pcolor])
    grid off
    pbaspect([1.5 1 1])
    if PlottingFormat(1,:) == 'Visual'
        set(gca,'FontSize',30, 'FontName','PT Sans')
        set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
    elseif PlottingFormat(1,:) == 'Papers'
        set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
        set(gcf, 'PaperPositionMode', 'auto');
        set(gcf,'Renderer','painters')
    end 
    
    if save_fig == 1
        if check_Output_Posterior == 1 
            str_save_fig = [strcat('./fig/Posteriors/QoI/HPD_1_',num2str(i_sets),'.fig')];
        else
            str_save_fig = [strcat('./fig/Posteriors/QoI/HPD_',num2str(i_sets),'.fig')];
        end
        savefig(i_figure, str_save_fig);
    end
    i_figure=i_figure+1;
    
end
