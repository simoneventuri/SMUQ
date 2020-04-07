function i_figure = check_Output_Posterior_fun(PlottingFormat, i_figure, save_fig, nb_pcolor, x_check, ...  
                                               plot_data_min, plot_data_max, x_names, x_nodes, y_names, data_scale,  ...
                                               x_data, y_data, y_Space, Hist_Matrix, x_data_orig, y_data_orig, i_sets)
    space = {' '};                                       
    for i=1:length(x_check)     

        j=1;
        while (x_nodes(j)<x_check(i)) && (j<length(x_nodes))
            j=j+1;
        end
        
        jj=1;
        while (x_data(jj)<x_check(i)) && (jj<length(x_data))
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
        y_data_provv = y_Space(k);
        pdf_vec = Hist_Matrix(:,j)/trapz(y_Space_provv,Hist_Matrix(:,j));

        %%% Finding the value of the pdf for the data        
        pdf_data = interp1(y_Space_provv,pdf_vec,y_data_provv,'linear');

        figure(i_figure)
        if data_scale == 'lin'
            f = fit(y_Space(:),Hist_Matrix(:,j),'smoothingspline','SmoothingParam',0.005);
            vec = feval(f,y_Space(:));
            pdf_vec = vec / trapz(y_Space(:),vec);
            hposterior=plot(y_Space(:),pdf_vec,'b','LineWidth',3);
%             hposterior=plot(y_Space(:),Hist_Matrix(:,j)/trapz(y_Space_provv,Hist_Matrix(:,j)),'b','LineWidth',3);
            hold on
            plot([y_data_provv y_data_provv],[0 pdf_data],'-.g','LineWidth',3);
            hdata=plot(y_data_provv, pdf_data,'og','MarkerSize',15,'MarkerFaceColor','g');
        elseif data_scale == 'log'
            f = fit(10.^y_Space(:),Hist_Matrix(:,j),'smoothingspline','SmoothingParam',0.005);
            vec = feval(f,10.^y_Space(:));
            pdf_vec = vec / trapz(10.^y_Space(:),vec);
            hposterior=plot(10.^y_Space(:),pdf_vec,'b','LineWidth',3);
%             hposterior=plot(10.^y_Space(:),Hist_Matrix(:,j)/trapz(y_Space_provv,Hist_Matrix(:,j)),'b','LineWidth',3);
            hold on
            plot([10.^y_data_provv 10.^y_data_provv],[0 pdf_data],'-.g','LineWidth',3);
            hdata=plot(10.^y_data_provv, pdf_data,'og','MarkerSize',15,'MarkerFaceColor','g');
        end

        harea=area(hposterior.XData, hposterior.YData);
        harea.EdgeColor='none';
        harea.FaceColor=[.5,.5,1];
        harea.FaceAlpha=.25;

        str_x = [y_names(1,:),' at ',x_names(1,:),'=', num2str(x_nodes(j))];
        str_y = ['PDF'];
        if PlottingFormat(1,:) == 'Visual'
            xh = xlabel(str_x);
            yh = ylabel(str_y);    
            %title(str_title);
            lh = legend([hposterior,hdata],'Output Posterior','Data');
        elseif PlottingFormat(1,:) == 'Papers'
            xh = xlabel(str_x,'Interpreter','latex');
            yh = ylabel(str_y,'Interpreter','latex');    
            %title(str_title,'Interpreter','latex');
            lh = legend([hposterior,hdata],{'Output Posterior','Data'},'Location','eastoutside','Interpreter','latex');
        end
        
        set(gca,'CLim',[0 nb_pcolor],'Xlim',[plot_data_min plot_data_max]);
        ylim([0; Inf]); 

        if PlottingFormat(1,:) == 'Visual'
            set(gca,'FontSize',30, 'FontName','PT Sans')
            set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
        elseif PlottingFormat(1,:) == 'Papers'
            set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
            set(gcf, 'PaperPositionMode', 'auto');
            set(gcf,'Renderer','painters')
        end 
        if save_fig == 1
            str_save_fig = [strcat('./fig/Posteriors/QoI/QoI_Posterior_at_', num2str(x_nodes(j)),'_',num2str(i_sets),'.fig')];
            savefig(i_figure, str_save_fig);
        end
        i_figure=i_figure+1;

    end
    
end
