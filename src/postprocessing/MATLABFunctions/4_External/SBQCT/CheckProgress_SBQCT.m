function i_figure = checkProgress_SQBQCT(PlottingFormat, i_figure, data_scale_plot, x_names, y_names, plot_y_data, ...
                                         plot_y_start, plot_y_current, plot_y_prop,  x_data, y_data, ...
                                         x_start, y_start, x_current, y_current, x_prop, y_prop)

    NBins = 10

    
    if plot_y_data
        iBinVec_data = x_data;
        KQ_data_Vec  = 10.d0.^y_data;
        iVec=1;
        KQ_data=ones(NBins,NBins)*1.d-16;
        for iBins = 1:NBins
            KQ_data_Diss(iBins) = KQ_data_Vec(iVec);
            iVec = iVec + 1;
            for jBins = 1:iBins
                KQ_data(iBins,jBins) = KQ_data_Vec(iVec);
                iVec = iVec + 1;
            end
        end
    end
    
    if plot_y_start == 1
        KQ_start=ones(NBins,NBins,size(y_start,2))*1.d-16;
        for iProc=1:size(y_start,2)
            iBinVec_start(:,iProc) = x_start(:,iProc);
            KQ_start_Vec(:,iProc)  = 10.d0.^y_start(:,iProc);
            iVec=1;
            for iBins = 1:NBins
                KQ_start_Diss(iBins,iProc) = KQ_start_Vec(iVec,iProc);
                iVec = iVec + 1;
                for jBins = 1:iBins
                    KQ_start(iBins,jBins,iProc) = KQ_start_Vec(iVec,iProc);
                    iVec = iVec + 1;
                end
            end
        end
    end
    
    
    if plot_y_current == 1
        KQ_current=ones(NBins,NBins,size(y_current,2))*1.d-16;
        for iProc=1:size(y_current,2)
            iBinVec_current(:,iProc) = x_current(:,iProc);
            KQ_current_Vec(:,iProc)  = 10.d0.^y_current(:,iProc);
            iVec=1;
            for iBins = 1:NBins
                KQ_current_Diss(iBins,iProc) = KQ_current_Vec(iVec,iProc);
                iVec = iVec + 1;
                for jBins = 1:iBins
                    KQ_current(iBins,jBins,iProc) = KQ_current_Vec(iVec,iProc);
                    iVec = iVec + 1;
                end
            end
        end
    end

    
    if plot_y_prop == 1
        KQ_prop=ones(NBins,NBins,size(y_prop,2))*1.d-16;
        for iProc=1:size(y_prop,2)
            iBinVec_prop(:,iProc) = x_prop(:,iProc);
            KQ_prop_Vec(:,iProc)  = 10.d0.^y_prop(:,iProc);
            iVec=1;
            KQ_prop=ones(NBins,NBins)*1.d-16;
            for iBins = 1:NBins
                KQ_prop_Diss(iBins,iProc) = KQ_prop_Vec(iVec,iProc);
                iVec = iVec + 1;
                for jBins = 1:iBins
                    KQ_prop(iBins,jBins,iProc) = KQ_prop_Vec(iVec,iProc);
                    iVec = iVec + 1;
                end
            end
        end
    end

    
    figure(i_figure)
    if plot_y_data == 1
        hdata=semilogy([1:NBins],KQ_data_Diss,'Color','g','linestyle','-','LineWidth',3);%,'Marker','o','Markersize',10);
        hold on
    end
    if plot_y_start == 1
        for iProc=1:size(y_start,2)
            hstart=semilogy([1:NBins],KQ_start_Diss(:,iProc),'Color','k','linestyle','--','LineWidth',3);%,'Marker','o','Markersize',10);
            hold on
        end
    end
    if plot_y_current == 1
        for iProc=1:size(y_current,2)
            hcurrent=semilogy([1:NBins],KQ_current_Diss(:,iProc),'Color','b','linestyle','-.','LineWidth',3);%,'Marker','o','Markersize',10);
            hold on
        end
    end
    if plot_y_prop == 1
        for iProc=1:size(y_prop,2)
            hprop=semilogy([1:NBins],KQ_prop_Diss(:,iProc),'Color','r','linestyle',':','LineWidth',3);%,'Marker','o','Markersize',10);
            hold on
        end
    end
    
    ylabel(strcat('K_{i_{Diss}} * Q_{i} / Q_{Tot} [cm^3/s]'));
    xlabel('i-th Bin')
    legend([hdata,hcurrent,hprop],'Data','Current Step','Proposed Step');
    grid on
    %str_title = ['Dissociation Rate Coefficients for ', Molecule(1,:), ', ',System, ' System'];
    %title(str_title);
    xlim([0,  NBins+1]);
    ylim([1.d-16, 1.d-8]);
    set(gca,'FontSize',20, 'FontName','Palatino','TickDir','out','TickLabelInterpreter', 'latex');
    set(gcf, 'PaperPositionMode', 'auto');
    i_figure=i_figure+1;
    
    figure(i_figure)
    for iBins=1:min(NBins)

        if plot_y_data == 1
            hdata=semilogy([1:NBins],KQ_data(iBins,:),'Color','g','linestyle','-','LineWidth',3);%,'Marker','o','Markersize',10);
            hold on
        end
        if plot_y_start == 1
            for iProc=1:size(y_start,2)
                hstart=semilogy([1:NBins],KQ_start(iBins,:,iProc),'Color','k','linestyle','--','LineWidth',3);%,'Marker','o','Markersize',10);
                hold on
            end
        end
        if plot_y_current == 1
            for iProc=1:size(y_current,2)
                hcurrent=semilogy([1:NBins],KQ_current(iBins,:,iProc),'Color','b','linestyle','-.','LineWidth',3);%,'Marker','o','Markersize',10);
                hold on
            end
        end
        if plot_y_prop == 1
            for iProc=1:size(y_prop,2)
                hprop=semilogy([1:NBins],KQ_prop(iBins,:,iProc),'Color','r','linestyle',':','LineWidth',3);%,'Marker','o','Markersize',10);
                hold on
            end
        end
        
        ylabel(strcat('K_{',num2str(iBins),'-j} * Q_{',num2str(iBins),'} / Q_{Tot} [cm^3/s]'));
        xlabel('j-th Bin')
        legend([hdata,hcurrent,hprop],'Data','Current Step','Proposed Step');
        grid on
        %str_title = ['Dissociation Rate Coefficients for ', Molecule(1,:), ', ',System, ' System'];
        %title(str_title);
        xlim([0,  NBins+1]);
        ylim([1.d-16, 1.d-8]);
        set(gca,'FontSize',20, 'FontName','Palatino','TickDir','out','TickLabelInterpreter', 'latex');
        set(gcf, 'PaperPositionMode', 'auto');
        hold off
        pause

    end
    i_figure=i_figure+1;
    
end