function i_figure = plot_Theta_Posterior_fun(PlottingFormat, i_figure, save_fig, proc, plot_theta_hist, plot_theta_pdf,... 
                                                 plot_Priors, nb_bins_hist, plot_theta_max_like, plot_theta_max_post)

    filename = './theta_name.dat';
    delimiter = '\t';
    formatSpec = '%s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
    fclose(fileID);
    theta_name_vec = dataArray{:, 1};
    clearvars filename delimiter formatSpec fileID dataArray ans;
    theta_labels = theta_name_vec;
    theta_names = char(theta_name_vec);

    filename = './theta_data.dat';
    delimiter = ' ';
    startRow = 2;
    formatSpec = '%s%f%*s%*s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
    fclose(fileID);
    theta_scale = char(dataArray{:, 1});
    theta_data = dataArray{:, 2};
    clearvars filename delimiter startRow formatSpec fileID dataArray ans;
                       
    filename = './Prior.dat';
    startRow = 2;
    formatSpec = '%20s%20s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
    dataArray{1} = strtrim(dataArray{1});
    dataArray{2} = strtrim(dataArray{2});
    fclose(fileID);
    Prior_Dist = char(dataArray{:, 1});
    SigmaPrior_Norm = char(dataArray{:, 2});
    clearvars filename startRow formatSpec fileID dataArray ans;

    filename = './Prior.dat';
    startRow = 2;
    formatSpec = '%*40s%20f%20f%20f%f%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
    fclose(fileID);
    MuPrior = dataArray{:, 1};
    SigmaPrior = dataArray{:, 2};
    UnifMin = dataArray{:, 3};
    UnifMax = dataArray{:, 4};
    clearvars filename startRow formatSpec fileID dataArray ans;
    nb_dim=length(UnifMin);
    
    for i=1:nb_dim
        if SigmaPrior_Norm(i,1:2) == 'ye'
            if theta_scale(i,:) == 'lin'
                SigmaPrior(i)=MuPrior(i)*SigmaPrior(i);
            elseif theta_scale(i,:) == 'log'
                SigmaPrior(i)=log10(MuPrior(i))*SigmaPrior(i);
            end
        end 
    end
    
    
    theta = [];

    for i=proc

        filename = strcat('./clean_chain_',num2str(i),'.dat');
        startRow = 2;
        str_format='%20f';
        for ii=2:nb_dim+1
            str_format = strcat(str_format,'%20f');
        end
        str_format = strcat(str_format,'%f%[^\n\r]');
        formatSpec = str_format;
        fileID = fopen(filename,'r');
        dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
        fclose(fileID);
        Clean_Chain = [dataArray{1:end-1}];
        clearvars filename startRow formatSpec fileID dataArray ans;
        theta_temp(:,:)=Clean_Chain(:,1:nb_dim);

        theta = [theta; theta_temp];

        clearvars theta_temp

    end

    for i=proc

        filename = strcat('./theta_start_',num2str(i),'.dat');
        startRow = 2;
        endRow = 2;
        str_format='%20f';
        for ii=2:nb_dim+1
            str_format = strcat(str_format,'%20f');
        end
        str_format = strcat(str_format,'%f%[^\n\r]');
        formatSpec = str_format;
        fileID = fopen(filename,'r');
        dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
        fclose(fileID);
        Dirty_Chain = [dataArray{1:end-1}];
        clearvars filename startRow formatSpec fileID dataArray ans;
        theta_start(:,i)=Dirty_Chain(1,1:nb_dim);

    end
    
    if (plot_theta_max_like == 1)

        for i=proc

            filename = strcat('./theta_max_like_',num2str(i),'.dat');
            startRow = 2;
            formatSpec = '%20f';
            for j=1:size(Prior_Dist,1)-2
                formatSpec = strcat(formatSpec,'%20f');
            end
            formatSpec = strcat(formatSpec,'%f%[^\n\r]');
            fileID = fopen(filename,'r');
            dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
            fclose(fileID);
            theta_max_like(:,i) = [dataArray{1:end-1}];
            clearvars filename startRow formatSpec fileID dataArray ans;

        end

    end
    
    if (plot_theta_max_post == 1)
            
        for i=proc

            filename = strcat('./theta_max_post_',num2str(i),'.dat');
            startRow = 2;
            formatSpec = '%20f';
            for j=1:size(Prior_Dist,1)-2
                formatSpec = strcat(formatSpec,'%20f');
            end
            formatSpec = strcat(formatSpec,'%f%[^\n\r]');
            fileID = fopen(filename,'r');
            dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
            fclose(fileID);
            theta_max_post(:,i) = [dataArray{1:end-1}];
            clearvars filename startRow formatSpec fileID dataArray ans;

        end

    end

    theta_Space=zeros(nb_bins_hist+1,nb_dim);
    
    for i=1:nb_dim
        
        figure(i_figure)
        
        if theta_scale(i,:) == 'log'
            theta_Space(:,i) = logspace(log10(UnifMin(i)),log10(UnifMax(i)),nb_bins_hist+1);
        elseif theta_scale(i,:) == 'lin'
            theta_Space(:,i) = linspace(UnifMin(i),UnifMax(i),nb_bins_hist+1);
        end

        if (plot_theta_hist==1) || (plot_theta_pdf==1)

            if theta_scale(i,:) == 'lin'
                hpdf=histfit(theta(:,i),nb_bins_hist,'kernel');
                tmin = UnifMin(i);
                tmax = UnifMax(i);                
            elseif theta_scale(i,:) == 'log'
                hpdf=histfit(log10(theta(:,i)),nb_bins_hist,'kernel');
                tmin = log10(UnifMin(i));
                tmax = log10(UnifMax(i));
            end

            if (plot_theta_hist~=1)
                hpdf(1).FaceColor='none';
                hpdf(1).LineStyle = 'none';
                hpdf(1).Visible='off';
            end
        
            if (plot_theta_pdf~=1)
                hpdf(2).LineStyle='none';
                hpdf(2).Visible='off';
            else
                i_c = 0;
                for i_f = 1:length(hpdf(2).XData)
                    i_c = i_c + 1;
                    if hpdf(2).XData(i_f) >= tmin
                        i_start = i_c;
                        break
                    end
                end
                
                i_c = 0;
                for i_f = 1:length(hpdf(2).XData)
                    i_c = i_c + 1;
                    if hpdf(2).XData(length(hpdf(2).XData) - i_f + 1) <= tmax
                        i_end = length(hpdf(2).XData) - i_c + 1;
                        break
                    end
                end
                hpdf(2).Color=[.25,.25,.9];
                hpdf(2).LineWidth=3;
                hpdf(2).YData=hpdf(2).YData *1/trapz(hpdf(2).XData(i_start:i_end),hpdf(2).YData(i_start:i_end));
                hold on
                grid on
                harea=area(hpdf(2).XData,hpdf(2).YData);
                harea.EdgeColor='none';
                harea.FaceColor=[.5,.5,1];
                harea.FaceAlpha=.25;
            end
        
        end
        
        if plot_Priors == 1
        
            if Prior_Dist(i,:) == 'nor'

                if theta_scale(i,:) == 'log'
                    norm=normpdf(log10(theta_Space(:,i)),log10(MuPrior(i)),abs(SigmaPrior(i)));
                    hprior=plot(log10(theta_Space(:,i)),norm,'--k','LineWidth',3);
                    for i_processor=proc
                        ystart(i,i_processor)=normpdf(log10(theta_start(i,i_processor)),log10(MuPrior(i)),abs(SigmaPrior(i)));
                        s_p=plot(log10(theta_start(i,i_processor)),ystart(i,i_processor),'ko','MarkerSize',10,'LineWidth',3,'MarkerFaceColor','k');
                    end 
                elseif theta_scale(i,:) == 'lin'    
                    norm=normpdf(theta_Space(:,i),MuPrior(i),abs(SigmaPrior(i)));  
                    hprior=plot(theta_Space(:,i),norm,'--k','LineWidth',3);
                    for i_processor=1:proc
                        ystart(i,i_processor)=normpdf(theta_start(i,i_processor),MuPrior(i),abs(SigmaPrior(i)));
                        s_p=plot(theta_start(i,i_processor),ystart(i,i_processor),'ko','MarkerSize',10,'LineWidth',3,'MarkerFaceColor','k');
                    end
                end

            elseif Prior_Dist(i,:) == 'uni'

                if theta_scale(i,:) == 'log'
                    unif=unifpdf(log10(theta_Space(:,i)),log10(UnifMin(i)),log10(UnifMax(i)));
                    hprior=plot(log10(theta_Space(:,i)),unif,'--k','LineWidth',3);
                    for i_processor=proc
                        ystart(i,i_processor)=unifpdf(log10(theta_start(i,i_processor)),log10(UnifMin(i)),log10(UnifMax(i)));
                        s_p=plot(log10(theta_start(i,i_processor)),ystart(i,i_processor),'ko','MarkerSize',10,'LineWidth',3,'MarkerFaceColor','k');
                    end 
                elseif theta_scale(i,:) == 'lin'    
                    unif=unifpdf(theta_Space(:,i),UnifMin(i),UnifMax(i));
                    hprior=plot(theta_Space(:,i),unif,'--k','LineWidth',3);
                    for i_processor=proc
                        ystart(i,i_processor)=unifpdf(theta_start(i,i_processor),UnifMin(i),UnifMax(i));
                        s_p=plot(theta_start(i,i_processor),ystart(i,i_processor),'ko','MarkerSize',10,'LineWidth',3,'MarkerFaceColor','k');
                    end 
                end

            end
            
        end
        
        for i_processor=proc
            if plot_theta_max_like == 1
                h_maxLike=plot(theta_max_like(i,i_processor),0.d0.*theta_max_like(i,i_processor),'ro','MarkerSize',10,'LineWidth',3,'MarkerFaceColor','r');
            end
            if plot_theta_max_post == 1
                h_maxLike=plot(theta_max_post(i,i_processor),0.d0.*theta_max_post(i,i_processor),'bo','MarkerSize',10,'LineWidth',3,'MarkerFaceColor','b');
            end
        end
       
        
        if PlottingFormat(1,:) == 'Visual'    
            if (plot_theta_pdf == 1) && (plot_Priors == 1) && (plot_theta_hist==1)
                legend([hprior,hpdf(1),hpdf(2)],'Prior','Posterior','Normalized Posterior');
                set(gca,'xlim',[UnifMin(i) UnifMax(i)])
            elseif (plot_theta_pdf == 1) && (plot_Priors == 1)
                legend([hprior,hpdf(2)],'Prior', 'Normalized Posterior');
                set(gca,'xlim',[UnifMin(i) UnifMax(i)])
            elseif (plot_theta_pdf == 1) && (plot_theta_hist==1)
                legend([hpdf(1),hpdf(2)],'Posterior','Normalized Posterior');
            elseif (plot_theta_hist == 1) && (plot_Priors == 1)
                legend([hprior,hpdf(1)],'Prior','Posterior');
            else
                legend([hpdf(2)],'Posterior');
            end
        elseif PlottingFormat(1,:) == 'Papers' 
            if (plot_theta_pdf == 1) && (plot_Priors == 1) && (plot_theta_hist==1)
                legend([hprior,hpdf(1),hpdf(2)],'Prior','Posterior','Normalized Posterior','Interpreter','latex','Location','best');
                set(gca,'xlim',[UnifMin(i) UnifMax(i)])
            elseif (plot_theta_pdf == 1) && (plot_Priors == 1)
                legend([hprior,hpdf(2)],'Prior', 'Normalized Posterior','Interpreter','latex','Location','best');
                set(gca,'xlim',[UnifMin(i) UnifMax(i)])
            elseif (plot_theta_pdf == 1) && (plot_theta_hist==1)
                legend([hpdf(1),hpdf(2)],'Posterior','Normalized Posterior','Interpreter','latex','Location','best');
            elseif (plot_theta_hist == 1) && (plot_Priors == 1)
                legend([hprior,hpdf(1)],'Prior','Posterior','Interpreter','latex','Location','best');
            else
                legend([hpdf(2)],'Posterior','Interpreter','latex','Location','best');
            end
        end
        
        if theta_scale(i,:) == 'log'
            xlim([log10(UnifMin(i)),log10(UnifMax(i))])
        elseif theta_scale(i,:) == 'lin'
            xlim([UnifMin(i),UnifMax(i)])
        end
        
        if theta_scale(i,:) == 'log'
            if PlottingFormat(1,:) == 'Visual'    
                str_x = strcat('log_{10}(',strtrim(theta_names(i,:)),')');
            elseif PlottingFormat(1,:) == 'Papers' 
                str_x = strcat('$log_{10}(',strtrim(theta_names(i,:)),')$');
            end
        elseif theta_scale(i,:) == 'lin'
            str_x = strtrim(theta_names(i,:));
        end
        space = {' '};
        str_title = [strcat(str_x,space(1),'Posterior PDF')];
        if PlottingFormat(1,:) == 'Visual'
            xlabel(str_x);
            ylabel('Marginal PDFs');    
            title(str_title);
        elseif PlottingFormat(1,:) == 'Papers' 
            xlabel(str_x,'Interpreter','latex');
            ylabel('Marginal PDFs','Interpreter','latex');    
            title(str_title,'Interpreter','latex');
        end
        
        if PlottingFormat(1,:) == 'Visual'
            set(gca,'FontSize',30, 'FontName','PT Sans')
            set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
        elseif PlottingFormat(1,:) == 'Papers'
            set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
            set(gcf, 'PaperPositionMode', 'auto');
        end 
        grid off
        pbaspect([1.5 1 1])
        
        if save_fig == 1
            str_save_fig = ['./fig/Posteriors/Parameters/', str_x, '_Posterior.fig'];
            savefig(i_figure, str_save_fig);
        end
        i_figure=i_figure+1;
    end
    
end
