function i_figure = plot_Priors_fun(PlottingFormat, i_figure, save_fig, proc, nb_bins_hist, plot_Props)

    filename = './theta_name.dat';
    delimiter = '\t';
    formatSpec = '%s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
    fclose(fileID);
    theta_name_vec = dataArray{:, 1};
    clearvars filename delimiter formatSpec fileID dataArray ans;
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
          
    nb_dim=length(UnifMin);
    space = {' '};
    
    filename = './Proposal.dat';
    startRow = 2;
    formatSpec = '%*40s%20f%f%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
    fclose(fileID);
    MuProp = dataArray{:, 1};
    SigmaProp = dataArray{:, 2};
    clearvars filename startRow formatSpec fileID dataArray ans;

    filename = './Proposal.dat';
    startRow = 2;
    formatSpec = '%20s%20s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
    dataArray{1} = strtrim(dataArray{1});
    dataArray{2} = strtrim(dataArray{2});
    fclose(fileID);
    Prop_Dist = char(dataArray{:, 1});
    SigmaProp_Norm = char(dataArray{:, 2});
    clearvars filename formatSpec fileID dataArray ans;
    
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
    


    theta_Space=zeros(nb_bins_hist+1,nb_dim);
    for i=1:nb_dim
        
        figure(i_figure)
        
        if theta_scale(i,:) == 'log'
            theta_Space(:,i) = logspace(log10(UnifMin(i)),log10(UnifMax(i)),nb_bins_hist+1);
            xlim([log10(UnifMin(i)),log10(UnifMax(i))])
        elseif theta_scale(i,:) == 'lin'
            theta_Space(:,i) = linspace(UnifMin(i),UnifMax(i),nb_bins_hist+1);
            xlim([UnifMin(i),UnifMax(i)])
        end
        
        if Prior_Dist(i,:) == 'nor'
            
            if theta_scale(i,:) == 'log'
                norm=normpdf(log10(theta_Space(:,i)),log10(MuPrior(i)),abs(SigmaPrior(i)));               
                hprior=plot(log10(theta_Space(:,i)),norm,'-k','LineWidth',3);
                hold on
                grid on
                for i_processor=proc
                    ystart(i,i_processor)=normpdf(log10(theta_start(i,i_processor)),log10(MuPrior(i)),abs(SigmaPrior(i)));
                    s_p=plot(log10(theta_start(i,i_processor)),ystart(i,i_processor),'o','MarkerSize',10,'LineWidth',3,'MarkerFaceColor',[128/255 128/255 128/255],'Color',[128/255 128/255 128/255]);
                end 
            elseif theta_scale(i,:) == 'lin'    
                norm=normpdf(theta_Space(:,i),MuPrior(i),abs(SigmaPrior(i)));             
                hprior=plot(theta_Space(:,i),norm,'-k','LineWidth',3);
                hold on
                grid on
                for i_processor=proc
                    ystart(i,i_processor)=normpdf(theta_start(i,i_processor),MuPrior(i),SigmaPrior(i));
                    s_p=plot(theta_start(i,i_processor),ystart(i,i_processor),'o','MarkerSize',10,'LineWidth',3,'MarkerFaceColor',[128/255 128/255 128/255],'Color',[128/255 128/255 128/255]);
                end

            end
            
            if theta_scale(i,:) == 'log'
                harea=area(log10(theta_Space(:,i)),norm);
            elseif theta_scale(i,:) == 'log'
                harea=area(theta_Space(:,i),norm);                
            end     
            harea.EdgeColor='none';
            harea.FaceColor=[.5,.5,1];
            harea.FaceAlpha=.25;   
            
        elseif Prior_Dist(i,:) == 'uni'
            
            if theta_scale(i,:) == 'log'
                unif=unifpdf(log10(theta_Space(:,i)),log10(UnifMin(i)),log10(UnifMax(i)));          
                hprior=plot(log10(theta_Space(:,i)),unif,'-k','LineWidth',3);
                hold on
                grid on
                for i_processor=proc
                    ystart(i,i_processor)=unifpdf(log10(theta_start(i,i_processor)),log10(UnifMin(i)),log10(UnifMax(i)));
                    s_p=plot(log10(theta_start(i,i_processor)),ystart(i,i_processor),'o','MarkerSize',10,'LineWidth',3,'MarkerFaceColor',[128/255 128/255 128/255],'Color',[128/255 128/255 128/255]);
                end 
            elseif theta_scale(i,:) == 'lin'    
                unif=unifpdf(theta_Space(:,i),UnifMin(i),UnifMax(i));
                hprior=plot(theta_Space(:,i),unif,'-k','LineWidth',3);
                hold on
                grid on
                for i_processor=proc
                    ystart(i,i_processor)=unifpdf(theta_start(i,i_processor),UnifMin(i),UnifMax(i));
                    s_p=plot(theta_start(i,i_processor),ystart(i,i_processor),'o','MarkerSize',10,'LineWidth',3,'MarkerFaceColor',[128/255 128/255 128/255],'Color',[128/255 128/255 128/255]);
                end
            end
            
            harea=area(hprior.XData,unif);
            harea.EdgeColor='none';
            harea.FaceColor=[.5,.5,1];
            harea.FaceAlpha=.25;
            
        end
        
        
        if plot_Props == 1

            if SigmaProp_Norm(i,1:2) == 'ye'
                if theta_scale(i,:) == 'lin'
                    SigmaProp(i)=MuProp(i)*SigmaProp(i);
                elseif theta_scale(i,:) == 'log'
                    SigmaProp(i)=log10(MuProp(i))*SigmaProp(i);
                end
            end 
            
            if sum(Prop_Dist(i,:)== 'nor') == 3

                if theta_scale(i,:) == 'log'
                    for i_processor=proc
                        yy = logspace(log10(theta_start(i,i_processor))-abs(3*SigmaProp(i)),log10(theta_start(i,i_processor))+abs(3*SigmaProp(i)),nb_bins_hist+1);
                        norm=normpdf(log10(yy),log10(theta_start(i,i_processor)),abs(SigmaProp(i)));
                        norm=norm.*ystart(i,i_processor)./max(norm);
                        hprop=plot(log10(yy),norm,'LineWidth',3,'Color',[128/255 128/255 128/255]);
                        harea=area(hprop.XData,norm);
                        harea.EdgeColor='none';
                        harea.FaceColor=[128/255 128/255 128/255];
                        harea.FaceAlpha=.25;
                    end 
                elseif theta_scale(i,:) == 'lin'    
                    for i_processor=proc
                        yy = linspace(max(0,theta_start(i,i_processor)-3*abs(SigmaProp(i))),theta_start(i,i_processor)+3*abs(SigmaProp(i)),nb_bins_hist+1);
                        norm=normpdf(yy,theta_start(i,i_processor),abs(SigmaProp(i))); 
                        norm=norm.*ystart(i,i_processor)./max(norm);
                        hprop=plot(yy,norm,'LineWidth',3,'Color',[128/255 128/255 128/255]);
                        harea=area(hprop.XData,norm);
                        harea.EdgeColor='none';
                        harea.FaceColor=[128/255 128/255 128/255];
                        harea.FaceAlpha=.25;
                    end
                end
                
            end
            
            if PlottingFormat(1,:) == 'Visual'
                legend([hprior, s_p, hprop],{'Prior','Chains Starting Points', 'Starting Props'},'Location','eastoutside');
            elseif PlottingFormat(1,:) == 'Papers'
                legend([hprior, s_p, hprop],{'Prior','Chains Starting Points', 'Starting Props'},'Location','eastoutside','Interpreter','latex');
            end
            
        else
            
            if PlottingFormat(1,:) == 'Visual'
                legend([hprior, s_p],{'Prior','Chains Starting Points'},'Location','eastoutside');
            elseif PlottingFormat(1,:) == 'Papers'
                legend([hprior, s_p],{'Prior','Chains Starting Points'},'Location','eastoutside','Interpreter','latex');
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
            else 
                str_x = strcat('$log_{10}(',strtrim(theta_names(i,:)),')$');
            end
        elseif theta_scale(i,:) == 'lin'
            if PlottingFormat(1,:) == 'Visual'
                str_x = strtrim(theta_names(i,:));
            else 
                str_x = strcat('$',strtrim(theta_names(i,:)),'$');
            end
        end

        str_title = [strcat(str_x,space(1), 'Prior PDF')];
        if PlottingFormat(1,:) == 'Visual'
            xh = xlabel(str_x);
            yh = ylabel('Marginal PDF');
            th = title(str_title);
        elseif PlottingFormat(1,:) == 'Papers'
            xh = xlabel(str_x,'Interpreter','latex');
            yh = ylabel('Marginal PDF','Interpreter','latex');
            th = title(str_title,'Interpreter','latex');
        end 
        if PlottingFormat(1,:) == 'Visual'
            set(gca,'FontSize',30, 'FontName','PT Sans')
            set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
        elseif PlottingFormat(1,:) == 'Papers'
            set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
            set(gcf, 'PaperPositionMode', 'auto');
            set(gcf,'Renderer','painters')
        end 
        grid off
        pbaspect([1.5 1 1])
                
        if save_fig == 1
            str_save_fig = ['./fig/Priors/', str_x, '_Prior.fig'];
            savefig(i_figure, str_save_fig);
        end
        i_figure=i_figure+1;
    end
    
end
