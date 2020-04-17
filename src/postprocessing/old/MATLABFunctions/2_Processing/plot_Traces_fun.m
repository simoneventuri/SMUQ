function i_figure = plot_Traces_fun(PlottingFormat, i_figure, save_fig, proc, plot_Theta_Moments, Theta_Moments_Jump, clean_flag) 
        

    filename = './problem_parameters.dat';
    delimiter = ' ';
    formatSpec = '%f%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
    fclose(fileID);
    problem_parameters = dataArray{:, 1};
    clearvars filename delimiter formatSpec fileID dataArray ans;
    nb_dim=problem_parameters(1);
    nb_bins_histogram=problem_parameters(2);
    plot_absc_min=problem_parameters(3);
    plot_absc_max=problem_parameters(4);
    plot_data_min=problem_parameters(5);
    plot_data_max=problem_parameters(6);
    nb_alpha=problem_parameters(7);
    nb_fwd=problem_parameters(8);
    burn_in=problem_parameters(9);

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
                                
    nb_processors = length(proc);  
    chain_length = zeros(nb_processors+1,1);
    theta = [];
    if (clean_flag == 0)
        iii = 0;
        for i=proc
            iii = iii + 1;
            filename = strcat('./dirty_chain_',num2str(i),'.dat');
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
            Dirty_Chain = [dataArray{1:end-1}];
            clearvars filename startRow formatSpec fileID dataArray ans;
            theta_dirty_temp(:,:)=Dirty_Chain(:,1:nb_dim);
            theta = [theta; theta_dirty_temp];
            chain_length(iii+1) = size(theta_dirty_temp,1);
            clearvars theta_dirty_temp
        end
        
    elseif (clean_flag == 1)
        
        iii = 0;
        for i=proc
            iii = iii + 1;
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
            chain_length(iii+1) = size(theta_temp,1);
            theta = [theta; theta_temp];

            clearvars theta_temp

        end
    end
 
    for i = 1:nb_processors
       chain_length(i+1) = chain_length(i+1) + chain_length(i); 
    end
    
    for i=1:size(theta,2)

        figure(i_figure)
        hold on
        x=[1:1:size(theta,1)];
        y=theta(:,i);
        if theta_scale(i,:) == 'log'
            for j = 1:nb_processors
                semilogy(x(chain_length(j)+1:chain_length(j+1)),y(chain_length(j)+1:chain_length(j+1)),'-k','LineWidth',1.5)
            end
            y1=get(gca,'ylim');
            for j=1:nb_processors-1
                semilogy([chain_length(j+1) chain_length(j+1)],y1,'-.b','LineWidth',3);
            end
        elseif theta_scale(i,:) == 'lin'
            for j = 1:nb_processors
                plot(x(chain_length(j)+1:chain_length(j+1)),y(chain_length(j)+1:chain_length(j+1)),'-k','LineWidth',1.5)
            end
            y1=get(gca,'ylim');
            for j=1:nb_processors-1
                plot([chain_length(j+1) chain_length(j+1)],y1,'-.b','LineWidth',3);
            end
        end

        if plot_Theta_Moments==1
            for k = 1: nb_processors
                theta_mean_evo = [];
                theta_sigma_evo = [];
                for j=chain_length(k)+2:Theta_Moments_Jump:chain_length(k+1)
                    theta_mean_evo(j,i)=mean(theta(chain_length(k)+1:j,i));
                    theta_sigma_evo(j,i)=std(theta(chain_length(k)+1:j,i));
                end
                plot(x(chain_length(k)+2:Theta_Moments_Jump:chain_length(k+1)),theta_mean_evo(chain_length(k)+2:Theta_Moments_Jump:chain_length(k+1),i),'-r')
                hold on
                plot(x(chain_length(k)+2:Theta_Moments_Jump:chain_length(k+1)),theta_mean_evo(chain_length(k)+2:Theta_Moments_Jump:chain_length(k+1),i)+3*theta_sigma_evo(chain_length(k)+2:Theta_Moments_Jump:chain_length(k+1),i),':r')
                plot(x(chain_length(k)+2:Theta_Moments_Jump:chain_length(k+1)),theta_mean_evo(chain_length(k)+2:Theta_Moments_Jump:chain_length(k+1),i)-3*theta_sigma_evo(chain_length(k)+2:Theta_Moments_Jump:chain_length(k+1),i),':r')
            end
        end

        str_y = strtrim(theta_names(i,:));
        if clean_flag==0
            str_title = ['Dirty Chain Evolution for ', str_y];
        elseif clean_flag==1
            str_title = ['Clean Chain Evolution for ', str_y];
        end 
        if PlottingFormat(1,:) == 'Visual'
            xlabel('Chain step');
            ylabel(str_y);    
        %    if show_titles==1
                title(str_title)
        %    end
        elseif PlottingFormat(1,:) == 'Papers'
            xlabel('Chain step','Interpreter','latex');
            ylabel(str_y,'Interpreter','latex');    
        %    if show_titles==1
                title(str_title,'Interpreter','latex')
        %    end
        end
        set(gca,'xlim',[0 x(end)]);
        if PlottingFormat(1,:) == 'Visual'
            set(gca,'FontSize',30, 'FontName','PT Sans')
            set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
        elseif PlottingFormat(1,:) == 'Papers'
            set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
            set(gcf, 'PaperPositionMode', 'auto');
        end 
        if save_fig == 1
            str_save_fig = ['./fig/Correlations/', str_x, '_Autocorrelation.fig'];
            savefig(i_figure, str_save_fig);
        end
        i_figure=i_figure+1;

    end
    
end
