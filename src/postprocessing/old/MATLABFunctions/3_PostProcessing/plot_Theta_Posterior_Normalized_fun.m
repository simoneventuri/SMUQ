function i_figure = plot_Theta_Posterior_Normalized_fun(PlottingFormat, i_figure, proc, save_fig)

    filename = './theta_name.dat';
    delimiter = '\t';
    formatSpec = '%s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
    fclose(fileID);
    theta_name_vec = dataArray{:, 1};
    clearvars filename delimiter formatSpec fileID dataArray ans;
    theta_labels = theta_name_vec;

    nb_dim=size(theta,2);

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

        clearvars theta_temp like_temp post_temp

    end
    
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
    
    for i=1:nb_dim
        
        if theta_scale(i,:) == 'log' 
            theta_norm(:,i) = theta(:,i) ./ theta_data(i);
        elseif theta_scale(i,:) == 'lin' 
            theta_norm(:,i) = theta(:,i) ./ theta_data(i);
        end
        
        figure(i_figure)
        hist(theta_norm(:,i),linspace(0,2,1000));
        hold on
        grid on
        str_title = [theta_labels(i), 'Normalized Value'];
        if PlottingFormat(1,:) == 'Visual'
            title(str_title);
        elseif PlottingFormat(1,:) == 'Papers' 
            title(str_title,'Interpreter','latex');
        end
        if PlottingFormat(1,:) == 'Visual'
            set(gca,'FontSize',30, 'FontName','PT Sans')
            set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
        elseif PlottingFormat(1,:) == 'Papers'
            set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
            set(gcf, 'PaperPositionMode', 'auto');
        end 
                
        i_figure=i_figure+1;
        
    end
    
end
