function i_figure = plot_Output_Posterior_sets_fun(PlottingFormat, i_figure, save_fig, nb_of_sigma, plot_y_interval, ...
                                                    plot_Max_Like, plot_Max_Post, check_Output_Posterior, proc, x_check)      
    
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
                                                
    filename = './x_name.dat';
    delimiter = ' ';
    formatSpec = '%s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
    fclose(fileID);
    x_name_vec = dataArray{:, 1};
    clearvars filename delimiter formatSpec fileID dataArray ans;
    x_names = char(x_name_vec);
                                                
    filename = './y_name.dat';
    delimiter = ' ';
    formatSpec = '%s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
    fclose(fileID);
    y_name_vec = dataArray{:, 1};
    clearvars filename delimiter formatSpec fileID dataArray ans;
    y_names = char(y_name_vec);
                                                
    filename = './x_Nodes.dat';
    delimiter = ' ';
    startRow = 2;
    formatSpec = '%f%*s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
    fclose(fileID);
    x_nodes = dataArray{:, 1};
    clearvars filename delimiter startRow formatSpec fileID dataArray ans;
    
    filename = './like_node_sets.dat';
    delimiter = ' ';
    startRow = 2;
    formatSpec = '%f%*s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
    fclose(fileID);
    like_node_sets = dataArray{:, 1};
    clearvars filename delimiter startRow formatSpec fileID dataArray ans;
                                                
    filename = './x_y_data.dat';
    delimiter = ' ';
    startRow = 2;
    formatSpec = '%f%f%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
    fclose(fileID);
    x_data = dataArray{:, 1};
    y_data = dataArray{:, 2};
    clearvars filename delimiter startRow formatSpec fileID dataArray ans;

    filename = './x_y_data_orig.dat';
    delimiter = ' ';
    startRow = 2;
    formatSpec = '%f%f%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
    fclose(fileID);
    x_data_orig = dataArray{:, 1};
    y_data_orig = dataArray{:, 2};
    clearvars filename delimiter startRow formatSpec fileID dataArray ans;

    filename = './data_scale.dat';
    delimiter = '';
    formatSpec = '%s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter,  'ReturnOnError', false);
    fclose(fileID);
    data_scale = char(dataArray{:, 1});
    clearvars filename delimiter formatSpec fileID dataArray ans;
    
    if (data_scale == 'log')
        y_data = 10.^y_data;
    end

    filename = './data_sets.dat';
    delimiter = ' ';
    startRow = 2;
    formatSpec = '%f%*s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
    fclose(fileID);
    data_sets = dataArray{:, 1};
    clearvars filename delimiter startRow formatSpec fileID dataArray ans;                                                
                                                
    for i=proc

        filename = strcat('./x_y_max_like_',num2str(i),'.dat');
        delimiter = ' ';
        startRow = 2;
        formatSpec = '%f%f%[^\n\r]';
        fileID = fopen(filename,'r');
        dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
        fclose(fileID);
        x_Max_Like(:,i) = dataArray{:, 1};
        y_Max_Like(:,i) = dataArray{:, 2};
        clearvars filename delimiter startRow formatSpec fileID dataArray ans;

    end

    for i=proc

        filename = strcat('./x_y_max_posterior_',num2str(i),'.dat');
        delimiter = ' ';
        startRow = 2;
        formatSpec = '%f%f%[^\n\r]';
        fileID = fopen(filename,'r');
        dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
        fclose(fileID);
        x_Max_Post(:,i) = dataArray{:, 1};
        y_Max_Post(:,i) = dataArray{:, 2};
        clearvars filename delimiter startRow formatSpec fileID dataArray ans;

    end
     
    if (data_scale == 'log')
        y_Max_Post = 10.^y_Max_Post;
        y_Max_Like = 10.^y_Max_Like;
    end  
    
    y_sum= [];
    y_sq_sum= [];

    if plot_y_interval == 1

        y_sum=x_nodes.*0.d0;
        y_sq_sum=x_nodes.*0.d0;

        for i=proc

            filename = strcat('./x_y_Sum_',num2str(i),'.dat');
            delimiter = ' ';
            startRow = 2;
            formatSpec = '%f%f%[^\n\r]';
            fileID = fopen(filename,'r');
            dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
            fclose(fileID);
            y_sum = y_sum + dataArray{:, 2};
            clearvars filename delimiter startRow formatSpec fileID dataArray ans;

            filename = strcat('x_y_Sq_Sum_',num2str(i),'.dat');
            delimiter = ' ';
            startRow = 2;
            formatSpec = '%f%f%[^\n\r]';
            fileID = fopen(filename,'r');
            dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
            fclose(fileID);
            y_sq_sum = y_sq_sum + dataArray{:, 2};
            clearvars filename delimiter startRow formatSpec fileID dataArray ans;

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

        clearvars theta_temp like_temp post_temp

    end
    i_def=size(theta,1)*nb_alpha*max(nb_fwd,1);
    clearvars theta 
    
    
    nb_sets = size(like_node_sets,1) - 1;

    for i_sets = 1:nb_sets

        l_start = like_node_sets(i_sets)+1;
        l_end = like_node_sets(i_sets+1);
        d_start = data_sets(i_sets)+1;
        d_end = data_sets(i_sets+1);

        i_figure = plot_Output_Posterior_fun(PlottingFormat, i_figure, save_fig, nb_of_sigma, plot_y_interval, plot_Max_Like, plot_Max_Post, check_Output_Posterior, ...
            data_scale, x_names, plot_absc_min, plot_absc_max, plot_data_min, plot_data_max, y_names(i_sets), proc, ...
            x_check, y_sum(l_start:l_end), y_sq_sum(l_start:l_end), i_def, x_nodes(l_start:l_end), x_data(l_start:l_end), ...
            y_data(l_start:l_end), x_Max_Like(l_start:l_end), y_Max_Like(l_start:l_end), x_Max_Post(l_start:l_end), ...
            y_Max_Post(l_start:l_end), i_sets);
    
    end
                                                
end

