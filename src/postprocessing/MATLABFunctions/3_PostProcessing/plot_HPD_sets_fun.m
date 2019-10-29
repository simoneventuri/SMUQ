function i_figure = plot_HPD_sets_fun(PlottingFormat, i_figure, save_fig, proc, nb_pcolor, HPD_tol, check_Output_Posterior, x_check )

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
    
    if ( data_scale == 'log' )
        y_data = 10.^y_data;
        y_data_orig = 10.^y_data_orig;
    end              
       
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
           
    if ( data_scale == 'log' )
        y_Max_Post = 10.^y_Max_Post;
        y_Max_Like = 10.^y_Max_Like;
    end  
    
    if data_scale == 'lin'
       y_Space_temp = linspace(plot_data_min,plot_data_max,nb_bins_histogram+1);
       y_Space_min  = (y_Space_temp(1)+y_Space_temp(2))/2;
       y_Space_max  = (y_Space_temp(end)+y_Space_temp(end-1))/2;
       y_Space_eff  = linspace(y_Space_min,y_Space_max,nb_bins_histogram);
       y_Space      = linspace(y_Space_min,y_Space_max,nb_bins_histogram);
    elseif data_scale == 'log'
       y_Space_temp = linspace(log10(plot_data_min),log10(plot_data_max),nb_bins_histogram+1);
       y_Space_min  = ((y_Space_temp(1)+y_Space_temp(2))/2);
       y_Space_max  = ((y_Space_temp(end)+y_Space_temp(end-1))/2);
       y_Space_eff  = linspace(y_Space_min,y_Space_max,nb_bins_histogram);
       y_Space      = 10.^linspace(y_Space_min,y_Space_max,nb_bins_histogram);
    end

    filename = './Hist_x_Nodes.dat';
    delimiter = ' ';
    startRow = 2;
    formatSpec = '%f%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
    fclose(fileID);
    hist_x_nodes = dataArray{:, 1};
    clearvars filename delimiter startRow formatSpec fileID dataArray ans;

    filename = './Hist_Nodes_Sets.dat';
    delimiter = ' ';
    startRow = 2;
    formatSpec = '%f%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
    fclose(fileID);
    hist_nodes_sets = dataArray{:, 1};
    clearvars filename delimiter startRow formatSpec fileID dataArray ans;

    Hist_Vector=zeros(nb_bins_histogram*(length(hist_x_nodes)),1);
    for i=proc

        filename = strcat('./Hist_for_pcolor_',num2str(i),'.dat');
        delimiter = ' ';
        formatSpec = '%f%[^\n\r]';
        fileID = fopen(filename,'r');
        dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'EmptyValue' ,NaN, 'ReturnOnError', false);
        fclose(fileID);
        Hist_Vector = Hist_Vector + dataArray{:, 1};
        clearvars filename delimiter formatSpec fileID dataArray ans;

    end

    for j=1:length(hist_x_nodes)
        for i=1:nb_bins_histogram
            Hist_Matrix(i,j) = Hist_Vector( (j-1)*(nb_bins_histogram)+ i);
        end 
    end
    
   nb_sets = size(like_node_sets,1)-1;

   for i_sets = 1:nb_sets
       l_start = like_node_sets(i_sets)+1;
       l_end = like_node_sets(i_sets+1);
       h_start = hist_nodes_sets(i_sets)+1;
       h_end = hist_nodes_sets(i_sets+1);
       i_counter = 1;
       for i = h_start:h_end
           if round(hist_x_nodes(i),4) == round(x_nodes(l_start+i_counter-1),4)
               vec(i_counter) = i;
               i_counter = i_counter + 1;
               
           end
       end

       i_figure = plot_HPD_fun(PlottingFormat, i_figure, save_fig, nb_pcolor, HPD_tol, check_Output_Posterior, x_check, ...
                               plot_absc_min, plot_absc_max, x_names, x_data(l_start:l_end), x_nodes(l_start:l_end), data_scale, ...
                               y_data(l_start:l_end), y_Space_eff, Hist_Matrix(:,vec),i_sets);

   end
    
end

