close all
clear all
clc
i_figure = 1;

save_fig       = 0;
show_titles    = 1;
PlottingFormat = 'Visual'; %('Visual' / 'Papers')

data_scale_plot= 'lin';

plot_debug = 0;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3 %%% POST-PROCESSING 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3.1.1 %%% Plotting Pearson Correlation Related Statistics
plot_Pearson_Correlation = 0;
    %%% 3.1.1.1 %%% Plotting Pearson Correlation
    plot_Correlations = 1;
    %%% 3.1.1.2 %%% Plotting QoI Output Variation Ratios
    plot_Variation_Contribution = 1;
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3.1.2 %%% Plotting Sobol Indices
plot_Sobol_Indices = 1;
    %%% 3.1.2.1 %%% Plotting Pearson Correlation
    plot_Sobol_Total_Order = 1;
    %%% 3.1.2.2 %%% Plotting QoI Output Variation Ratios
    plot_Sobol_First_Order = 1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3.2 %%% Plotting QoI Output Variation
plot_Variation = 0;
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.2.1 %%% Plotting QoI Output Variation Histogram
    plot_Output_Variation_Pcolor = 0;
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.2.2 %%% Plotting QoI Output Standard Deviation
    plot_Output_Sigma = 0;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Paths for Saving Plots    
if plot_debug ~= 1
    mkdir('./','fig')
    mkdir('./fig','Histogram')
    mkdir('./fig','Priors')
    mkdir('./fig','Correlations')
    mkdir('./fig','Sobol')
    mkdir('./fig','Variation')
end
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% READING SMUQ-OUTPUTS   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FUNDAMENTAL OUTPUTS

i_figure_start = i_figure;

filename = './nb_processor.dat';
delimiter = ' ';
startRow = 2;
formatSpec = '%f%*s%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
fclose(fileID);
nb_processors = dataArray{:, 1};
clearvars filename delimiter startRow formatSpec fileID dataArray ans;
proc = [1:1:nb_processors];

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

filename = './like_grid_extrema.dat';
delimiter = ' ';
startRow = 2;
formatSpec = '%f%f%*s%*s%*s%*s%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
fclose(fileID);
x_like_start = dataArray{:, 1};
x_like_end = dataArray{:, 2};
clearvars filename delimiter startRow formatSpec fileID dataArray ans;

filename = './x_name.dat';
delimiter = ' ';
formatSpec = '%s%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
fclose(fileID);
x_name_vec = dataArray{:, 1};
clearvars filename delimiter formatSpec fileID dataArray ans;
x_labels = x_name_vec;
x_names = char(x_name_vec);

filename = './y_name.dat';
delimiter = ' ';
formatSpec = '%s%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
fclose(fileID);
y_name_vec = dataArray{:, 1};
clearvars filename delimiter formatSpec fileID dataArray ans;
y_labels = y_name_vec;
y_names = char(y_name_vec);

filename = './data_scale.dat';
delimiter = '';
formatSpec = '%s%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter,  'ReturnOnError', false);
fclose(fileID);
data_scale = char(dataArray{:, 1});
clearvars filename delimiter formatSpec fileID dataArray ans;

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

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UPLOAD QUANTITIES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if plot_Pearson_Correlation == 1
    
    str_format = '%f';
    for i = 1:nb_dim-1
        str_format = strcat(str_format,'%f');
    end
    str_format = strcat(str_format,'%[^\n\r]');
    i_counter = 0;
    for i = proc
        i_counter = i_counter + 1;
        filename = strcat('./pearson_corr_',num2str(i),'.dat');
        delimiter = ' ';
        startRow = 2;
        formatSpec = str_format;
        fileID = fopen(filename,'r');
        dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'HeaderLines' ,startRow-1,'ReturnOnError', false);
        fclose(fileID);
        matrix_temp = [dataArray{1:end-1}];
        if i_counter == 1
            pearson_correlation = matrix_temp;
        else
            pearson_correlation = pearson_correlation+matrix_temp;
        end
        clearvars filename delimiter formatSpec fileID dataArray ans matrix_temp;
        
        filename = strcat('./theta_variance_',num2str(i),'.dat');
        delimiter = ' ';
        startRow = 2;
        formatSpec = str_format;
        fileID = fopen(filename,'r');
        dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'HeaderLines' ,startRow-1, 'ReturnOnError', false);
        fclose(fileID);
        matrix_temp = [dataArray{1:end-1}];
        if i_counter == 1
            theta_variance = matrix_temp;
        else
            theta_variance = theta_variance+matrix_temp;
        end
        clearvars filename delimiter formatSpec fileID dataArray ans matrix_temp;
        
        filename = strcat('./y_variance_',num2str(i),'.dat');
        delimiter = ' ';
        startRow = 2;
        formatSpec = '%f%[^\n\r]';
        fileID = fopen(filename,'r');
        dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
        fclose(fileID);
        matrix_temp = [dataArray{1:end-1}];
        if i_counter == 1
            y_variance = matrix_temp;
        else
            y_variance = y_variance+matrix_temp;
        end
        clearvars filename delimiter startRow formatSpec fileID dataArray ans matrix_temp;
        
    end
    clearvars i_counter
    
    pearson_correlation = pearson_correlation ./ length(proc);
    theta_variance = theta_variance ./ length(proc);
    y_variance = y_variance ./ length(proc);
   
end

if plot_Sobol_Indices == 1
    str_format = '%f';
    for i = 1:nb_dim-1
        str_format = strcat(str_format,'%f');
    end
    str_format = strcat(str_format,'%[^\n\r]');
    i_counter = 0;
    
    for i = proc
        i_counter = i_counter + 1;
        if plot_Sobol_Total_Order == 1
            filename = strcat('./sobol_total_order_',num2str(i),'.dat');
            startRow = 2;
            formatSpec = str_format;
            fileID = fopen(filename,'r');
            dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
            fclose(fileID);
            matrix_temp = [dataArray{1:end-1}];
            clearvars filename startRow formatSpec fileID dataArray ans;
            if i_counter == 1
                sobol_total_order = matrix_temp;
            else
                sobol_total_order = sobol_total_order+matrix_temp;
            end
            clearvars matrix_temp
        end
        
        if plot_Sobol_First_Order == 1
            filename = strcat('./sobol_first_order_',num2str(i),'.dat');
            startRow = 2;
            formatSpec = str_format;
            fileID = fopen(filename,'r');
            dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
            fclose(fileID);
            matrix_temp = [dataArray{1:end-1}];
            clearvars filename startRow formatSpec fileID dataArray ans;
            if i_counter == 1
                sobol_first_order = matrix_temp;
            else
                sobol_first_order = sobol_first_order+matrix_temp;
            end
            clearvars matrix_temp
        end    
    end
    clearvars i_counter
    
    if plot_Sobol_Total_Order == 1
        sobol_total_order = sobol_total_order / length(proc);
    end
    
    if plot_Sobol_First_Order == 1
        sobol_first_order = sobol_first_order / length(proc);
    end
    
end

if plot_Variation == 1
    
    if plot_Output_Sigma == 1
        i_counter = 0;
        for i = proc
            i_counter = i_counter + 1;
            filename = strcat('./y_variance_',num2str(i),'.dat');
            delimiter = ' ';
            startRow = 2;
            formatSpec = '%f%[^\n\r]';
            fileID = fopen(filename,'r');
            dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
            fclose(fileID);
            if i_counter == 1
                y_variance = dataArray{:, 1};
            else
                y_variance = y_variance + dataArray{:, 1};
            end
            clearvars filename delimiter startRow formatSpec fileID dataArray ans;
        end
        y_variance = y_variance / length(proc);
    end
    
    if plot_Output_Variation_Pcolor == 1
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
        hist_node_sets = dataArray{:, 1};
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
                Hist_Matrix(i,j) = Hist_Vector((j-1)*(nb_bins_histogram)+ i);
            end
        end
    end
    
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% POST-PROCESSING 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

theta_colors = distinguishable_colors(nb_dim);

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% POST-PROCESSING 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3.1.1 %%% Plotting Pearson Correlation Related Statistics
if plot_Pearson_Correlation == 1   
    
    %%% 3.1.1.1 %%% Plotting Pearson Correlation
    if plot_Correlations == 1
        i_figure = plot_Correlations_fun_sets(PlottingFormat, i_figure, save_fig, plot_absc_min, plot_absc_max, ...
            x_names, y_names, x_nodes, pearson_correlation, nb_dim, theta_colors, theta_names, like_node_sets);
    end

    %%% 3.1.1.2 %%% Plotting QoI Output Variation Ratios
    if plot_Variation_Contribution == 1       
        i_figure = plot_Variation_Contribution_fun_sets(PlottingFormat, i_figure, save_fig, plot_absc_min, plot_absc_max, ...
                                                    x_names, y_names, x_nodes, pearson_correlation, ...
                                                    theta_variance, y_variance, UnifMin, UnifMax, nb_dim, ...
                                                    theta_colors, theta_names, like_node_sets);                                                
    end
end

%%% 3.1.2 %%% Plotting Sobol Indices
if plot_Sobol_Indices == 1   
    
    %%% 3.1.2.1 %%% Plotting Sobol Total Order Indices
    if(plot_Sobol_Total_Order == 1)
        i_figure = plot_Sobol_Indices_sets(PlottingFormat, i_figure, save_fig, plot_absc_min, plot_absc_max, ...
            x_names, y_names, x_nodes, sobol_total_order, nb_dim, theta_colors, theta_names, like_node_sets,...
            't');
    end

    %%% 3.1.2.2 %%% Plotting Sobol First Order Indices
    if(plot_Sobol_First_Order == 1)
        i_figure = plot_Sobol_Indices_sets(PlottingFormat, i_figure, save_fig, plot_absc_min, plot_absc_max, ...
            x_names, y_names, x_nodes, sobol_first_order, nb_dim, theta_colors, theta_names, like_node_sets,...
            'f');
    end
    
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3.2 %%% Plotting QoI Output Variation
if plot_Variation == 1

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.2.1 %%% Plotting QoI Output Variation Histogram
    if plot_Output_Variation_Pcolor == 1     
        i_figure = plot_Output_Variation_Pcolor_fun_sets(PlottingFormat, i_figure, save_fig, ...
            plot_absc_min, plot_absc_max, plot_data_min, plot_data_max, data_scale, ...
            x_names, y_names, x_nodes,  y_Space_eff, y_Space, Hist_Matrix, hist_x_nodes, hist_node_sets);      
    end
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.2.2 %%% Plotting QoI Output Standard Deviation
    if plot_Output_Sigma == 1
       i_figure = plot_Output_Sigma_sets(PlottingFormat, i_figure, save_fig, ...
            plot_absc_min, plot_absc_max, x_names, y_names, x_nodes, like_node_sets, y_variance);
    end
    
end

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Plot Formating 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for i = i_figure_start:i_figure-1
%     figure(i)
% end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
