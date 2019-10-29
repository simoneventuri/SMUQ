clear all
close all
i_figure=1;
clc

save_fig       = 0
show_titles    = 1
PlottingFormat = 'Visual' %('Visual' / 'Papers')

data_scale_plot='lin'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 0 %%% DEBUGGING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot_Debug = 0
%     proc_Debug = [1]
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%% Debug Output
%     debug_output = 0
%         plot_y = 0
%             plot_y_Data    = 1
%             plot_y_Nominal = 0
%             plot_Like_Grid = 0
%             plot_y_Start   = 1
%             plot_y_Current = 1
%             plot_y_Prop    = 1
%             plot_y_Sum     = 0
%             plot_y_Sq_Sum  = 0
%                nb_of_sigma = 3
%             plot_abs_error = 0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Debug Chain
%     debug_chain_space     = 0
%     debug_chain_posterior = 0
%     debug_chain_clean     = 0
%         dim_vector_debug=[1:7];
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%% Plot Tau Histogram
%     verify_tau = 0
        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 1 %%% PRE-PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 1.1 %%% Plotting Priors and Parameters Chains Starting Values
plot_Priors = 1
    plot_Props = 1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 2 %%% PROCESSING  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
analyze_Theta_Chains = 0
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 2.1 %%% Visual Check for Parameters Traces
    plot_Traces = 1
        plot_Dirty_Traces = 0
        plot_Clean_Traces = 0
            plot_Theta_Moments = 1
            Theta_Moments_Jump = 5
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 2.2 %%% Visual Check for the Parameters Spaces
    plot_Theta_Spaces = 0
        dim_vector=[1,2,3]
        plot_Theta_Spaces_Dirty = 0
        plot_Theta_Spaces_Clean = 1
            Theta_Spaces_pcolor_nbins = 100
            plot_Theta_Spaces_Points               = 1
            plot_Theta_Spaces_Evo                  = 1
                nb_DR     = 3
                Evo_start = 5
                Evo_jump  = 40
                Evo_cut   = 1
            plot_Theta_Spaces_Hist                 = 1
            plot_Theta_Spaces_Pcolor               = 1
            plot_Theta_Spaces_Posterior_Surf       = 1
            plot_Theta_Spaces_Posterior_Pcolor     = 1   
            plot_Theta_Spaces_Posterior_Normalized = 1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 2.3 %%% Parameters Correlations Bars
    compute_ThetaTheta_Corr = 1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 2.4 %%% Parameters Auto-Correlations
    plot_Autocorrelations = 0
        plot_Theta_Dirty_Autocorrelations = 1
        plot_Theta_Clean_Autocorrelations = 1
            max_lag_dirty=100;
            max_lag_clean=100;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3 %%% POST-PROCESSING 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot_Posterior = 1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.1.1 %%% Plotting Parameters Posterior Distributions
    plot_Theta_Posterior = 1
        plot_theta_max_like      = 0
        plot_theta_max_post      = 1
        plot_theta_hist          = 0
        plot_theta_nb_bins_hist  = 200
        plot_theta_pdf           = 1
        
    plot_Theta_Posterior_Bspline = 0
        theta_post_cpts = [1,2,3,4,5;6,7,8,9,10]
        theta_cpts_range= [300, 800;300, 3000]
        theta_spline_nb_bins = [800,200]
        theta_spline_nb_points = 1000

    plot_Theta_Posterior_Gauss1 = 0
        theta_post_gauss1 = [1,2,3;4,5,6]
        theta_gauss1_range= [200, 800;200, 3000]
        theta_gauss1_nb_bins = [800,200]
        theta_gauss1_nb_points = 1000

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.1.2 %%% Plotting Parameters Posterior Distributions Normalized with respect to Parameters Data Values
    plot_Theta_Posterior_Normalized = 0
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.2.1 %%% Plot Output Posterior Mean and Confidence Interval 
    plot_Output_Posterior = 1
        plot_y_interval = 1
            nb_of_sigma = 3
        plot_Max_Like   = 1
        plot_Max_Post   = 1 
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.2.2 %%% Plot Output Posterior Histogram
    plot_Output_Posterior_Pcolor = 0
        nb_pcolor = 0.01
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.2.3 %%%  Checking the Punctual Output Posterior Histogram
    check_Output_Posterior = 0
        x_check=[140 180];
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.2.4 %%% Plot Highest Probability Density (HPD) Interval for Output Posterior
    plot_HPD = 1
        HPD_tol = 0.05


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 4 %%% EXTERNAL CODE PARTICULARIZED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot_ExtCode = 0


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Paths for Saving Plots
if plot_Debug ~= 1 
    mkdir('./fig')
    mkdir('./fig','Important')
    mkdir('./fig/Important','Correlations')
    mkdir('./fig','Priors')
    mkdir('./fig','Correlations')
    mkdir('./fig','Posteriors')
    mkdir('./fig/Posteriors','Parameters')
    mkdir('./fig/Posteriors','QoI')
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% READING SMUQ-OUTPUTS   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% FUNDAMENTAL OUTPUTS
i_figure_start = i_figure;

% filename = './nb_processor.dat';
% delimiter = ' ';
% startRow = 2;
% formatSpec = '%f%*s%[^\n\r]';
% fileID = fopen(filename,'r');
% dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
% fclose(fileID);
% nb_processors = dataArray{:, 1};
% proc = 1:1:nb_processors;
% clearvars filename delimiter startRow formatSpec fileID dataArray ans;

filename = './def_processors.dat';
delimiter = ' ';
formatSpec = '%f%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
fclose(fileID);
proc = dataArray{:, 1};
proc = proc';
clearvars filename delimiter formatSpec fileID dataArray ans;
nb_processors=length(proc);

% filename = './problem_parameters.dat';
% delimiter = ' ';
% formatSpec = '%f%[^\n\r]';
% fileID = fopen(filename,'r');
% dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
% fclose(fileID);
% problem_parameters = dataArray{:, 1};
% clearvars filename delimiter formatSpec fileID dataArray ans;
% nb_dim=problem_parameters(1);
% nb_bins_histogram=problem_parameters(2);
% plot_absc_min=problem_parameters(3);
% plot_absc_max=problem_parameters(4);
% plot_data_min=problem_parameters(5);
% plot_data_max=problem_parameters(6);
% nb_alpha=problem_parameters(7);
% nb_fwd=problem_parameters(8);
% burn_in=problem_parameters(9);
% 
% filename = './like_grid_extrema.dat';
% delimiter = ' ';
% startRow = 2;
% formatSpec = '%f%f%*s%*s%*s%*s%[^\n\r]';
% fileID = fopen(filename,'r');
% dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
% fclose(fileID);
% x_like_start = dataArray{:, 1};
% x_like_end = dataArray{:, 2};
% clearvars filename delimiter startRow formatSpec fileID dataArray ans;
% 
% filename = './x_name.dat';
% delimiter = ' ';
% formatSpec = '%s%[^\n\r]';
% fileID = fopen(filename,'r');
% dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
% fclose(fileID);
% x_name_vec = dataArray{:, 1};
% clearvars filename delimiter formatSpec fileID dataArray ans;
% x_labels = x_name_vec;
% x_names = char(x_name_vec);
% 
% filename = './y_name.dat';
% delimiter = ' ';
% formatSpec = '%s%[^\n\r]';
% fileID = fopen(filename,'r');
% dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
% fclose(fileID);
% y_name_vec = dataArray{:, 1};
% clearvars filename delimiter formatSpec fileID dataArray ans;
% y_labels = y_name_vec;
% y_names = char(y_name_vec);
% 
% filename = './data_scale.dat';
% delimiter = '';
% formatSpec = '%s%[^\n\r]';
% fileID = fopen(filename,'r');
% dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter,  'ReturnOnError', false);
% fclose(fileID);
% data_scale = char(dataArray{:, 1});
% clearvars filename delimiter formatSpec fileID dataArray ans;
% 
% filename = './theta_name.dat';
% delimiter = '\t';
% formatSpec = '%s%[^\n\r]';
% fileID = fopen(filename,'r');
% dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
% fclose(fileID);
% theta_name_vec = dataArray{:, 1};
% clearvars filename delimiter formatSpec fileID dataArray ans;
% theta_labels = theta_name_vec;
% theta_names = char(theta_name_vec);
%         
% filename = './theta_data.dat';
% delimiter = ' ';
% startRow = 2;
% formatSpec = '%s%f%*s%*s%[^\n\r]';
% fileID = fopen(filename,'r');
% dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
% fclose(fileID);
% theta_scale = char(dataArray{:, 1});
% theta_data = dataArray{:, 2};
% clearvars filename delimiter startRow formatSpec fileID dataArray ans;
% 
% theta_max_like = [];
% theta_max_post = [];

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UPLOAD QUANTITIES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filename = './ExtCode.dat';
delimiter = '';
formatSpec = '%s%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter,  'ReturnOnError', false);
fclose(fileID);
ExtCode_cell = dataArray{:, 1};
clearvars filename delimiter formatSpec fileID dataArray ans;
ExtCode = char(ExtCode_cell);

%%
% if (plot_Debug == 1)
%     
%     proc=[1:nb_processors];
%     if proc_Debug == [0]
%         clear proc_Debug
%         proc_Debug = proc
%     end
%     nb_processors=length(proc);
%     
%     if (plot_y_Current == 1)
% 
%         x_current=[];
%         y_current=[];
% 
%         for i=proc_Debug
% 
%             filename = strcat('./x_y_current_',num2str(i),'.dat');
%             delimiter = ' ';
%             startRow = 2;
%             formatSpec = '%f%f%[^\n\r]';
%             fileID = fopen(filename,'r');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%             fclose(fileID);
%             x_current(:,i) = dataArray{:, 1};
%             y_current(:,i) = dataArray{:, 2};
%             clearvars filename delimiter startRow formatSpec fileID dataArray ans;
% 
%         end
% 
%     end
% 
%     if (plot_y_Prop == 1)
%         
%         x_prop=[];
%         y_prop=[];
% 
%         for i=proc_Debug
% 
%             filename = strcat('./x_y_prop_',num2str(i),'.dat');
%             delimiter = ' ';
%             startRow = 2;
%             formatSpec = '%f%f%[^\n\r]';
%             fileID = fopen(filename,'r');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%             fclose(fileID);
%             x_prop(:,i) = dataArray{:, 1};
%             y_prop(:,i) = dataArray{:, 2};
%             clearvars filename delimiter startRow formatSpec fileID dataArray ans;
% 
%         end
% 
%     end
%     
%     if (plot_y_Data == 1)
% 
%         filename = './x_y_data.dat';
%         delimiter = ' ';
%         startRow = 2;
%         formatSpec = '%f%f%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         fclose(fileID);
%         x_data = dataArray{:, 1};
%         y_data = dataArray{:, 2};
%         clearvars filename delimiter startRow formatSpec fileID dataArray ans;
% 
%     end
%     
%     if (plot_y_Start == 1)
% 
%         x_start=[];
%         y_start=[];
% 
%         for i=proc_Debug
% 
%             filename = strcat('./x_y_start_',num2str(i),'.dat');
%             delimiter = ' ';
%             startRow = 2;
%             formatSpec = '%f%f%[^\n\r]';
%             fileID = fopen(filename,'r');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%             fclose(fileID);
%             x_start(:,i) = dataArray{:, 1};
%             y_start(:,i) = dataArray{:, 2};
%             clearvars filename delimiter startRow formatSpec fileID dataArray ans;
% 
%         end
%         
%     else
%         
%         x_start=x_data;
%         y_start=y_data;
% 
%     end
%     
%     
%     if (plot_Priors == 1) || (plot_Theta_Posterior == 1) || debug_chain_space == 1 || debug_chain_posterior == 1 || debug_chain_clean == 1
% 
%         filename = './Prior.dat';
%         startRow = 2;
%         formatSpec = '%20s%20s%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         dataArray{1} = strtrim(dataArray{1});
%         dataArray{2} = strtrim(dataArray{2});
%         fclose(fileID);
%         Prior_Dist = char(dataArray{:, 1});
%         SigmaPrior_Norm = char(dataArray{:, 2});
%         clearvars filename startRow formatSpec fileID dataArray ans;
% 
%         filename = './Prior.dat';
%         startRow = 2;
%         formatSpec = '%*40s%20f%20f%20f%f%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         fclose(fileID);
%         MuPrior = dataArray{:, 1};
%         SigmaPrior = dataArray{:, 2};
%         UnifMin = dataArray{:, 3};
%         UnifMax = dataArray{:, 4};
%         clearvars filename startRow formatSpec fileID dataArray ans;
% 
%         for i=1:nb_dim
%             if SigmaPrior_Norm(i,1:2) == 'ye'
%                 if theta_scale(i,:) == 'lin'
%                     SigmaPrior(i)=MuPrior(i)*SigmaPrior(i);
%                 elseif theta_scale(i,:) == 'log'
%                     SigmaPrior(i)=log10(MuPrior(i))*SigmaPrior(i);
%                 end
%             end 
%         end
%         
%     end
%     
%     if (plot_Props == 1)
% 
%         filename = './Proposal.dat';
%         startRow = 2;
%         formatSpec = '%*40s%20f%f%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         fclose(fileID);
%         MuProp = dataArray{:, 1};
%         SigmaProp = dataArray{:, 2};
%         clearvars filename startRow formatSpec fileID dataArray ans;
% 
%         filename = './Proposal.dat';
%         startRow = 2;
%         formatSpec = '%20s%20s%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         dataArray{1} = strtrim(dataArray{1});
%         dataArray{2} = strtrim(dataArray{2});
%         fclose(fileID);
%         Prop_Dist = char(dataArray{:, 1});
%         SigmaProp_Norm = char(dataArray{:, 2});
%         clearvars filename formatSpec fileID dataArray ans;
%         
%         for i=proc
% 
%             filename = strcat('./theta_start_',num2str(i),'.dat');
%             startRow = 2;
%             endRow = 2;
%             str_format='%20f';
%             for ii=2:nb_dim+1
%                 str_format = strcat(str_format,'%20f');
%             end
%             str_format = strcat(str_format,'%f%[^\n\r]');
%             formatSpec = str_format;
%             fileID = fopen(filename,'r');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%             fclose(fileID);
%             Dirty_Chain = [dataArray{1:end-1}];
%             clearvars filename startRow formatSpec fileID dataArray ans;
%             theta_start(:,i)=Dirty_Chain(1,1:nb_dim);
% 
%         end
% 
% 
%     end
%     
%     if debug_chain_space == 1 || debug_chain_posterior == 1 || debug_chain_clean == 1
%         
%         LengthVec=[0];
%         for i=proc
% 
%             filename = strcat('./ChainDebug_',num2str(i),'.dat');
%             startRow = 2;
%             formatSpec = '%10f%10f%[^\n\r]';
%             fileID = fopen(filename,'r');
%             textscan(fileID, '%[^\n\r]', startRow-1, 'WhiteSpace', '', 'ReturnOnError', false, 'EndOfLine', '\r\n');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'ReturnOnError', false);
%             fclose(fileID);
%             cellsz = cellfun(@size,dataArray,'uni',false);
%             LengthVec = [LengthVec, cellsz{1}(1)];
%             maxLength = max(LengthVec);
%             clearvars filename startRow formatSpec fileID dataArray ans;
%         
%         end
%         
%         i_MCMC     = zeros(maxLength,length(proc));
%         i_DR       = zeros(maxLength,length(proc));
%         ThetaDebug = zeros(maxLength,nb_dim,length(proc));
%         Like       = zeros(maxLength,length(proc));
%         Post       = zeros(maxLength,length(proc));
%         DRPar      = zeros(maxLength,length(proc));
%         Accept     = cell(maxLength,length(proc));
%         AccRate    = zeros(maxLength,length(proc));
% 
%         for i=proc
% 
%             filename = strcat('./ChainDebug_',num2str(i),'.dat');
% 
%             startRow = 2;
%             formatSpec = '%10f%10f%[^\n\r]';
%             fileID = fopen(filename,'r');
%             textscan(fileID, '%[^\n\r]', startRow-1, 'WhiteSpace', '', 'ReturnOnError', false, 'EndOfLine', '\r\n');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'ReturnOnError', false);
%             fclose(fileID);
%             i_MCMC(1:LengthVec(proc(i)+1),i) = dataArray{:, 1};
%             i_DR(1:LengthVec(proc(i)+1),i)   = dataArray{:, 2};
%             clearvars formatSpec fileID dataArray ans;
% 
%             str_format='%*20s%20f';
%             for ii=2:nb_dim-1
%                 str_format = strcat(str_format,'%20f');
%             end
%             str_format = strcat(str_format,'%f%[^\n\r]');
%             formatSpec = str_format;
%             fileID = fopen(filename,'r');
%             textscan(fileID, '%[^\n\r]', startRow-1, 'WhiteSpace', '', 'ReturnOnError', false, 'EndOfLine', '\r\n');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'ReturnOnError', false);
%             fclose(fileID);
%             ThetaDebug(1:LengthVec(proc(i)+1),:,i) = [dataArray{1:end-1}];
%             clearvars formatSpec fileID dataArray ans;
% 
%             str_format='%*20s%*20s';
%             for ii=2:nb_dim
%                 str_format = strcat(str_format,'%*20s');
%             end
%             str_format = strcat(str_format,'%20f%20f%10f%[^\n\r]');
%             formatSpec = str_format;
%             fileID = fopen(filename,'r');
%             textscan(fileID, '%[^\n\r]', startRow-1, 'WhiteSpace', '', 'ReturnOnError', false, 'EndOfLine', '\r\n');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'ReturnOnError', false);
%             fclose(fileID);
%             Like(1:LengthVec(proc(i)+1),i)  = dataArray{:, 1};
%             Post(1:LengthVec(proc(i)+1),i)  = dataArray{:, 2};
%             DRPar(1:LengthVec(proc(i)+1),i) = dataArray{:, 3};
%             clearvars formatSpec fileID dataArray ans;
% 
%             str_format='%*20s%*20s';
%             for ii=2:nb_dim
%                 str_format = strcat(str_format,'%*20s');
%             end
%             str_format = strcat(str_format,'%*20s%*20s%*10s%7s%[^\n\r]');
%             formatSpec = str_format;
%             fileID = fopen(filename,'r');
%             textscan(fileID, '%[^\n\r]', startRow-1, 'WhiteSpace', '', 'ReturnOnError', false, 'EndOfLine', '\r\n');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'ReturnOnError', false);
%             dataArray{1} = strtrim(dataArray{1});
%             fclose(fileID);
%             Accept(1:LengthVec(proc(i)+1),i) = dataArray{:, 1};
%             clearvars formatSpec fileID dataArray ans;
% 
%             str_format='%*20s%*20s';
%             for ii=2:nb_dim
%                 str_format = strcat(str_format,'%*20s');
%             end
%             str_format = strcat(str_format,'%*20s%*20s%*10s%*7s%f%[^\n\r]');
%             formatSpec = str_format;
%             fileID = fopen(filename,'r');
%             textscan(fileID, '%[^\n\r]', startRow-1, 'WhiteSpace', '', 'ReturnOnError', false, 'EndOfLine', '\r\n');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'ReturnOnError', false);
%             fclose(fileID);
%             AccRate(1:LengthVec(proc(i)+1),i) = dataArray{:, 1};
%             clearvars filename startRow formatSpec fileID dataArray ans;
% 
%         end 
%         
%     end
% %%    
% else
%     
%     filename = './def_processors.dat';
%     delimiter = ' ';
%     formatSpec = '%f%[^\n\r]';
%     fileID = fopen(filename,'r');
%     dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
%     fclose(fileID);
%     proc = dataArray{:, 1};
%     proc = proc';
%     clearvars filename delimiter formatSpec fileID dataArray ans;
%     nb_processors=length(proc);
% 
%     if (plot_y_Data == 1)
% 
%         filename = './x_y_data.dat';
%         delimiter = ' ';
%         startRow = 2;
%         formatSpec = '%f%f%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         fclose(fileID);
%         x_data = dataArray{:, 1};
%         y_data = dataArray{:, 2};
%         clearvars filename delimiter startRow formatSpec fileID dataArray ans;
%         
%         filename = './x_y_data_orig.dat';
%         delimiter = ' ';
%         startRow = 2;
%         formatSpec = '%f%f%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         fclose(fileID);
%         x_data_orig = dataArray{:, 1};
%         y_data_orig = dataArray{:, 2};
%         clearvars filename delimiter startRow formatSpec fileID dataArray ans;
%         
%         filename = './data_sets.dat';
%         delimiter = ' ';
%         startRow = 2;
%         formatSpec = '%f%*s%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         fclose(fileID);
%         data_sets = dataArray{:, 1};
%         clearvars filename delimiter startRow formatSpec fileID dataArray ans;
%         
%     end
% 
%     if (plot_y_Start == 1)
% 
%         x_start=[];
%         y_start=[];
% 
%         for i=proc
% 
%             filename = strcat('./x_y_start_',num2str(i),'.dat');
%             delimiter = ' ';
%             startRow = 2;
%             formatSpec = '%f%f%[^\n\r]';
%             fileID = fopen(filename,'r');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%             fclose(fileID);
%             x_start(:,i) = dataArray{:, 1};
%             y_start(:,i) = dataArray{:, 2};
%             clearvars filename delimiter startRow formatSpec fileID dataArray ans;
% 
%         end
% 
%     end
% 
%     if (plot_Priors == 1) || (plot_Theta_Posterior == 1)
% 
%         filename = './Prior.dat';
%         startRow = 2;
%         formatSpec = '%20s%20s%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         dataArray{1} = strtrim(dataArray{1});
%         dataArray{2} = strtrim(dataArray{2});
%         fclose(fileID);
%         Prior_Dist = char(dataArray{:, 1});
%         SigmaPrior_Norm = char(dataArray{:, 2});
%         clearvars filename startRow formatSpec fileID dataArray ans;
% 
%         filename = './Prior.dat';
%         startRow = 2;
%         formatSpec = '%*40s%20f%20f%20f%f%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         fclose(fileID);
%         MuPrior = dataArray{:, 1};
%         SigmaPrior = dataArray{:, 2};
%         UnifMin = dataArray{:, 3};
%         UnifMax = dataArray{:, 4};
%         clearvars filename startRow formatSpec fileID dataArray ans;
% 
%         for i=1:nb_dim
%             if SigmaPrior_Norm(i,1:2) == 'ye'
%                 if theta_scale(i,:) == 'lin'
%                     SigmaPrior(i)=MuPrior(i)*SigmaPrior(i);
%                 elseif theta_scale(i,:) == 'log'
%                     SigmaPrior(i)=log10(MuPrior(i))*SigmaPrior(i);
%                 end
%             end 
%         end
%         
%         if (plot_theta_max_like == 1)
%             
%             for i=proc
% 
%                 filename = strcat('./theta_max_like_',num2str(i),'.dat');
%                 startRow = 2;
%                 formatSpec = '%20f';
%                 for j=1:size(Prior_Dist,1)-2
%                     formatSpec = strcat(formatSpec,'%20f');
%                 end
%                 formatSpec = strcat(formatSpec,'%f%[^\n\r]');
%                 fileID = fopen(filename,'r');
%                 dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
%                 fclose(fileID);
%                 theta_max_like(:,i) = [dataArray{1:end-1}];
%                 clearvars filename startRow formatSpec fileID dataArray ans;
% 
%             end
%             
%         end
%         
%         if (plot_theta_max_post == 1)
%             
%             for i=proc
% 
%                 filename = strcat('./theta_max_post_',num2str(i),'.dat');
%                 startRow = 2;
%                 formatSpec = '%20f';
%                 for j=1:size(Prior_Dist,1)-2
%                     formatSpec = strcat(formatSpec,'%20f');
%                 end
%                 formatSpec = strcat(formatSpec,'%f%[^\n\r]');
%                 fileID = fopen(filename,'r');
%                 dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
%                 fclose(fileID);
%                 theta_max_post(:,i) = [dataArray{1:end-1}];
%                 clearvars filename startRow formatSpec fileID dataArray ans;
% 
%             end
%             
%         end
% 
%     end
% 
%     if (plot_Props == 1)
% 
%         filename = './Proposal.dat';
%         startRow = 2;
%         formatSpec = '%*40s%20f%f%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         fclose(fileID);
%         MuProp = dataArray{:, 1};
%         SigmaProp = dataArray{:, 2};
%         clearvars filename startRow formatSpec fileID dataArray ans;
% 
%         filename = './Proposal.dat';
%         startRow = 2;
%         formatSpec = '%20s%20s%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         dataArray{1} = strtrim(dataArray{1});
%         dataArray{2} = strtrim(dataArray{2});
%         fclose(fileID);
%         Prop_Dist = char(dataArray{:, 1});
%         SigmaProp_Norm = char(dataArray{:, 2});
%         clearvars filename formatSpec fileID dataArray ans;
% 
%     end
% 
%     if (plot_Dirty_Traces == 1)
% 
%         theta_dirty = [];
%         like_dirty  = [];
%         post_dirty  = [];
%         theta_start = [];
% 
%         like_dirty_max_value_temp = 0;
%         post_dirty_max_value_temp = 0;
% 
%         for i=proc
% 
%             filename = strcat('./dirty_chain_',num2str(i),'.dat');
%             startRow = 2;
%             str_format='%20f';
%             for ii=2:nb_dim+1
%                 str_format = strcat(str_format,'%20f');
%             end
%             str_format = strcat(str_format,'%f%[^\n\r]');
%             formatSpec = str_format;
%             fileID = fopen(filename,'r');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%             fclose(fileID);
%             Dirty_Chain = [dataArray{1:end-1}];
%             clearvars filename startRow formatSpec fileID dataArray ans;
%             theta_dirty_temp(:,:)=Dirty_Chain(:,1:nb_dim);
%             like_dirty_temp(:,1)=Dirty_Chain(:,nb_dim+1);
%             post_dirty_temp(:,1)=Dirty_Chain(:,nb_dim+2);
%             theta_start(:,i)=Dirty_Chain(1,1:nb_dim);
% 
%             [like_dirty_max_value_temp_temp, like_dirty_max_pos_temp_temp]=max(like_dirty_temp);
%             [post_dirty_max_value_temp_temp, post_dirty_max_pos_temp_temp]=max(post_dirty_temp);
%             if like_dirty_max_value_temp_temp > like_dirty_max_value_temp
%                 i_proc_like_dirty_max = i;
%                 like_dirty_max_value_temp = like_dirty_max_value_temp_temp;
%             end
%             if post_dirty_max_value_temp_temp > post_dirty_max_value_temp
%                 i_proc_post_dirty_max = i;
%                 post_dirty_max_value_temp = post_dirty_max_value_temp_temp;
%             end
% 
%             theta_dirty = [theta_dirty; theta_dirty_temp];
%             like_dirty  = [like_dirty; like_dirty_temp];
%             post_dirty  = [post_dirty; post_dirty_temp];
% 
%             clearvars theta_dirty_temp like_dirty_temp post_dirty_temp
% 
%         end
% 
%         [like_dirty_max_value, like_dirty_max_pos]=max(like_dirty);
%         [post_dirty_max_value, post_dirty_max_pos]=max(post_dirty);
% 
%     else
% 
%         for i=proc
% 
%             filename = strcat('./dirty_chain_',num2str(i),'.dat');
%             startRow = 2;
%             endRow = 2;
%             str_format='%20f';
%             for ii=2:nb_dim+1
%                 str_format = strcat(str_format,'%20f');
%             end
%             str_format = strcat(str_format,'%f%[^\n\r]');
%             formatSpec = str_format;
%             fileID = fopen(filename,'r');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%             fclose(fileID);
%             Dirty_Chain = [dataArray{1:end-1}];
%             clearvars filename startRow formatSpec fileID dataArray ans;
%             theta_start(:,i)=Dirty_Chain(1,1:nb_dim);
% 
%         end
% 
% 
%     end
% 
%     if (plot_Clean_Traces == 1)
% 
%         theta = [];
%         like  = [];
%         post  = [];
% 
%         like_max_value_temp = 0;
%         post_max_value_temp = 0;
% 
%         for i=proc
% 
%             filename = strcat('./clean_chain_',num2str(i),'.dat');
%             startRow = 2;
%             str_format='%20f';
%             for ii=2:nb_dim+1
%                 str_format = strcat(str_format,'%20f');
%             end
%             str_format = strcat(str_format,'%f%[^\n\r]');
%             formatSpec = str_format;
%             fileID = fopen(filename,'r');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%             fclose(fileID);
%             Clean_Chain = [dataArray{1:end-1}];
%             clearvars filename startRow formatSpec fileID dataArray ans;
%             theta_temp(:,:)=Clean_Chain(:,1:nb_dim);
%             like_temp(:,1)=Clean_Chain(:,nb_dim+1);
%             post_temp(:,1)=Clean_Chain(:,nb_dim+2);
% 
%             [like_max_value_temp_temp, like_max_pos_temp_temp]=max(like_temp);
%             [post_max_value_temp_temp, post_max_pos_temp_temp]=max(post_temp);
%             if like_max_value_temp_temp > like_max_value_temp
%                 i_proc_like_max = i;
%                 like_max_value_temp = like_max_value_temp_temp;
%             end
%             if post_max_value_temp_temp > post_max_value_temp
%                 i_proc_post_max = i;
%                 post_max_value_temp = post_max_value_temp_temp;
%             end
% 
%             theta = [theta; theta_temp];
%             like  = [like; like_temp];
%             post  = [post; post_temp];
% 
%             clearvars theta_temp like_temp post_temp
% 
%         end
% 
%         [like_max_value, like_max_pos]=max(like);
%         [post_max_value, post_max_pos]=max(post);
% 
%         i_def=size(theta,1)*nb_alpha*max(nb_fwd,1);
% 
%     end
% 
%     if (plot_Posterior == 1)
% 
%         filename = './x_y_data.dat';
%         delimiter = ' ';
%         startRow = 2;
%         formatSpec = '%f%f%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         fclose(fileID);
%         x_data = dataArray{:, 1};
%         y_data = dataArray{:, 2};
%         clearvars filename delimiter startRow formatSpec fileID dataArray ans;
%         
%         filename = './x_y_data_orig.dat';
%         delimiter = ' ';
%         startRow = 2;
%         formatSpec = '%f%f%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         fclose(fileID);
%         x_data_orig = dataArray{:, 1};
%         y_data_orig = dataArray{:, 2};
%         clearvars filename delimiter startRow formatSpec fileID dataArray ans;
%         
%         
%         filename = './data_sets.dat';
%         delimiter = ' ';
%         startRow = 2;
%         formatSpec = '%f%*s%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         fclose(fileID);
%         data_sets = dataArray{:, 1};
%         clearvars filename delimiter startRow formatSpec fileID dataArray ans;
%         
%     end
%     
%     filename = './x_Nodes.dat';
%     delimiter = ' ';
%     startRow = 2;
%     formatSpec = '%f%*s%[^\n\r]';
%     fileID = fopen(filename,'r');
%     dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false, 'EndOfLine', '\r\n');
%     fclose(fileID);
%     x_nodes = dataArray{:, 1};
%     clearvars filename delimiter startRow formatSpec fileID dataArray ans;
%     
%     filename = './like_node_sets.dat';
%     delimiter = ' ';
%     startRow = 2;
%     formatSpec = '%f%*s%[^\n\r]';
%     fileID = fopen(filename,'r');
%     dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%     fclose(fileID);
%     like_node_sets = dataArray{:, 1};
%     clearvars filename delimiter startRow formatSpec fileID dataArray ans;
% 
%     if (compute_ThetaTheta_Corr == 1)
% 
%         A=[];
%         for j=1:nb_dim
%             if theta_scale(j,:) == 'lin'
%                 A=[A, theta(:,j)];
%             elseif theta_scale(j,:) == 'log'
%                 A=[A, log10(theta(:,j))];
%             end
%         end
%         [R,P] = corrcoef(A);
%         corr_Thetas=R;
% 
%     end
% 
%     if (plot_Output_Posterior == 1) || (plot_Output_Posterior_Pcolor == 1) 
% 
%         for i=proc
% 
%             filename = strcat('./x_y_max_like_',num2str(i),'.dat');
%             delimiter = ' ';
%             startRow = 2;
%             formatSpec = '%f%f%[^\n\r]';
%             fileID = fopen(filename,'r');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%             fclose(fileID);
%             x_Max_Like(:,i) = dataArray{:, 1};
%             y_Max_Like(:,i) = dataArray{:, 2};
%             clearvars filename delimiter startRow formatSpec fileID dataArray ans;
% 
%         end
% 
%         for i=proc
% 
%             filename = strcat('./x_y_max_posterior_',num2str(i),'.dat');
%             delimiter = ' ';
%             startRow = 2;
%             formatSpec = '%f%f%[^\n\r]';
%             fileID = fopen(filename,'r');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%             fclose(fileID);
%             x_Max_Post(:,i) = dataArray{:, 1};
%             y_Max_Post(:,i) = dataArray{:, 2};
%             clearvars filename delimiter startRow formatSpec fileID dataArray ans;
% 
%         end
% 
% %         nb_markers=1;
% %         x_markers=linspace(plot_absc_min,plot_absc_max,nb_markers);
% %         j=2;
% %         for i=1:nb_markers
% %             while x_nodes(j)<x_markers(i)
% %                j=j+1; 
% %             end
% %             nodes_for_markers(i)=j-1;
% %         end
% % 
% %         nb_markers2=1;
% %         x_markers2=linspace(plot_absc_min,plot_absc_max,nb_markers2);
% %         j=2;
% %         for i=1:nb_markers2
% %             while x_nodes(j)<x_markers2(i)
% %                j=j+1; 
% %             end
% %             nodes_for_markers2(i)=j-1;
% %         end
% 
%     end
% 
%     if (plot_Output_Posterior_Pcolor == 1) || (check_Output_Posterior == 1) || (plot_HPD ==1 )
% 
%         if data_scale == 'lin'
%            y_Space_temp = linspace(plot_data_min,plot_data_max,nb_bins_histogram+1);
%            y_Space_min  = (y_Space_temp(1)+y_Space_temp(2))/2;
%            y_Space_max  = (y_Space_temp(end)+y_Space_temp(end-1))/2;
%            y_Space_eff  = linspace(y_Space_min,y_Space_max,nb_bins_histogram);
%            y_Space      = linspace(y_Space_min,y_Space_max,nb_bins_histogram);
%         elseif data_scale == 'log'
%            y_Space_temp = linspace(log10(plot_data_min),log10(plot_data_max),nb_bins_histogram+1);
%            y_Space_min  = ((y_Space_temp(1)+y_Space_temp(2))/2);
%            y_Space_max  = ((y_Space_temp(end)+y_Space_temp(end-1))/2);
%            y_Space_eff  = linspace(y_Space_min,y_Space_max,nb_bins_histogram);
%            y_Space      = 10.^linspace(y_Space_min,y_Space_max,nb_bins_histogram);
%         end
%         
%         filename = './Hist_x_Nodes.dat';
%         delimiter = ' ';
%         startRow = 2;
%         formatSpec = '%f%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         fclose(fileID);
%         hist_x_nodes = dataArray{:, 1};
%         clearvars filename delimiter startRow formatSpec fileID dataArray ans;
%         
%         filename = './Hist_Nodes_Sets.dat';
%         delimiter = ' ';
%         startRow = 2;
%         formatSpec = '%f%[^\n\r]';
%         fileID = fopen(filename,'r');
%         dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
%         fclose(fileID);
%         hist_nodes_sets = dataArray{:, 1};
%         clearvars filename delimiter startRow formatSpec fileID dataArray ans;
%         
%         Hist_Vector=zeros(nb_bins_histogram*(length(hist_x_nodes)),1);
%         for i=proc
% 
%             filename = strcat('./Hist_for_pcolor_',num2str(i),'.dat');
%             delimiter = ' ';
%             formatSpec = '%f%[^\n\r]';
%             fileID = fopen(filename,'r');
%             dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'EmptyValue' ,NaN, 'ReturnOnError', false);
%             fclose(fileID);
%             Hist_Vector = Hist_Vector + dataArray{:, 1};
%             clearvars filename delimiter formatSpec fileID dataArray ans;
% 
%         end
%         
%         for j=1:length(hist_x_nodes)
%             for i=1:nb_bins_histogram
%                 Hist_Matrix(i,j) = Hist_Vector( (j-1)*(nb_bins_histogram)+ i);
%             end 
%         end
% 
%     end
%     
% end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%



%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 0 %%% SMUQ VISUAL DEBUG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% if (plot_Debug == 1)
% 
%       if debug_output == 1
%           
%           funcName = str2func(strcat('CheckProgress_',ExtCode(1,:)));
%           i_figure = funcName(PlottingFormat, i_figure, data_scale_plot, x_names, y_names, plot_y_Data, ...
%                               plot_y_Start, plot_y_Current, plot_y_Prop, ...
%                               x_data, y_data, ...
%                               x_start(:,proc_Debug), y_start(:,proc_Debug), ...
%                               x_current(:,proc_Debug), y_current(:,proc_Debug), ...
%                               x_prop(:,proc_Debug), y_prop(:,proc_Debug));
%       end
%       
%       i_figure_pre = i_figure;
%       for iProc=proc_Debug
%           i_figure = i_figure_pre;
%           
%           for ii=1:length(dim_vector_debug)-1
%               
%               for jj=ii+1:length(dim_vector_debug)
%                   
%                   i = dim_vector_debug(ii);
%                   j = dim_vector_debug(jj);
%           
%                   if debug_chain_space == 1
%                       
%                       i_figure = debug_chain_space_fun(PlottingFormat, i_figure, theta_scale, theta_names, i, j, i_MCMC(1:LengthVec(proc(iProc)+1),iProc), i_DR(1:LengthVec(proc(iProc)+1),iProc), ...
%                                                        ThetaDebug(1:LengthVec(proc(iProc)+1),:,iProc), Like(1:LengthVec(proc(iProc)+1),iProc), Post(1:LengthVec(proc(iProc)+1),iProc), ...
%                                                        DRPar(1:LengthVec(proc(iProc)+1),iProc), Accept(1:LengthVec(proc(iProc)+1),iProc), AccRate(1:LengthVec(proc(iProc)+1),iProc), ...
%                                                        UnifMin, UnifMax);
%                     
%                   end
%                   
%                   if debug_chain_posterior == 1
%                       
%                       i_figure = debug_chain_posterior_fun(PlottingFormat, i_figure, theta_scale, theta_names, i, j, i_MCMC(1:LengthVec(proc(iProc)+1),iProc), i_DR(1:LengthVec(proc(iProc)+1),iProc), ...
%                                                            ThetaDebug(1:LengthVec(proc(iProc)+1),:,iProc), Like(1:LengthVec(proc(iProc)+1),iProc), Post(1:LengthVec(proc(iProc)+1),iProc), ...
%                                                            DRPar(1:LengthVec(proc(iProc)+1),iProc), Accept(1:LengthVec(proc(iProc)+1),iProc), AccRate(1:LengthVec(proc(iProc)+1),iProc), ...
%                                                            UnifMin, UnifMax);
%                                                        
%                   end
%                   
%                   if debug_chain_clean == 1
%                       
%                       i_figure = debug_chain_clean_fun(PlottingFormat, i_figure, theta_scale, theta_names, i, j, i_MCMC(1:LengthVec(proc(iProc)+1),iProc), i_DR(1:LengthVec(proc(iProc)+1),iProc), ...
%                                                        ThetaDebug(1:LengthVec(proc(iProc)+1),:,iProc), Like(1:LengthVec(proc(iProc)+1),iProc), Post(1:LengthVec(proc(iProc)+1),iProc), ...
%                                                        DRPar(1:LengthVec(proc(iProc)+1),iProc), Accept(1:LengthVec(proc(iProc)+1),iProc), AccRate(1:LengthVec(proc(iProc)+1),iProc), ...
%                                                        UnifMin, UnifMax);
%                                                        
%                   end
%                   
%               end
%               
%           end
%                     
%       end      
% 
% end
%%



%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 1 %%% PRE-PROCESSING: Plotting Priors and Parameters Chain Starting Values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if plot_Priors==1

    i_figure = plot_Priors_fun(PlottingFormat, i_figure, save_fig, proc, plot_theta_nb_bins_hist, plot_Props);
    
end
%%



%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 2 %%% PROCESSING: Analyzing the Parameters Chains
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if analyze_Theta_Chains == 1

    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 2.1 %%% Plot the Traces for Parameters Chains 
    if plot_Traces == 1
        
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% 2.1.1 %%% Plot Dirty Traces for Parameters Chains 
        if plot_Dirty_Traces == 1
           plot_Dirty_Traces

            i_figure = plot_Traces_fun(PlottingFormat, i_figure, save_fig, proc, plot_Theta_Moments, Theta_Moments_Jump, 0);
            
        end

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% 2.1.2 %%% Plot Clean Traces for Parameters Chains 
        if plot_Clean_Traces == 1
           plot_Clean_Traces

            i_figure = plot_Traces_fun(PlottingFormat, i_figure, save_fig, proc, plot_Theta_Moments, Theta_Moments_Jump, 1);

        end
        
    end
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 2.2 %%% Plot Chain in the Parameters Spaces
    if plot_Theta_Spaces == 1
       
       if (length(dim_vector)==1)
          dim_vector=[1:nb_dim];
       end
        
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% 2.2.1 %%% Plot Dirty Chain in the Parameters Spaces
        if plot_Theta_Spaces_Dirty == 1
           plot_Theta_Spaces_Dirty
            
           for i=dim_vector(1:end-1)

               for j=dim_vector

                    if i~=j
                        
                        proc=proc;

                        i_figure = plot_Theta_Spaces_fun(PlottingFormat, i_figure, proc, i, j, nb_DR, 0, ...
                                          plot_Theta_Spaces_Points, plot_Theta_Spaces_Hist, plot_Theta_Spaces_Pcolor, ...
                                          plot_Theta_Spaces_Posterior_Surf, plot_Theta_Spaces_Posterior_Pcolor, ...
                                          plot_Theta_Spaces_Posterior_Normalized, plot_Theta_Spaces_Evo, Evo_start, Evo_jump, Evo_cut, ...
                                          Theta_Spaces_pcolor_nbins);
                                  
                    end
                                  
                end

            end
        
        end
        
        
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% 2.2.2 %%% Plot Clean Chain in the Parameters Spaces
        if plot_Theta_Spaces_Clean == 1
           plot_Theta_Spaces_Clean
            
            for i=dim_vector(1:end-1)

                for j=dim_vector

                    if i~=j

                       proc=proc; 
                        
                       i_figure = plot_Theta_Spaces_fun(PlottingFormat, i_figure, proc, i, j, nb_DR, 1, ...
                                          plot_Theta_Spaces_Points, plot_Theta_Spaces_Hist, plot_Theta_Spaces_Pcolor, ...
                                          plot_Theta_Spaces_Posterior_Surf, plot_Theta_Spaces_Posterior_Pcolor, ...
                                          plot_Theta_Spaces_Posterior_Normalized, plot_Theta_Spaces_Evo, Evo_start, Evo_jump, Evo_cut, ...
                                          Theta_Spaces_pcolor_nbins);

                    end

                end

            end
            
        end

    end
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 2.3 %%% Parameters Correlations Bar
    if compute_ThetaTheta_Corr == 1
       compute_ThetaTheta_Corr

        i_figure = compute_Thetas_Corr_fun(PlottingFormat, i_figure, save_fig, proc);

    end
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 2.4 %%% Parameters Auto-Correlations
    if plot_Autocorrelations == 1
       plot_Autocorrelations
        
        if plot_Theta_Dirty_Autocorrelations == 1
           plot_Theta_Dirty_Autocorrelations
           
            [i_figure, T_auto] = plot_Autocorrelations_fun(PlottingFormat, i_figure, save_fig, proc, max_lag_dirty, 0);
            T_auto_dirty=T_auto
            
        end
        
        if plot_Theta_Clean_Autocorrelations == 1
           plot_Theta_Clean_Autocorrelations

            [i_figure, T_auto] = plot_Autocorrelations_fun(PlottingFormat, i_figure, save_fig, proc, max_lag_clean, 1);
            T_auto
            
        end
        
    end
 

end 
%%


%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% POST-PROCESSING 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if plot_Posterior == 1
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.1.1 %%% Plotting Posterior for Parameters
    if plot_Theta_Posterior == 1
       plot_Theta_Posterior

        i_figure = plot_Theta_Posterior_fun(PlottingFormat, i_figure, save_fig, proc, plot_theta_hist, plot_theta_pdf,... 
                                                 plot_Priors, plot_theta_nb_bins_hist, plot_theta_max_like, plot_theta_max_post);

    end
    
    if plot_Theta_Posterior_Bspline == 1
       plot_Theta_Posterior_Bspline
       
       i_figure = plot_Theta_Posterior_Bspline_fun(PlottingFormat, i_figure, save_fig, proc, theta_post_cpts, theta_cpts_range, ... 
                                                     theta_spline_nb_bins, theta_spline_nb_points);
       
       
    end

    if plot_Theta_Posterior_Gauss1 == 1
       plot_Theta_Posterior_Gauss1
       
       i_figure = plot_Theta_Posterior_Gauss1_fun(PlottingFormat,i_figure, save_fig, proc, theta_post_gauss1, theta_gauss1_range, ...
                                                     theta_gauss1_nb_bins, theta_gauss1_nb_points);
       
       
    end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.1.2 %%% Parameters Posterior Normalized with respect to Parameters Data Values
    if plot_Theta_Posterior_Normalized == 1
       plot_Theta_Posterior_Normalized

        i_figure = plot_Theta_Posterior_Normalized_fun(PlottingFormat, i_figure, proc, save_fig);

    end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.2.1 %%% Plot QoI Posterior Mean and Confidence Interval    
    if plot_Output_Posterior == 1
       plot_Output_Posterior

        i_figure = plot_Output_Posterior_sets_fun(PlottingFormat, i_figure, save_fig, nb_of_sigma, plot_y_interval, ...
                                                  plot_Max_Like, plot_Max_Post, check_Output_Posterior, proc, x_check);

    end 
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.2.2 %%% Plot the y Posteriors
    if plot_Output_Posterior_Pcolor == 1
       plot_Output_Posterior_Pcolor

         i_figure = plot_Output_Posterior_Pcolor_sets_fun(PlottingFormat, i_figure, save_fig, proc, plot_Max_Like, plot_Max_Post, ...
                                                          check_Output_Posterior, x_check);

    end 
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.2.3 %%%  Checking the y punctual Posteriors
    if check_Output_Posterior == 1
       check_Output_Posterior

        i_figure = check_Output_Posterior_sets_fun(PlottingFormat, i_figure, save_fig, proc, nb_pcolor, x_check);                                       

    end       
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 3.2.4 %%% Plot Highest Probability Density (HPD) Intervals for y Posteriors
    if plot_HPD == 1
       plot_HPD

        i_figure = plot_HPD_sets_fun(PlottingFormat, i_figure, save_fig, proc, nb_pcolor, HPD_tol, check_Output_Posterior, x_check);

    end
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 4 %%% EXTERNAL CODE PARTICULARIZED
% if plot_ExtCode == 1
%    plot_ExtCode
% 
%     funcName = str2func(strcat('plot_posterior_fun_',ExtCode));
%     i_figure = funcName(PlottingFormat, i_figure, save_fig, plot_y_interval, proc, i_def, data_scale, x_nodes, ...
%                         y_data, y_Max_Like, y_Max_Post);
% 
% end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
