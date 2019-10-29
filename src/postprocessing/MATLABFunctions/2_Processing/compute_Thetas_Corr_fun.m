function i_figure = compute_Thetas_Corr_fun(PlottingFormat, i_figure, save_fig, proc )

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
    
    theta_clean = [];
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

        theta_clean = [theta_clean; theta_temp];

        clearvars theta_temp

    end
    
    A=[];
    for j=1:nb_dim
        if theta_scale(j,:) == 'lin'
            A=[A, theta_clean(:,j)];
        elseif theta_scale(j,:) == 'log'
            A=[A, log10(theta_clean(:,j))];
        end
    end
    [R,P] = corrcoef(A);
    corr_Thetas=R;
    clearvars theta_clean A P R

    figure(i_figure) 
    corr_Thetas_mod=abs(corr_Thetas);
    corr_Thetas_mod=corr_Thetas_mod-eye(nb_dim);
    bar(abs(corr_Thetas_mod),'stacked')
    set(gca,'Xtick',1:1:nb_dim,'XTickLabel',theta_labels,'XTickLabelRotation',-90)
    str_title = ['Parameters Correlation'];
    if PlottingFormat(1,:) == 'Visual'
      %title(str_title);
        legend(theta_labels,'location','eastoutside')
    elseif PlottingFormat(1,:) == 'Papers'
        legend(theta_labels,'location','eastoutside','Interpreter','latex')
    end
    if PlottingFormat(1,:) == 'Visual'
        set(gca,'FontSize',30, 'FontName','PT Sans');
    elseif PlottingFormat(1,:) == 'Papers'
        set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','TickLabelInterpreter','latex');
        set(gcf, 'PaperPositionMode', 'auto');
    end 
    if save_fig == 1
        str_save_fig = ['./fig/Correlations/theta_QoI_Correlation.fig'];
        savefig(i_figure, str_save_fig);
    end
    i_figure=i_figure+1;
    
    figure(i_figure) 

    imagesc(corr_Thetas);
    set(gca, 'XTick', 1:nb_dim);
    set(gca, 'YTick', 1:nb_dim);
    set(gca, 'XTickLabel', theta_labels);
    set(gca, 'YTickLabel', theta_labels);
    colorbar;
    title('Parameters Correlation')
    if PlottingFormat(1,:) == 'Visual'
        set(gca,'FontSize',30, 'FontName','PT Sans');
    elseif PlottingFormat(1,:) == 'Papers'
        set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','TickLabelInterpreter','latex');
        set(gcf, 'PaperPositionMode', 'auto');
    end 
    if save_fig == 1
        str_save_fig = ['./fig/Correlations/theta_QoI_Correlation_Checkerboard.fig'];
        savefig(i_figure, str_save_fig);
    end
    i_figure=i_figure+1;
    
end
