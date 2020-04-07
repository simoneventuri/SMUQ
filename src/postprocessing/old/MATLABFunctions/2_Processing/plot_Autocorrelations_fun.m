function [i_figure, T_auto] = plot_Autocorrelations_fun(PlottingFormat, i_figure, save_fig, proc, max_lag, clean_flag )

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
    
%     if clean_flag==1
%     
%         theta_mean=mean(theta);
%         theta_std=std(theta);
%         theta_n=size(theta,1);
%         
%         for i=1:nb_dim
% 
%             for k=1:theta_n-1
%                 sum_R=0.d0;
%                 for t=1:theta_n-k
%                     sum_R=sum_R+(theta(t,i)-theta_mean(i))*(theta(t+k,i)-theta_mean(i));
%                 end
%                 R(k,i)=(1.d0/((theta_n-k)*theta_std(i)^2))*sum_R;
%             end
% 
%             ii=1;
%             sum_T=0.d0;
%             while R(ii,i)>0
%                 sum_T=sum_T+R(ii,i);
%                 ii=ii+1;
%             end
%             ii=ii-1;
%             T_auto(i)=1+2*sum_T;
% 
%             figure(i_figure) 
%             plot([1:max_lag-1]',R(1:max_lag-1,i),'-ko','MarkerSize',5,'MarkerFaceColor','k')
%             str_theta_i = strtrim(theta_names(i,:));
%             str_title = ['Clean Chain Autocorrelation for ', str_theta_i];
%             if PlottingFormat(1,:) == 'Visual'
%                 xlabel('Lag');
%                 ylabel('Autocorrelation');  
%                 title(str_title);
%             elseif PlottingFormat(1,:) == 'Papers'
%                 xlabel('Lag','Interpreter','latex');
%                 ylabel('Autocorrelation','Interpreter','latex');  
%                 title(str_title);
%             end
%             if PlottingFormat(1,:) == 'Visual'
%                 set(gca,'FontSize',30, 'FontName','PT Sans')
%                 set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
%             elseif PlottingFormat(1,:) == 'Papers'
%                 set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
%                 set(gcf, 'PaperPositionMode', 'auto');
%             end 
%             if save_fig == 1
%                 str_save_fig = ['./fig/Correlations/Autocorrelation.fig'];
%                 savefig(i_figure, str_save_fig);
%             end
%             i_figure=i_figure+1;
%         
%         end
%         
%     elseif clean_flag==0
        
        for i=1:nb_dim
            
            figure(i_figure)
            
            for ii=proc
                theta_temp = [];
                if ( clean_flag == 0 )

                    filename = strcat('./dirty_chain_',num2str(ii),'.dat');
                    startRow = 2;
                    str_format='%20f';
                    for iii=2:nb_dim+1
                        str_format = strcat(str_format,'%20f');
                    end
                    str_format = strcat(str_format,'%f%[^\n\r]');
                    formatSpec = str_format;
                    fileID = fopen(filename,'r');
                    dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
                    fclose(fileID);
                    theta_temp = [dataArray{1:end-1}];
                    clearvars filename startRow formatSpec fileID dataArray ans;

                elseif ( clean_flag == 1 )
                    
                    filename = strcat('./clean_chain_',num2str(ii),'.dat');
                    startRow = 2;
                    str_format='%20f';
                    for iii=2:nb_dim+1
                        str_format = strcat(str_format,'%20f');
                    end
                    str_format = strcat(str_format,'%f%[^\n\r]');
                    formatSpec = str_format;
                    fileID = fopen(filename,'r');
                    dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
                    fclose(fileID);
                    theta_temp = [dataArray{1:end-1}];
                    clearvars filename startRow formatSpec fileID dataArray ans;
                end
                
                theta_mean=mean(theta_temp);
                theta_std=std(theta_temp);
                theta_n=size(theta_temp,1);
                
                for k=1:theta_n-1
                    sum_R=0.d0;
                    for t=1:theta_n-k
                        sum_R=sum_R+(theta_temp(t,i)-theta_mean(i))*(theta_temp(t+k,i)-theta_mean(i));
                    end
                    R(k,i)=(1.d0/((theta_n-k)*theta_std(i)^2))*sum_R;
                end

                iii=1;
                sum_T=0.d0;
                while R(iii,i)>0
                    sum_T=sum_T+R(iii,i);
                    iii=iii+1;
                end
                iii=iii-1;
                T_auto(ii,i)=1+2*sum_T;

                plot([1:max_lag-1]',R(1:max_lag-1,i),'-ko','MarkerSize',5,'LineWidth',5,'MarkerFaceColor','k'); 
                hold on
            
            end
        
            str_theta_i = strtrim(theta_names(i,:));
            if ( clean_flag == 0 )
                str_title = ['Dirty Chain Autocorrelation for ', str_theta_i];
            elseif( clean_flag == 1 )
                str_title = ['Clean Chain Autocorrelation for ', str_theta_i];
            end
            if PlottingFormat(1,:) == 'Visual'
                xlabel('Lag');
                ylabel('Autocorrelation');  
                title(str_title);
            elseif PlottingFormat(1,:) == 'Papers'
                xlabel('Lag','Interpreter','latex');
                ylabel('Autocorrelation','Interpreter','latex');  
                title(str_title);
            end
            if PlottingFormat(1,:) == 'Visual'
                set(gca,'FontSize',30, 'FontName','PT Sans')
                set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
            elseif PlottingFormat(1,:) == 'Papers'
                set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
                set(gcf, 'PaperPositionMode', 'auto');
            end 
            if save_fig == 1
                str_save_fig = ['./fig/Correlations/Autocorrelation.fig'];
                savefig(i_figure, str_save_fig);
            end
            i_figure=i_figure+1;
            
        end
        
%     end
    
end
