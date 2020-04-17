close all
clear all
clc


outputdirectory = '/home/rstkwsk2/workspace/runs/results/misp4_3tc/2p/add/bayesian_mult/output';
procs = [1:20];
nb_procs = length(procs);
nb_bins_hist = 50;

parameterchain = [];
maxposterior = 0;
maxposteriorparam = [];
startpoints = [];
ystart = zeros(nb_procs,1);

if (nb_procs > 1)
    filename = strcat(outputdirectory,'/',num2str(procs(1)),'/parameter_names.dat');
else
    filename = strcat(outputdirectory,'/parameter_names.dat');
end
delimiter = ' ';
formatSpec = '%s%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter,  'ReturnOnError', false);
fclose(fileID);
names = [dataArray{1:end-1}];
clearvars filename delimiter formatSpec fileID dataArray ans;

nb_params = 2;

% 
% min = [0.1835544 0.2055264  0.135524E-010 0.3607648E-010 4 5 0.003 0.003 0.003]';
% max = [0.2753316 0.3082896  0.203286E-010 0.5411472E-010 7 8 0.06 0.06 0.06]';
% type = { 'uniform'; 'uniform'; 'uniform'; 'uniform'; 'uniform';...
%          'uniform'; 'uniform'; 'uniform'; 'uniform'};

% min = [0.1835544 0.2055264  0.003 0.003 0.003]';
% max = [0.2753316 0.3082896  0.06 0.06 0.06]';
% mu = [0.2294 0.2569 0 0 0];
% sigma = [0.0153 0.0171 0 0 0];
% type = { 'uniform'; 'uniform'; 'uniform'; 'uniform'; 'uniform'};

% min = [0.1835544 0.2055264  3 3 3]';
% max = [0.2753316 0.3082896  100 100 100]';
% mu = [0.2294 0.2569 0 0 0];
% sigma = [0.0153 0.0171 0 0 0];
% type = { 'normal'; 'normal'; 'uniform'; 'uniform'; 'uniform'};

% min = [0.1835544 0.2055264  3 3 3 0 0 0 0.01 0.01 0.01]';
% max = [0.2753316 0.3082896  50 50 50 100 100 100 1 1 1]';
% type = { 'normal'; 'normal'; 'uniform'; 'uniform'; 'uniform';...
%          'uniform'; 'uniform'; 'uniform'; 'uniform'; 'uniform';'uniform'};
% mu = [0.2294 0.2569 0 0 0];
% sigma = [0.0153 0.0171 0 0 0];

% min = [0.1835544 0.2055264  1 1 1 0 0 0 0.01 0.01 0.01]';
% max = [0.2753316 0.3082896  2500 2500 2500 100 100 100 1 1 1]';
% type = { 'uniform'; 'uniform'; 'uniform'; 'uniform'; 'uniform';...
%          'uniform'; 'uniform'; 'uniform'; 'uniform'; 'uniform';'uniform'};

% min = [0.1835544 0.2055264  .003 0.003 0.003 30 30 30 0.01 0.01 0.01]';
% max = [0.2753316 0.3082896  0.06 0.06 0.06 100 100 100 1 1 1]';
% type = { 'uniform'; 'uniform'; 'uniform'; 'uniform'; 'uniform';...
%          'uniform'; 'uniform'; 'uniform'; 'uniform'; 'uniform';'uniform'};
     
min = [0.1835544 0.2055264  0 0 0 1 1 1 0 0 0]';
max = [0.2753316 0.3082896  50 50 50 100 100 100 2 2 2]';
type = { 'uniform'; 'uniform'; 'uniform'; 'uniform'; 'uniform';...
         'uniform'; 'uniform'; 'uniform';'uniform'; 'uniform'; 'uniform'};
% 
% min = [0.1835544 0.2055264  0 0 0 -10 -10 -10 0 0 0]';
% max = [0.2753316 0.3082896  200 200 200 -3 -3 -3 2 2 2]';
% type = { 'normal'; 'normal'; 'uniform'; 'uniform'; 'uniform';...
%          'uniform'; 'uniform'; 'uniform';'uniform'; 'uniform'; 'uniform'};
% mu = [0.2294 0.2569];
% sigma = [0.0153 0.0171];

% min = [0.1835544 0.2055264  0 0 0 0 0 0 0 0 0 0 0 0 -9 -9 -9]';
% max = [0.2753316 0.3082896  100 100 100 100 100 100 100 200 200 150 150 150 -3 -3 -3]';
% type = { 'normal'; 'normal'; 'uniform'; 'uniform'; 'uniform';...
%     'uniform'; 'uniform'; 'uniform';'uniform'; 'uniform';...
%     'uniform'; 'uniform'; 'uniform';'uniform'; 'uniform';...
%     'uniform'; 'uniform';};
% mu = [0.2294 0.2569];
% sigma = [0.0153 0.0171];

% type = { 'uniform'; 'uniform'; 'uniform'; 'uniform'; 'uniform'; 'uniform'; ...
%          'uniform'; 'log10uniform';'log10uniform'};

% type = { 'uniform'; 'uniform'; 'uniform'; 'uniform'; 'uniform'; 'uniform'; ...
%   'uniform'; 'uniform'; 'uniform'; 'uniform'; 'uniform'};
    
lineartypes = {'uniform'; 'normal'; 'logistic'; 'gamma'};
logtypes = {'loguniform'; 'lognormal'};
log10types = {'log10uniform'; 'log10normal'};
      
for i = 1:nb_procs
    if (nb_procs == 1)
        filename = strcat(outputdirectory,'/parameter_chain.dat');
    else
        filename = strcat(outputdirectory,'/',num2str(procs(i)),'/parameter_chain.dat');
    end
    delimiter = ' ';
    formatSpec = '%f';
    for ii = 2:nb_params
        formatSpec = strcat(formatSpec,'%f');
    end
    formatSpec =  strcat(formatSpec,'%[^\n\r]');
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'EmptyValue' ,NaN, 'ReturnOnError', false);
    fclose(fileID);
    parameterchaintemp = [dataArray{1:end-1}];
    clearvars filename formatSpec fileID dataArray ans;
    parameterchain = [parameterchain;parameterchaintemp(:,1:2)];
    
    if (nb_procs == 1)
        filename = strcat(outputdirectory,'/posterior_chain.dat');
    else
        filename = strcat(outputdirectory,'/',num2str(procs(i)),'/posterior_chain.dat');
    end
    delimiter = ' ';
    formatSpec = '%f';
    formatSpec =  strcat(formatSpec,'%[^\n\r]');
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'EmptyValue' ,NaN, 'ReturnOnError', false);
    fclose(fileID);
    posteriorchaintemp = [dataArray{1:end-1}];
    clearvars filename formatSpec fileID dataArray ans;
    maxtemp = 0;
    maxtempi = 0;
    ii = 1;
    for ii = 1: length(posteriorchaintemp)
        if posteriorchaintemp(i) > maxtemp
            maxtemp = posteriorchaintemp(i);
            maxtempi = ii;
        end
    end

    if (maxtemp > maxposterior)
        maxposterior = maxtemp;
        maxposteriorparam = parameterchaintemp(maxtempi,:);
    end
    
    clearvars parameterchaintemp
    clearvars posteriorchaintemp
    
    if (nb_procs == 1)
        filename = strcat(outputdirectory,'/posterior_sampler/initial_start.dat');
    else
        filename = strcat(outputdirectory,'/',num2str(procs(i)),'/posterior_sampler/initial_start.dat');
    end
    delimiter = '';
    formatSpec = '%f%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter,  'ReturnOnError', false);
    fclose(fileID);
    start_point = dataArray{:, 1};
    clearvars filename delimiter formatSpec fileID dataArray ans;
    startpoints = [startpoints;start_point'];
    clearvars start_point

end
% for i = 1:nb_params
%     figure(i)
%     hpdf=histfit(parameterchain(:,i),nb_bins_hist,'kernel');
% end
% stop
maxposteriorparam'

figure(1+nb_params)
plotmatrix(parameterchain)

for i = 1:nb_params
    figure(i)
    hold on
    if (ismember(type(i),lineartypes))
        hpdf=histfit(parameterchain(:,i),nb_bins_hist,'kernel');
        hold on
        if (strcmp('uniform',char(type(i))))
            tmin = min(i);
            tmax = max(i);
            for ii = 1:nb_procs
               ystart(ii) = unifpdf(startpoints(ii,i),tmin,tmax);
               plot(startpoints(ii,i),ystart(ii),'ko','MarkerSize',10,'LineWidth',3,'MarkerFaceColor','k');
            end
        elseif (strcmp('normal',char(type(i))))
            tmin = min(i);
            tmax = max(i);
            for ii = 1:nb_procs
               ystart(ii) = normpdf(startpoints(ii,i),mu(i),sigma(i));
               plot(startpoints(ii,i),ystart(ii),'ko','MarkerSize',10,'LineWidth',3,'MarkerFaceColor','k');
            end
        end
    elseif (ismember(type(i),logtypes))
        hpdf=histfit(log(parameterchain(:,i)),nb_bins_hist,'kernel');
        hold on
        if (strcmp('loguniform',char(type(i))))
            tmin = log(min(i));
            tmax = log(max(i));
            for ii = 1:nb_procs
                ystart(ii) = unifpdf(log(startpoints(ii,i)),tmin,tmax);
                plot(log(startpoints(ii,i)),ystart(ii),'ko','MarkerSize',10,'LineWidth',3,'MarkerFaceColor','k');
            end
        end        
    elseif (ismember(type(i),log10types))
        hpdf=histfit(log10(parameterchain(:,i)),nb_bins_hist,'kernel');
        hold on
        if (strcmp('log10uniform',char(type(i))))
            tmin = log10(min(i));
            tmax = log10(max(i));
            for ii = 1:nb_procs
                ystart(ii) = unifpdf(log10(startpoints(ii,i)),tmin,tmax);
                plot(log10(startpoints(ii,i)),ystart(ii),'ko','MarkerSize',10,'LineWidth',3,'MarkerFaceColor','k');
            end
        end
    else
        break
    end

    x = linspace(tmin,tmax,1000);
    
    if (strcmp(char(type(i)),'uniform') || strcmp(char(type(i)),'log10uniform') || strcmp(char(type(i)),'loguniform'))
        pdf=unifpdf(x,tmin,tmax);
    elseif (strcmp(char(type(i)),'normal') || strcmp(char(type(i)),'log10normal') || strcmp(char(type(i)),'lognormal'))
        pdf=normpdf(x,mu(i),sigma(i));
    else
        break
    end
    
    hprior=plot(x,pdf,'--k','LineWidth',3);

    hpdf(1).FaceColor='none';
    hpdf(1).LineStyle = 'none';
    hpdf(1).Visible='off';
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
    hpdf(2).YData=hpdf(2).YData *1/trapz(hpdf(2).XData,hpdf(2).YData);
    if i == 1
        kcondvir_mult = [hpdf(2).XData,hpdf(2).YData];
        save('kcondvir_mult.mat','kcondvir_mult')
        clearvars kcondvir_mult
    elseif i == 2
        kcondchar_mult = [hpdf(2).XData,hpdf(2).YData];
        save('kcondchar_mult.mat','kcondchar_mult')
        clearvars kcondchar_mult
    end
%     if i == 1
%         kcondvir_logisitic = [hpdf(2).XData,hpdf(2).YData];
%         save('kcondvir_logisitic.mat','kcondvir_logisitic')
%         clearvars kcondvir_logisitic
%     elseif i == 2
%         kcondchar_logisitic = [hpdf(2).XData,hpdf(2).YData];
%         save('kcondchar_logisitic.mat','kcondchar_logisitic')
%         clearvars kcondchar_logisitic
%     end
%     if i == 1
%         kcondvir_exp1 = [hpdf(2).XData,hpdf(2).YData];
%         save('kcondvir_exp1.mat','kcondvir_exp1')
%         clearvars kcondvir_exp1
%     elseif i == 2
%         kcondchar_exp1 = [hpdf(2).XData,hpdf(2).YData];
%         save('kcondchar_exp1.mat','kcondchar_exp1')
%         clearvars kcondchar_exp1
%     end
%     if i == 1
%         kcondvir_exp2 = [hpdf(2).XData,hpdf(2).YData];
%         save('kcondvir_exp2.mat','kcondvir_exp2')
%         clearvars kcondvir_exp2
%     elseif i == 2
%         kcondchar_exp2 = [hpdf(2).XData,hpdf(2).YData];
%         save('kcondchar_exp2.mat','kcondchar_exp2')
%         clearvars kcondchar_exp2
%     end
    
    hold on
    grid on
    harea=area(hpdf(2).XData,hpdf(2).YData);
    harea.EdgeColor='none';
    harea.FaceColor=[.5,.5,1];
    harea.FaceAlpha=.25;
    
    legend([hprior,hpdf(2)],'Prior','Normalized Posterior');
    xlim([tmin tmax])
    xlabel(char(names(i)));
    ylabel('Marginal PDFs');
    grid off
    pbaspect([1.5 1 1])            
    set(gca,'FontSize',30, 'FontName','PT Sans')
    set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
end
