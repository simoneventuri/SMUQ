function i_figure = plot_Theta_Posterior_Gauss1_fun(PlottingFormat,i_figure, save_fig, proc, theta_post_gauss1, theta_gauss1_range, ...
                                                     theta_gauss1_nb_bins, theta_gauss1_nb_points)

filename = './Prior.dat';
startRow = 2;
formatSpec = '%*40s%20f%20f%20f%f%[^\n\r]';
fileID = fopen(filename,'r');
dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
fclose(fileID);
UnifMin = dataArray{:, 3};
UnifMax = dataArray{:, 4};
clearvars filename startRow formatSpec fileID dataArray ans;

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
                                                 
space = {' '};
                                                 
for i = 1:size(theta_post_gauss1,1)
   
    x = linspace(theta_gauss1_range(i,1),theta_gauss1_range(i,2),2*theta_gauss1_nb_points);
    priorx = x;
    theta_x = [];
    theta_y = [];

    for k = 1:size(theta,1)
    
        a = theta(k,theta_post_gauss1(i,1));
        b = theta(k,theta_post_gauss1(i,2));
        c = theta(k,theta_post_gauss1(i,3));
           
        y = a*exp(-((x-b)/c).^2);
        
        theta_x = [theta_x;x'];
        theta_y = [theta_y;y'];
        
    end

    [n,C] = hist3([theta_x,theta_y],[length(x),theta_gauss1_nb_bins(2)]);
    centers = C{2};
    d = diff(centers)/2;
    edgesy = [centers(1)-d(1),centers(1:end-1)+d,centers(end)+d(end)];
    clearvars d centers
    centers = C{1};
    d = diff(centers)/2;
    edgesx = [centers(1)-d(1),centers(1:end-1)+d,centers(end)+d(end)];
    clearvars d centers

    if max(edgesy) < UnifMax(theta_post_gauss1(i,1))
        flag1 = 1;
    end

%     if min(edgesy) > min(prioryl)
%         flag2 = 1;
%     end    

    clearvars theta_x theta_y
    n1 = n';
    n1(:,size(n,1) + 1) = 0;
    n1(size(n,2) + 1,:) = 0;

    if flag1 == 1
        n1(size(n,2) + 2,:) = 0;
        edgesy = [edgesy,UnifMax(theta_post_gauss1(i,1))];
    end
%     
%     if flag2 == 1
%         n1 = [zeros(1,size(n1,2));n1];
%         edgesy = [min(prioryl),edgesy];
%     end

    n1 = [zeros(1,size(n1,2));n1];
    edgesy = [0,edgesy];

    clearvars n

    figure(i_figure)
    h = pcolor(edgesx,edgesy,n1);
    set(h, 'EdgeColor', 'none');
    shading interp

% if i == 1
%    filename = './sVirgin.txt';  
% else
%    filename = './sChar.txt'; 
% end

% xt = x;
% if i == 1
%     a = .1222;
%     b = 522.1;
%     c = 497.4;
% else
%     a = 1.38;
%     b = 2164;
%     c = 1024;
% end
% yt = a*exp(-((x-b)/c).^2);
% hold on
% precal = plot(xt,yt,':m','LineWidth',3);
% legend(precal,'Nominal Value')
% if i == 1
%     xlim([200 800])
% end
% clear vars xt yt x y
    


    if PlottingFormat(1,:) == 'Visual'
        str_title = strcat('Gauss1 Theta # ', space(1), num2str(i));
%         xh = xlabel(str_x);
%         yh = ylabel('Marginal PDF');
        th = title(str_title);
    elseif PlottingFormat(1,:) == 'Papers'
        str_title = strcat('Gauss1 Theta $\#$', space(1), num2str(i));
%         xh = xlabel(str_x,'Interpreter','latex');
%         yh = ylabel('Marginal PDF','Interpreter','latex');
        th = title(str_title,'Interpreter','latex');
    end 
    if PlottingFormat(1,:) == 'Visual'
        set(gca,'FontSize',30, 'FontName','PT Sans')
        set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
    elseif PlottingFormat(1,:) == 'Papers'
        set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
        set(gcf, 'PaperPositionMode', 'auto');
    end 
    grid off
    pbaspect([1.5 1 1])
    
    if save_fig == 1
        str_save_fig = [strcat('./fig/Posteriors/Parameters/Bspline_param_',num2str(i), '_Posterior.fig')];
        savefig(i_figure, str_save_fig);
    end
    
    i_figure = i_figure + 1;
        
end

end
