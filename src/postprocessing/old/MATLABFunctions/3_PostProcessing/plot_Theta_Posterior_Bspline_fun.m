function i_figure = plot_Theta_Posterior_Bspline_fun(PlottingFormat, i_figure, save_fig, proc, theta_post_cpts, theta_cpts_range, ... 
                                                     theta_spline_nb_bins, theta_spline_nb_points)

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
                                                 
for i = 1:size(theta_post_cpts,1)
    
    x_cpts = linspace(theta_cpts_range(i,1),theta_cpts_range(i,2),nnz(theta_post_cpts(i,:)));
    xq = linspace(theta_cpts_range(i,1),theta_cpts_range(i,2),2*theta_spline_nb_points);
    theta_x = [];
    theta_y = [];

    for k = 1:size(theta,1)
    
        for j = 1:length(x_cpts)
            y_cpts(j) = theta(k,theta_post_cpts(i,j));
        end
        
        [spl_x,spl_y] = mybspline([x_cpts',y_cpts'],3,theta_spline_nb_points);
        
        vq = interp1(spl_x,spl_y,xq);        
        
        theta_x = [theta_x;xq'];
        theta_y = [theta_y;vq'];
        
        clearvars spl_x spl_y vq 
        
    end
    
    for j = 1:length(x_cpts)
        y_cpts(j) = UnifMin(theta_post_cpts(i,j));
    end
    
    [spl_x,spl_y] = mybspline([x_cpts',y_cpts'],3,theta_spline_nb_points);
    vq = interp1(spl_x,spl_y,xq);
    priorxl = xq;
    prioryl = vq;
    
    clearvars vq spl_x spl_y
    
    for j = 1:length(x_cpts)
        y_cpts(j) = UnifMax(theta_post_cpts(i,j));
    end
    
    [spl_x,spl_y] = mybspline([x_cpts',y_cpts'],3,theta_spline_nb_points);
    vq = interp1(spl_x,spl_y,xq);
    priorxu = xq;
    prioryu = vq;
    
    clearvars vq spl_x spl_y

    [n,C] = hist3([theta_x,theta_y],theta_spline_nb_bins);
    centers = C{2};
    d = diff(centers)/2;
    edgesy = [centers(1)-d(1),centers(1:end-1)+d,centers(end)+d(end)];
    clearvars d centers
    centers = C{1};
    d = diff(centers)/2;
    edgesx = [centers(1)-d(1),centers(1:end-1)+d,centers(end)+d(end)];
    clearvars d centers
    
    if max(edgesy) < max(prioryu)
        flag1 = 1;
    end
    
    if min(edgesy) > min(prioryl)
        flag2 = 1;
    end    
    
    clearvars x_cpts y_cpts theta_x theta_y
    n1 = n';
    n1(:,size(n,1) + 1) = 0;
    n1(size(n,2) + 1,:) = 0;
    
    if flag1 == 1
        n1(size(n,2) + 2,:) = 0;
        edgesy = [edgesy,max(prioryu)];
    end
    
    if flag2 == 1
        n1 = [zeros(1,size(n1,2));n1];
        edgesy = [min(prioryl),edgesy];
    end

    clearvars n
    
    
    figure(i_figure)
    h = pcolor(edgesx,edgesy,n1);
    set(h, 'EdgeColor', 'none');
    shading interp

    hold on
    hpriorl = plot(priorxu,prioryu,'--k','LineWidth',4);
    hprioru = plot(priorxl,prioryl,'--k','LineWidth',4);
    
    clearvars n n1 xb yb xq x_cpts y_cpts hpriorxu hprioryl
    
    if PlottingFormat(1,:) == 'Visual'
        th = title(strcat('B Spline Theta #', space(1), num2str(i)));
        legend([hprioru],'Prior')
    elseif PlottingFormat(1,:) == 'Papers' 
        th = title(strcat('B Spline Theta $\#$', space(1), num2str(i)),'Interpreter','latex');
        legend([hprioru],'Prior','Interpreter','latex','Location','best')
    end
    
    if PlottingFormat(1,:) == 'Visual'
        set(gca,'FontSize',30, 'FontName','PT Sans')
        set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
    elseif PlottingFormat(1,:) == 'Papers'
        set(gca,'FontSize',30, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
        set(gcf, 'PaperPositionMode', 'auto');
    end 
    pbaspect([1.5 1 1])
    
    if save_fig == 1
        str_save_fig = [strcat('./fig/Posteriors/Parameters/Bspline_param_',num2str(i), '_Posterior.fig')];
        savefig(i_figure, str_save_fig);
    end
    
    i_figure = i_figure + 1;
    
        
end

end
