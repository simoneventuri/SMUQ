function i_figure = plot_Theta_Space_Posterior_Pcolor_fun(PlottingFormat, i_figure, i, j, theta_names, A_theta, X_post_surf, Y_post_surf, Z_post_surf)

    C_mean=mean(A_theta);
    C_cov = cov(A_theta);
    [C_vec C_val]=eig(C_cov);
    xx=ones(2,1).*C_mean(1);
    yy=ones(2,1).*C_mean(2);
    vx=[C_vec(1,1),C_vec(1,2)]';
    vy=[C_vec(2,1),C_vec(2,2)]';

    figure(i_figure)
    %mesh(X_post_surf,Y_post_surf,Z_post_surf)
    pcolor(X_post_surf,Y_post_surf,Z_post_surf)
    %plot3(A_theta(:,1),A_theta(:,2),post(:),'.k','MarkerSize',20);
    hold on
    quiver(xx,yy,vx/100,vy/100,'Linewidth',3,'Color','r')
    str_x = strtrim(theta_names(i,:));
    str_y = strtrim(theta_names(j,:));
    str_title = ['Posterior for ',theta_names(i,:),' - ',theta_names(j,:)];
    if PlottingFormat(1,:) == 'Visual'
        title(str_title);
        xlabel(str_x);
        ylabel(str_y);  
    elseif PlottingFormat(1,:) == 'Papers'
        title(str_title,'Interpreter','latex');
        xlabel(str_x,'Interpreter','latex');
        ylabel(str_y,'Interpreter','latex');  
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
