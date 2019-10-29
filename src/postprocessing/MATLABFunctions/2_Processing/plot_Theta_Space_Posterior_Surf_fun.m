function i_figure = plot_Theta_Space_Posterior_Surf_fun(PlottingFormat, i_figure, i, j, theta_scale, theta_names, post, A_theta, X_post_surf, Y_post_surf, Z_post_surf)
    
    figure(i_figure)
    %mesh(X_post_surf,Y_post_surf,Z_post_surf)
    surf(X_post_surf,Y_post_surf,Z_post_surf)
    axis tight; hold on
    if theta_scale(i,:) == 'log'
        if theta_scale(j,:) == 'log'
            plot3(10.^(A_theta(:,1)),10.^(A_theta(:,2)),post(:),'.k','MarkerSize',20);
        elseif theta_scale(j,:) == 'lin'
            plot3(10.^(A_theta(:,1)),A_theta(:,2),post(:),'.k','MarkerSize',20);
        end
    elseif theta_scale(i,:) == 'lin'
        if theta_scale(j,:) == 'log'
            plot3(A_theta(:,1),10.^(A_theta(:,2)),post(:),'.k','MarkerSize',20);
        elseif theta_scale(j,:) == 'lin'
            plot3(A_theta(:,1),A_theta(:,2),post(:),'.k','MarkerSize',20);
        end
    end
    str_x = strtrim(theta_names(i,:));
    str_y = strtrim(theta_names(j,:));
    str_z = ['Posterior (Not-Normalized)'];
    str_title = ['Posterior for ',theta_names(i,:),' - ',theta_names(j,:)];
    if PlottingFormat(1,:) == 'Visual'
        title(str_title);
        xlabel(str_x);
        ylabel(str_y);    
        zlabel(str_z); 
    elseif PlottingFormat(1,:) == 'Papers'
        title(str_title,'Interpreter','latex');
        xlabel(str_x,'Interpreter','latex');
        ylabel(str_y,'Interpreter','latex');    
        zlabel(str_z,'Interpreter','latex');  
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
