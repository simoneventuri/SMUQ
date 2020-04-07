function i_figure = plot_Theta_Space_Posterior_Normalized_fun(PlottingFormat, i_figure, i, j, theta_scale, theta_names, A_theta, A_x_Space_lin, A_y_Space_lin)

    C_mean=mean(A_theta);
    C_cov=cov(A_theta);
    [C_vec C_val]=eig(C_cov);

    [X_post_norm,Y_post_norm] = meshgrid(A_x_Space_lin,A_y_Space_lin);
    F_post_norm = mvnpdf([X_post_norm(:),Y_post_norm(:)],C_mean,C_cov);
    F_post_norm = reshape(F_post_norm,length(A_x_Space_lin),length(A_y_Space_lin));

    figure(i_figure)
    hold on
    if theta_scale(i,:) == 'log'
        if theta_scale(j,:) == 'log'
            surf(A_x_Space_lin,A_y_Space_lin,F_post_norm);
            %plot3(10.^(A_theta(:,1)),10.^(A_theta(:,2)),post(:),'.k','MarkerSize',20);
        elseif theta_scale(j,:) == 'lin'
            surf(A_x_Space_lin,A_y_Space_lin,F_post_norm);
            %plot3(10.^(A_theta(:,1)),A_theta(:,2),post(:),'.k','MarkerSize',20);
        end
    elseif theta_scale(i,:) == 'lin'
        if theta_scale(j,:) == 'log'
            surf(A_x_Space_lin,10.^(A_y_Space_lin),F_post_norm);
            %plot3(A_theta(:,1),10.^(A_theta(:,2)),post(:),'.k','MarkerSize',20);
        elseif theta_scale(j,:) == 'lin'
            surf(A_x_Space_lin,A_y_Space_lin,F_post_norm);
            %plot3(A_theta(:,1),A_theta(:,2),post(:),'.k','MarkerSize',20);
        end
    end
    %pcolor(A_x_Space,A_y_Space,F_post_norm);
    str_x = strtrim(theta_names(i,:));
    str_y = strtrim(theta_names(j,:));
    str_title = ['Normalized Posterior from ',theta_names(i,:),' - ',theta_names(j,:),' Sampling Histogram'];
    if PlottingFormat(1,:) == 'Visual'
        title(str_title);
        xlabel(str_x);
        ylabel(str_y);  
    elseif PlottingFormat(1,:) == 'Papers'
        title(str_title,'Interpreter','latex');
        xlabel(str_x,'Interpreter','latex');
        ylabel(str_y,'Interpreter','latex');  
    end
    if theta_scale(i,:) == 'log'
        if theta_scale(j,:) == 'log'
            set(gca,'Xlim',[10^A_x_Space_lin(1), 10^A_x_Space_lin(end)],'ylim',[10^A_y_Space_lin(1), 10^A_y_Space_lin(end)]);                      
        elseif theta_scale(j,:) == 'lin'
            set(gca,'Xlim',[10^A_x_Space_lin(1), 10^A_x_Space_lin(end)],'ylim',[A_y_Space_lin(1), A_y_Space_lin(end)]);                      
        end
    elseif theta_scale(i,:) == 'lin'
        if theta_scale(j,:) == 'log'
            set(gca,'Xlim',[A_x_Space_lin(1), A_x_Space_lin(end)],'ylim',[10^A_y_Space_lin(1), 10^A_y_Space_lin(end)]);                      
        elseif theta_scale(j,:) == 'lin'
            set(gca,[A_x_Space_lin(1), A_x_Space_lin(end)],'ylim',[A_y_Space_lin(1), A_y_Space_lin(end)]);                      
        end
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
