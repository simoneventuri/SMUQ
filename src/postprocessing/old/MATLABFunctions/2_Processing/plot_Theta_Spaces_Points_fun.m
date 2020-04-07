function i_figure = plot_Theta_Spaces_Points_fun(PlottingFormat, i_figure, i, j, theta_scale, theta_names, theta, like_max_pos, post_max_pos, corr_parpar)      

    figure(i_figure)
    if theta_scale(i,:) == 'log'
        if theta_scale(j,:) == 'log'
            loglog(theta(:,i),theta(:,j),'-o','Color',[204/255,204/255,204/255],'LineWidth',0.5,'MarkerSize',5,'MarkerFaceColor','k')
        elseif theta_scale(j,:) == 'lin'
            semilogx(theta(:,i),theta(:,j),'-o','Color',[204/255,204/255,204/255],'LineWidth',0.5,'MarkerSize',5,'MarkerFaceColor','k')
        end
    elseif theta_scale(i,:) == 'lin'
        if theta_scale(j,:) == 'log'
            semilogy(theta(:,i),theta(:,j),'-o','Color',[204/255,204/255,204/255],'LineWidth',0.5,'MarkerSize',5,'MarkerFaceColor','k')
        elseif theta_scale(j,:) == 'lin'
            plot(theta(:,i),theta(:,j),'-o','Color',[204/255,204/255,204/255],'LineWidth',0.5,'MarkerSize',5,'MarkerFaceColor','k')
        end
    end
    hold on
    if theta_scale(i,:) == 'log'
        if theta_scale(j,:) == 'log'
            h1=loglog(theta(like_max_pos,i),theta(like_max_pos,j),'oc','MarkerSize',15,'MarkerFaceColor','c');
        elseif theta_scale(j,:) == 'lin'
            h1=semilogx(theta(like_max_pos,i),theta(like_max_pos,j),'oc','MarkerSize',15,'MarkerFaceColor','c');
        end
    elseif theta_scale(i,:) == 'lin'
        if theta_scale(j,:) == 'log'
            h1=semilogy(theta(like_max_pos,i),theta(like_max_pos,j),'oc','MarkerSize',15,'MarkerFaceColor','c');
        elseif theta_scale(j,:) == 'lin'
            h1=plot(theta(like_max_pos,i),theta(like_max_pos,j),'oc','MarkerSize',15,'MarkerFaceColor','c');
        end
    end
    if theta_scale(i,:) == 'log'
        if theta_scale(j,:) == 'log'
            h2=loglog(theta(post_max_pos,i),theta(post_max_pos,j),'ob','MarkerSize',15,'MarkerFaceColor','b');
        elseif theta_scale(j,:) == 'lin'
            h2=semilogx(theta(post_max_pos,i),theta(post_max_pos,j),'ob','MarkerSize',15,'MarkerFaceColor','b');
        end
    elseif theta_scale(i,:) == 'lin'
        if theta_scale(j,:) == 'log'
            h2=semilogy(theta(post_max_pos,i),theta(post_max_pos,j),'ob','MarkerSize',15,'MarkerFaceColor','b');
        elseif theta_scale(j,:) == 'lin'
            h2=plot(theta(post_max_pos,i),theta(post_max_pos,j),'ob','MarkerSize',15,'MarkerFaceColor','b');
        end
    end
    str_x = strtrim(theta_names(i,:));
    str_y = strtrim(theta_names(j,:));
    str_title = [theta_names(i,:),' - ',theta_names(j,:),' Chain. \rho = ',num2str(corr_parpar(i,j))];
    if PlottingFormat(1,:) == 'Visual'
        xlabel(str_x);
        ylabel(str_y);    
        title(str_title);
        legend([h1,h2],'Max Likelihood','Max Posterior');
    elseif PlottingFormat(1,:) == 'Visual'
        xlabel(str_x,'Interpreter','latex');
        ylabel(str_y,'Interpreter','latex');    
        title(str_title,'Interpreter','latex');
        legend([h1,h2],'Max Likelihood','Max Posterior','Location','best','Interpreter','latex');
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
