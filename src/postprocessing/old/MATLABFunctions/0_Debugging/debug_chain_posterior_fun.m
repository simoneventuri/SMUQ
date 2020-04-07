function i_figure = debug_chain_posterior_fun(PlottingFormat, i_figure, theta_scale, theta_names, i, j, i_MCMC, i_DR, ...
                                              theta, Like, Post, DRPar, Accept, AccRate, UnifMin, UnifMax);
 
    MarkerVec = ['o','s','d','*'];
    AcceptVec = double((char(Accept(:)) == 'T')) + 2;
    ColorVec  = ['k','r','g'];
    PostVec   = log10(max(Post,1.d-300))+300.d0;
                                
    figure(i_figure)
    if theta_scale(i,:) == 'log'
        if theta_scale(j,:) == 'log'
            stem3(theta(:,i),theta(:,j),PostVec(:),'MarkerFaceColor',[0 .75 .75])
            set(gca, 'XScale', 'log', 'YScale', 'log')
        elseif theta_scale(j,:) == 'lin'
            stem3(theta(:,i),theta(:,j),PostVec(:),'MarkerFaceColor',[0 .75 .75])
            set(gca, 'XScale', 'log')
        end
    elseif theta_scale(i,:) == 'lin'
        if theta_scale(j,:) == 'log'
            stem3(theta(:,i),theta(:,j),PostVec(:),'MarkerFaceColor',[0 .75 .75])
            set(gca, 'YScale', 'log')
        elseif theta_scale(j,:) == 'lin'
            stem3(theta(:,i),theta(:,j),PostVec(:),'MarkerFaceColor',[0 .75 .75])
        end
    end
    xlim([UnifMin(i), UnifMax(i)]);
    ylim([UnifMin(j), UnifMax(j)]);
    str_x = strtrim(theta_names(i,:));
    str_y = strtrim(theta_names(j,:));
    str_title = ['Debugging Chain'];
    if PlottingFormat(1,:) == 'Visual'
        xlabel(str_x);
        ylabel(str_y);    
        title(str_title);
    elseif PlottingFormat(1,:) == 'Visual'
        xlabel(str_x,'Interpreter','latex');
        ylabel(str_y,'Interpreter','latex');    
        title(str_title,'Interpreter','latex');
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