function i_figure = plot_Theta_Spaces_Evo_fun(PlottingFormat, i_figure, i, j, theta_scale, theta_names, theta, nb_DR, ...
                                              i_proc, clean_flag, A_x_Space_lin, A_y_Space_lin, ...
                                              X_post_norm, Y_post_norm, Evo_start, Evo_jump, Evo_cut, prop_cov)
                                          
    pause_1=0.5;
    pause_2=1;

    nb_dim=size(theta_names,1);
    
    filename = strcat('./DRpar_',num2str(i_proc),'.dat');
    formatSpec = '%20f%20f%f%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '',  'ReturnOnError', false);
    fclose(fileID);
    i_sample_DRpar = dataArray{:, 1};
    i_sample_def_DRpar = dataArray{:, 2};
    DR_par_temp = dataArray{:, 3};
    clearvars filename formatSpec fileID dataArray ans;

    if length(DR_par_temp)>1
        if clean_flag==1
            i_clean=1;
            while i_sample_def_DRpar(i_clean)<1
                i_clean=i_clean+1;
            end
            DR_par(:)=DR_par_temp(i_sample_def_DRpar(i_clean:end));
        elseif clean_flag==0
            DR_par(:)=DR_par_temp(i_sample_DRpar(1:end));
            DR_par(1)=DR_par_temp(1);
        end
    else
        DR_par=ones(size(theta,1)).*0.d0+DR_par_temp(1);
    end
    
    figure(i_figure)
    for i_chain=Evo_start:Evo_jump:size(theta,1)-1
        i_chain

        for i_DR=1:nb_DR
            
            if theta_scale(i,:) == 'log'
                if theta_scale(j,:) == 'log'
                    plot(log10(theta(Evo_cut:i_chain,i)),log10(theta(Evo_cut:i_chain,j)),'-o','Color',[204/255,204/255,204/255],'LineWidth',0.5,'MarkerSize',5,'MarkerFaceColor','k')
                elseif theta_scale(j,:) == 'lin'
                    plot(log10(theta(Evo_cut:i_chain,i)),theta(Evo_cut:i_chain,j),'-o','Color',[204/255,204/255,204/255],'LineWidth',0.5,'MarkerSize',5,'MarkerFaceColor','k')
                end
            elseif theta_scale(i,:) == 'lin'
                if theta_scale(j,:) == 'log'
                    plot(theta(Evo_cut:i_chain,i),log10(theta(Evo_cut:i_chain,j)),'-o','Color',[204/255,204/255,204/255],'LineWidth',0.5,'MarkerSize',5,'MarkerFaceColor','k')
                elseif theta_scale(j,:) == 'lin'
                    plot(theta(Evo_cut:i_chain,i),theta(Evo_cut:i_chain,j),'-o','Color',[204/255,204/255,204/255],'LineWidth',0.5,'MarkerSize',5,'MarkerFaceColor','k')
                end
            end
            hold on
            if theta_scale(i,:) == 'log'
                if theta_scale(j,:) == 'log'
                    plot(log10(theta(i_chain,i)),log10(theta(i_chain,j)),'bo','MarkerSize',8,'MarkerFaceColor','b')
                elseif theta_scale(j,:) == 'lin'
                    plot(log10(theta(i_chain,i)),theta(i_chain,j),'bo','MarkerSize',8,'MarkerFaceColor','b')
                end
            elseif theta_scale(i,:) == 'lin'
                if theta_scale(j,:) == 'log'
                    plot(theta(i_chain,i),log10(theta(i_chain,j)),'bo','MarkerSize',8,'MarkerFaceColor','b')
                elseif theta_scale(j,:) == 'lin'
                    plot(theta(i_chain,i),theta(i_chain,j),'bo','MarkerSize',8,'MarkerFaceColor','b')
                end
            end
            
            if theta_scale(i,:) == 'log'
                theta_i_temp=log10(theta(i_chain,i));
                if theta_scale(j,:) == 'log'
                    theta_j_temp=log10(theta(i_chain,j));
                    F_post_norm = mvnpdf([X_post_norm(:),Y_post_norm(:)],[log10(theta(i_chain,i)), log10(theta(i_chain,j))],prop_cov(:,:,i_chain).*(DR_par(i_chain))^(i_DR-1));
                elseif theta_scale(j,:) == 'lin'
                    theta_j_temp=theta(i_chain,j);
                    F_post_norm = mvnpdf([X_post_norm(:),Y_post_norm(:)],[log10(theta(i_chain,i)), theta(i_chain,j)],prop_cov(:,:,i_chain).*(DR_par(i_chain))^(i_DR-1));
                end
            elseif theta_scale(i,:) == 'lin'
                theta_i_temp=theta(i_chain,i);
                if theta_scale(j,:) == 'log'
                    theta_j_temp=log10(theta(i_chain,j));
                    F_post_norm = mvnpdf([X_post_norm(:),Y_post_norm(:)],[theta(i_chain,i), log10(theta(i_chain,j))],prop_cov(:,:,i_chain).*(DR_par(i_chain))^(i_DR-1));
                elseif theta_scale(j,:) == 'lin'
                    theta_j_temp=theta(i_chain,j);
                    F_post_norm = mvnpdf([X_post_norm(:),Y_post_norm(:)],[theta(i_chain,i), theta(i_chain,j)],prop_cov(:,:,i_chain).*(DR_par(i_chain))^(i_DR-1));
                end
            end
            F_post_norm = reshape(F_post_norm,length(A_x_Space_lin),length(A_y_Space_lin));
            contour(A_x_Space_lin,A_y_Space_lin,F_post_norm,10)
            
            [C_vec C_val]=eig(prop_cov(:,:,i_chain));
            xx=ones(2,1).*theta_i_temp;
            yy=ones(2,1).*theta_j_temp;
            vx=[C_vec(1,1),C_vec(1,2)]';
            vy=[C_vec(2,1),C_vec(2,2)]';
            quiver(xx,yy,vx/10,vy/10,'Linewidth',3,'Color','r')

            if i_DR==nb_DR
                if theta_scale(i,:) == 'log'
                    if theta_scale(j,:) == 'log'
                        plot(log10(theta(i_chain+1,i)),log10(theta(i_chain+1,j)),'or','MarkerSize',8,'MarkerFaceColor','r')
                    elseif theta_scale(j,:) == 'lin'
                        plot(log10(theta(i_chain+1,i)),theta(i_chain+1,j),'or','MarkerSize',8,'MarkerFaceColor','r')
                    end
                elseif theta_scale(i,:) == 'lin'
                    if theta_scale(j,:) == 'log'
                        plot(theta(i_chain+1,i),log10(theta(i_chain+1,j)),'or','MarkerSize',8,'MarkerFaceColor','r')
                    elseif theta_scale(j,:) == 'lin'
                        plot(theta(i_chain+1,i),theta(i_chain+1,j),'or','MarkerSize',8,'MarkerFaceColor','r')
                    end
                end
            end
            if theta_scale(i,:) == 'log'
                str_x = ['log_{10}(', strtrim(theta_names(i,:)),')'];
            elseif theta_scale(i,:) == 'lin'
                str_x = strtrim(theta_names(i,:));
            end
            if theta_scale(i,:) == 'log'
                str_y = ['log_{10}(', strtrim(theta_names(j,:)),')'];
            elseif theta_scale(i,:) == 'lin'
                str_y = strtrim(theta_names(j,:));
            end
            if PlottingFormat(1,:) == 'Visual'
                xlabel(str_x);
                ylabel(str_y);
            elseif PlottingFormat(1,:) == 'Papers' 
                xlabel(str_x,'Interpreter','latex');
                ylabel(str_y,'Interpreter','latex');
            end
            if theta_scale(i,:) == 'log'
                if theta_scale(j,:) == 'log'
                    set(gca,'Xlim',[min(log10(theta(Evo_cut:end,i))), max(log10(theta(Evo_cut:end,i)))],'ylim',[min(log10(theta(Evo_cut:end,j))), max(log10(theta(Evo_cut:end,j)))]); 
                elseif theta_scale(j,:) == 'lin'
                    set(gca,'Xlim',[min(log10(theta(Evo_cut:end,i))), max(log10(theta(Evo_cut:end,i)))],'ylim',[min(theta(Evo_cut:end,j)), max(theta(Evo_cut:end,j))]); 
                end
            elseif theta_scale(i,:) == 'lin'
                if theta_scale(j,:) == 'log'
                    set(gca,'Yscale','log','Xlim',[min(theta(Evo_cut:end,i)), max(theta(Evo_cut:end,i))],'ylim',[min(log10(theta(Evo_cut:end,j))), max(log10(theta(Evo_cut:end,j)))]); 
                elseif theta_scale(j,:) == 'lin'
                    set(gca,'Xlim',[min(theta(Evo_cut:end,i)), max(theta(Evo_cut:end,i))],'ylim',[min(theta(Evo_cut:end,j)), max(theta(Evo_cut:end,j))]); 
                end
            end
            if PlottingFormat(1,:) == 'Visual'
                set(gca,'FontSize',30, 'FontName','PT Sans')
                set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
            elseif PlottingFormat(1,:) == 'Papers'
                set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
                set(gcf, 'PaperPositionMode', 'auto');
            end
            hold off
            
            pause(pause_1)
            xlim([.2 2])
            ylim([0 200])
            pbaspect([1.5 1 1])
            pause(pause_2)

            i_chain=i_chain+1;
            
        end

    end

    i_figure=i_figure+1;

end
