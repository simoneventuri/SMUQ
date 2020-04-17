function i_figure = plot_SPES_posterior_fun(PlottingFormat, i_figure, save_fig, plot_y_interval, proc, i_def, data_scale, x_nodes, y_data, y_Max_Like, y_Max_Post)
        
                                           
    video_flag = 0
    plot_y_interval = 1
        nb_of_sigma = 3
    plot_Max_Like   = 1
        i_proc_max_like = 1
    plot_Max_Post   = 1
        i_proc_max_post = 1
   

    filename = strcat('../input/SPES/SPES_for_MATLAB.dat');
    delimiter = '';
    formatSpec = '%f%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter,  'ReturnOnError', false);
    fclose(fileID);
    SPES_array = dataArray{:, 1};
    clearvars filename delimiter formatSpec fileID dataArray ans;

    nb_angles  = SPES_array(1);
    angles=[];
    angles(1:nb_angles) = SPES_array(2:1+nb_angles);
    N_R(1) = SPES_array(2+nb_angles);
    Rmin(1)= SPES_array(3+nb_angles);
    Rmax(1)= SPES_array(4+nb_angles);
    angles = angles.*pi./180;
    
    
    if plot_y_interval == 1

        y_sum=y_data(:,1).*0.d0;
        y_sq_sum=y_data(:,1).*0.d0;

        for i=proc

            filename = strcat('./x_y_Sum_',num2str(i),'.dat');
            filename
            delimiter = ' ';
            startRow = 2;
            formatSpec = '%f%f%[^\n\r]';
            fileID = fopen(filename,'r');
            dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
            fclose(fileID);
            x_sum = dataArray{:, 1};
            y_sum = y_sum + dataArray{:, 2};
            clearvars filename delimiter startRow formatSpec fileID dataArray ans;

            filename = strcat('x_y_Sq_Sum_',num2str(i),'.dat');
            delimiter = ' ';
            startRow = 2;
            formatSpec = '%f%f%[^\n\r]';
            fileID = fopen(filename,'r');
            dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
            fclose(fileID);
            x_sq_sum = dataArray{:, 1};
            y_sq_sum = y_sq_sum + dataArray{:, 2};
            clearvars filename delimiter startRow formatSpec fileID dataArray ans;

        end
        
        y_mean=y_sum./(i_def);
        y_st_deviation = sqrt(y_sq_sum ./ (i_def) - y_mean.^2);
        y_max = y_mean + nb_of_sigma*y_st_deviation;
        y_min = y_mean - nb_of_sigma*y_st_deviation;
        
    end
    
    
    jj=1;
    for k=1:nb_angles
    
        ii=1;
        for i=1:N_R(1)

            r(1)           = Rmin(1) + (i-1)*(Rmax(1)-Rmin(1))/(N_R(1)-1);
            R1(1:N_R(1),i) = r(1);
            
            RRR1(i) = r(1);


            for j=1:i

                r(2)    = Rmin(1) + (j-1)*(Rmax(1)-Rmin(1))/(N_R(1)-1);
                R2(j,1:N_R(1)) = r(2);
                r(3)    = (sqrt(r(1)^2 + r(2)^2 - 2.d0*r(1)*r(2)*cos(angles(k))));
                
                RR1(ii) = r(1);
                RR2(ii) = r(2);
                RRR2(j) = r(2);
                

                V_data(i,j,k)  = y_data(jj);
                V_data(j,i,k)  = y_data(jj);
                y_data_vec(ii,k) = y_data(jj);

                if plot_Max_Like == 1
                    V_Max_Like(i,j,k) = y_Max_Like(jj,i_proc_max_like);
                    V_Max_Like(j,i,k) = y_Max_Like(jj,i_proc_max_like);
                end

                if plot_Max_Post == 1
                    V_Max_Post(i,j,k) = y_Max_Post(jj,i_proc_max_post);   
                    V_Max_Post(j,i,k) = y_Max_Post(jj,i_proc_max_post);   
                end
                
                if plot_y_interval == 1
                
                    if data_scale == 'lin'
                        V_min(i,j,k)  = y_min(jj);
                        V_mean(i,j,k) = y_mean(jj);
                        V_max(i,j,k)  = y_max(jj);
                        V_min(j,i,k)  = y_min(jj);
                        V_mean(j,i,k) = y_mean(jj);
                        V_max(j,i,k)  = y_max(jj);
                    elseif data_scale == 'log'
                        V_min(i,j,k)  = 10.^y_min(jj);
                        V_mean(i,j,k) = 10.^y_mean(jj);
                        V_max(i,j,k)  = 10.^y_max(jj);
                        V_min(j,i,k)  = 10.^y_min(jj);
                        V_mean(j,i,k) = 10.^y_mean(jj);
                        V_max(j,i,k)  = 10.^y_max(jj);
                    end 
                    
                end

                ii=ii+1;
                jj=jj+1;

            end
            
        end
        
        figure(i_figure)
%         zmin = -100;
%         zmax = +100;
%         zinc = (zmax - zmin) / 20;
%         zlevs = zmin:zinc:zmax;
        if data_scale == 'lin'
            C=contour(R1(:,:),R2(:,:),V_data(:,:,k));%,zlevs);
        elseif data_scale == 'log'
            C=contour(R1(:,:),R2(:,:),10.d0.^V_data(:,:,k));%,zlevs);
        end
        str_title = ['NASA PES at N-N-N = ', num2str(angles(k)*180.d0/pi)];
        if PlottingFormat(1,:) == 'Visual'
            xlabel('R_{N-N}')
            ylabel('R_{N-N}')
            title(str_title)
        elseif PlottingFormat(1,:) == 'Papers'    
            xlabel('R_{N-N}','Interpreter','latex')
            ylabel('R_{N-N}','Interpreter','latex')
            title(str_title,'Interpreter','latex')
        end
        if PlottingFormat(1,:) == 'Visual'
            set(gca,'FontSize',30, 'FontName','PT Sans')
            set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
        elseif PlottingFormat(1,:) == 'Papers'
            set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
            set(gcf, 'PaperPositionMode', 'auto');
        end 
        i_figure=i_figure+1;

        figure(i_figure)
        if data_scale == 'lin'
            surf(R1(:,:),R2(:,:),V_data(:,:,k))
        elseif data_scale == 'log'
            surf(R1(:,:),R2(:,:),10.d0.^(V_data(:,:,k)))
        end
        str_title = ['NASA PES at N-N-N = ', num2str(angles(k)*180.d0/pi)];
        if PlottingFormat(1,:) == 'Visual'
            title(str_title)
        elseif PlottingFormat(1,:) == 'Papers'
            title(str_title,'Interpreter','latex')
        end
        if PlottingFormat(1,:) == 'Visual'
            set(gca,'FontSize',30, 'FontName','PT Sans')
            set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
        elseif PlottingFormat(1,:) == 'Papers'
            set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
            set(gcf, 'PaperPositionMode', 'auto');
        end 
        i_figure=i_figure+1;

        
        
        if plot_Max_Like == 1

%             figure(i_figure)
%             zmin = -100;
%             zmax = +100;
%             zinc = (zmax - zmin) / 20;
%             zlevs = zmin:zinc:zmax;
%             C=contour(R1,R2,V_Max_Like,zlevs);
%             clabel(C)
%             hold on
%             [ppx,ppy] = gradient(V_Max_Like,(Rmax(1)-Rmin(1))/(N_R(1)-1),(Rmax(2)-Rmin(2))/(N_R(2)-1));
%             quiver(RRR1,RRR2,ppx,ppy)
%             xlabel('R_{N-N}')
%             ylabel('R_{N-N}')
%             title('LEPS PES, Max Likelihood')
%             set(gca,'FontSize',25, 'FontName','PT Sans');
%             i_figure=i_figure+1;

            V_rel_diff(:,:,k) = (V_Max_Like(:,:,k) - V_data(:,:,k)) .* 1.d2 ./ V_data(:,:,k);

            figure(i_figure)
            [xx yy] = meshgrid(Rmin(1):0.01:Rmax(1));  %force it to interpolate at every 10th pixel
            if data_scale == 'lin'
                surf(xx,yy,interp2(R1(:,:),R2(:,:),V_rel_diff(:,:,k),xx,yy),'LineStyle', '-','LineWidth',0.1,'EdgeColor', [0.25, 0.25, 0.25], 'FaceColor', 'interp')
            elseif data_scale == 'log'
                surf(xx,yy,interp2(R1(:,:),R2(:,:),V_rel_diff(:,:,k),xx,yy),'LineStyle', '-','LineWidth',0.1,'EdgeColor', [0.25, 0.25, 0.25], 'FaceColor', 'interp')
            end
            grid off
            str_title = ['NASA vs LEPS, Relative Difference in Values at N-N-N = ', num2str(angles(k)*180.d0/pi)];
            colormap(jet)
            az = -22;
            el = 40;
            view(az, el);
            h = colorbar;
            material metal;
            if PlottingFormat(1,:) == 'Visual'
                xlabel('R_{N-N}')
                ylabel('R_{N-N}')
                title(str_title)
                set(get(h,'title'),'string','Relative Erorr (%)');
            elseif PlottingFormat(1,:) == 'Papers'
                xlabel('R_{N-N}','Interpreter','latex')
                ylabel('R_{N-N}','Interpreter','latex')
                title(str_title,'Interpreter','latex')
                set(get(h,'title'),'string','Relative Erorr (%)','Interpreter','latex');
            end
            if PlottingFormat(1,:) == 'Visual'
                set(gca,'FontSize',30, 'FontName','PT Sans')
                set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
            elseif PlottingFormat(1,:) == 'Papers'
                set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
                set(gcf, 'PaperPositionMode', 'auto');
            end 
            i_figure=i_figure+1;

            figure(i_figure)
            clf;
            if data_scale == 'lin'
                h1=surf(R1(:,:),R2(:,:),V_Max_Like(:,:,k));
                hold on
                h2=plot3(RR2(:),RR1(:),y_data_vec(:,k),'.','MarkerSize',15,'MarkerEdgeColor','r','MarkerFaceColor','r');
                h2=plot3(RR1(:),RR2(:),y_data_vec(:,k),'.','MarkerSize',15,'MarkerEdgeColor','r','MarkerFaceColor','r');
            elseif data_scale == 'log'
                h1=surf(R1(:,:),R2(:,:),10.d0.^(V_Max_Like(:,:,k)));
                hold on
                h2=plot3(RR2(:),RR1(:),10.d0.^(y_data_vec(:,k)),'.','MarkerSize',15,'MarkerEdgeColor','r','MarkerFaceColor','r');
                h2=plot3(RR1(:),RR2(:),10.d0.^(y_data_vec(:,k)),'.','MarkerSize',15,'MarkerEdgeColor','r','MarkerFaceColor','r');
            end
            str_title = ['N-N-N = ', num2str(angles(k)*180.d0/pi)];
            if PlottingFormat(1,:) == 'Visual'
                %xlabel('R_{N-N} [A]')
                %ylabel('R_{N-N} [A]')
                zlabel('E [kcal/m]')
                %title(str_title)
                %legend([h1,h2],'Max Likelihood for LEPS PES','NASA PES');
            elseif PlottingFormat(1,:) == 'Papers'
                %xlabel('R_{N-N} [A]','Interpreter','latex')
                %ylabel('R_{N-N} [A]','Interpreter','latex')
                zlabel('E [kcal/m]','Interpreter','latex')
                %title(str_title,'Interpreter','latex')
                %legend([h1,h2],'Max Likelihood for LEPS PES','NASA PES','Interpreter','latex');
            end
            if PlottingFormat(1,:) == 'Visual'
                set(gca,'FontSize',30, 'FontName','PT Sans')
                set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
            elseif PlottingFormat(1,:) == 'Papers'
                set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
                set(gcf, 'PaperPositionMode', 'auto');
            end 
            if video_flag == 1
                daspect([2,2,1000]);axis tight;
                OptionZ.FrameRate=50;OptionZ.Duration=10;OptionZ.Periodic=true;
                CaptureFigVid([-20,10;-110,10;-190,80;-290,10;-380,10], 'MaxLike',OptionZ)
            end 
            i_figure=i_figure+1;

%             [px_MaxLike,py_MaxLike] = gradient(V_Max_Like,(Rmax(1)-Rmin(1))/(N_R(1)-1),(Rmax(2)-Rmin(2))/(N_R(2)-1));
%             grad_mod_MaxLike = (px_MaxLike.^2+py_MaxLike.^2).^(0.5d0);
% 
%             grad_mod_rel_diff=(grad_mod_MaxLike-grad_mod_NASA).*100.d0./grad_mod_NASA;
% 
%             figure(i_figure)
%             %surf(R1,R2,grad_mod_rel_diff);
%             [xx yy] = meshgrid(Rmin(1):0.01:Rmax(1));  %force it to interpolate at every 10th pixel
%             surf(xx,yy,interp2(R1,R2,grad_mod_rel_diff,xx,yy),'LineStyle', '-','LineWidth',0.1,'EdgeColor', [0.25, 0.25, 0.25], 'FaceColor', 'interp')
%             grid off
%             xlabel('R_{N-N}')
%             ylabel('R_{N-N}')
%             %title('NASA vs LEPS, Relative Difference in Gradients')
%             set(gca,'FontSize',25, 'FontName','PT Sans');
%             colormap(jet)
%             az = -22;
%             el = 40;
%             view(az, el);
%             material metal;
%             h = colorbar;
%             set(get(h,'title'),'string','Relative Erorr (%)');
%     %         daspect([2,2,200]);axis tight;
%     %         OptionZ.FrameRate=50;OptionZ.Duration=10;OptionZ.Periodic=true;
%     %         CaptureFigVid([-20,10;-110,10;-190,80;-290,10;-380,10], 'GradientError',OptionZ)
%             if PlottingFormat(1,:) == 'Visual'
%                 set(gca,'FontSize',30, 'FontName','PT Sans')
%                 set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
%             elseif PlottingFormat(1,:) == 'Papers'
%                 set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
%                 set(gcf, 'PaperPositionMode', 'auto');
%             end 
%             i_figure=i_figure+1;

        end
        
        
        
        if plot_Max_Post == 1
        
            figure(i_figure)
            clf;
            
            if data_scale == 'lin'
                h1=surf(R1(:,:),R2(:,:),V_Max_Post(:,:,k));
                hold on
                h2=plot3(RR2(:),RR1(:),y_data_vec(:,k),'.','MarkerSize',15,'MarkerEdgeColor','r','MarkerFaceColor','r');
                h2=plot3(RR1(:),RR2(:),y_data_vec(:,k),'.','MarkerSize',15,'MarkerEdgeColor','r','MarkerFaceColor','r');
            elseif data_scale == 'log'
                h1=surf(R1(:,:),R2(:,:),V_Max_Post(:,:,k));
                hold on
                h2=plot3(RR2(:),RR1(:),y_data_vec(:,k),'.','MarkerSize',15,'MarkerEdgeColor','r','MarkerFaceColor','r');
                h2=plot3(RR1(:),RR2(:),y_data_vec(:,k),'.','MarkerSize',15,'MarkerEdgeColor','r','MarkerFaceColor','r');
            end
            str_title = ['N-N-N = ', num2str(angles(k)*180.d0/pi)];
            if PlottingFormat(1,:) == 'Visual'
                %xlabel('R_{N-N} [A]')
                %ylabel('R_{N-N} [A]')
                zlabel('E [kcal/m]')
                %title(str_title)
                %legend([h1,h2],'Max Posterior for LEPS PES','NASA PES');
            elseif PlottingFormat(1,:) == 'Papers'
                %xlabel('R_{N-N} [A]','Interpreter','latex')
                %ylabel('R_{N-N} [A]','Interpreter','latex')
                zlabel('E [kcal/m]','Interpreter','latex')
                %title(str_title)
                %legend([h1,h2],{'Max Posterior for LEPS PES','NASA PES'},'Interpreter','latex');
            end
            if PlottingFormat(1,:) == 'Visual'
                set(gca,'FontSize',30, 'FontName','PT Sans')
                set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
            elseif PlottingFormat(1,:) == 'Papers'
                set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
                set(gcf, 'PaperPositionMode', 'auto');
            end 
            if video_flag == 1
                daspect([2,2,1000]);axis tight;
                OptionZ.FrameRate=50;OptionZ.Duration=10;OptionZ.Periodic=true;
                CaptureFigVid([-20,10;-110,10;-190,80;-290,10;-380,10], 'MaxLike',OptionZ)
            end
            i_figure=i_figure+1;

        end
        
        
        
        if plot_y_interval == 1
        
            figure(i_figure)
            if data_scale == 'lin'
                mesh(R1(:,:),R2(:,:),V_min(:,:,k))
                %hidden off
                hold on
                surf(R1(:,:),R2(:,:),V_mean(:,:,k))
                %hidden on
                mesh(R1(:,:),R2(:,:),V_max(:,:,k))
                %hidden off
                plot3(RR2(:)',RR1(:)',y_data_vec(:,k),'.','MarkerSize',15,'MarkerEdgeColor','r','MarkerFaceColor','r')
                plot3(RR1(:)',RR2(:)',y_data_vec(:,k),'.','MarkerSize',15,'MarkerEdgeColor','r','MarkerFaceColor','r')
            elseif data_scale == 'log'
                mesh(R1(:,:),R2(:,:),V_min(:,:,k))
                %hidden off
                hold on
                surf(R1(:,:),R2(:,:),V_mean(:,:,k))
                %hidden on
                mesh(R1(:,:),R2(:,:),V_max(:,:,k))
                %hidden off
                plot3(RR2(:)',RR1(:)',10.d0.^(y_data_vec(:,k)),'.','MarkerSize',15,'MarkerEdgeColor','r','MarkerFaceColor','r')
                plot3(RR1(:)',RR2(:)',10.d0.^(y_data_vec(:,k)),'.','MarkerSize',15,'MarkerEdgeColor','r','MarkerFaceColor','r')
            end
            str_title = ['N-N-N = ', num2str(angles(k)*180.d0/pi)];
            if PlottingFormat(1,:) == 'Visual'
                %xlabel('R_{N-N} [A]')
                %ylabel('R_{N-N} [A]')
                zlabel('E [kcal/m]')
                %title(str_title)
            elseif PlottingFormat(1,:) == 'Papers'
                %xlabel('R_{N-N} [A]','Interpreter','latex')
                %ylabel('R_{N-N} [A]','Interpreter','latex')
                zlabel('E [kcal/m]','Interpreter','latex')
                %title(str_title,'Interpreter','latex')
            end
            if PlottingFormat(1,:) == 'Visual'
                set(gca,'FontSize',30, 'FontName','PT Sans')
                set(gca,'Box','off','TickDir','out','TickLength', [.02 .01],'XMinorTick','on','YMinorTick', 'on','LineWidth',1)
            elseif PlottingFormat(1,:) == 'Papers'
                set(gca,'FontSize',36, 'FontName','Palatino','TickDir','out','XMinorTick','on','YMinorTick', 'on','TickLabelInterpreter','latex');
                set(gcf, 'PaperPositionMode', 'auto');
            end 
            if video_flag == 1
                daspect([2,2,1000]);axis tight;
                OptionZ.FrameRate=50;OptionZ.Duration=10;OptionZ.Periodic=true;
                CaptureFigVid([-20,10;-110,10;-190,80;-290,10;-380,10], 'ConfInt',OptionZ)
            end 
            i_figure=i_figure+1;
    
        end
    
        
    end
    
end
