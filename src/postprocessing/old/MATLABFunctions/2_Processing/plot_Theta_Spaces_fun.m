function i_figure = plot_Theta_Spaces_fun(PlottingFormat, i_figure, proc, i, j, nb_DR, clean_flag, ...
                                          plot_Theta_Spaces_Points, plot_Theta_Spaces_Hist, plot_Theta_Spaces_Pcolor, ...
                                          plot_Theta_Spaces_Posterior_Surf, plot_Theta_Spaces_Posterior_Pcolor, ...
                                          plot_Theta_Spaces_Posterior_Normalized, plot_Theta_Spaces_Evo, Evo_start, Evo_jump, Evo_cut, ...
                                          Theta_Spaces_pcolor_nbins)
    filename = './theta_name.dat';
    delimiter = '\t';
    formatSpec = '%s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true,  'ReturnOnError', false);
    fclose(fileID);
    theta_name_vec = dataArray{:, 1};
    clearvars filename delimiter formatSpec fileID dataArray ans;
    theta_labels = theta_name_vec;
    theta_names = char(theta_name_vec);

    filename = './theta_data.dat';
    delimiter = ' ';
    startRow = 2;
    formatSpec = '%s%f%*s%*s%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'MultipleDelimsAsOne', true, 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
    fclose(fileID);
    theta_scale = char(dataArray{:, 1});
    theta_data = dataArray{:, 2};
    clearvars filename delimiter startRow formatSpec fileID dataArray ans;
            
    filename = './Prior.dat';
    startRow = 2;
    formatSpec = '%*40s%20f%20f%20f%f%[^\n\r]';
    fileID = fopen(filename,'r');
    dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
    fclose(fileID);
    MuPrior = dataArray{:, 1};
    SigmaPrior = dataArray{:, 2};
    UnifMin = dataArray{:, 3};
    UnifMax = dataArray{:, 4};
    clearvars filename startRow formatSpec fileID dataArray ans;
    nb_dim=length(UnifMin);
    
    y_min = UnifMin(j);
    y_max = UnifMax(j);
    x_min = UnifMin(i);
    x_max = UnifMax(i);
    
    clearvars UnifMin UnifMax MuPrior SigmaPrior
    
    if ( clean_flag == 0 )
    
        theta = [];
        like  = [];
        post  = [];
        theta_start = [];

        like_dirty_max_value_temp = 0;
        post_dirty_max_value_temp = 0;

        for i=proc

            filename = strcat('./dirty_chain_',num2str(i),'.dat');
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
            Dirty_Chain = [dataArray{1:end-1}];
            clearvars filename startRow formatSpec fileID dataArray ans;
            theta_dirty_temp(:,:)=Dirty_Chain(:,1:nb_dim);
            like_dirty_temp(:,1)=Dirty_Chain(:,nb_dim+1);
            post_dirty_temp(:,1)=Dirty_Chain(:,nb_dim+2);
            theta_start(:,i)=Dirty_Chain(1,1:nb_dim);

            [like_dirty_max_value_temp_temp, like_dirty_max_pos_temp_temp]=max(like_dirty_temp);
            [post_dirty_max_value_temp_temp, post_dirty_max_pos_temp_temp]=max(post_dirty_temp);
            if like_dirty_max_value_temp_temp > like_dirty_max_value_temp
                i_proc_like_dirty_max = i;
                like_dirty_max_value_temp = like_dirty_max_value_temp_temp;
            end
            if post_dirty_max_value_temp_temp > post_dirty_max_value_temp
                i_proc_post_dirty_max = i;
                post_dirty_max_value_temp = post_dirty_max_value_temp_temp;
            end

            theta = [theta; theta_dirty_temp];
            like  = [like; like_dirty_temp];
            post  = [post; post_dirty_temp];

            clearvars theta_dirty_temp like_dirty_temp post_dirty_temp

        end

        [like_max_value, like_max_pos]=max(like);
        [post_max_value, post_max_pos]=max(post);

        theta_clean = [];
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

            theta_clean = [theta_clean; theta_temp];

            clearvars theta_temp

        end

        A=[];
        for j=1:nb_dim
            if theta_scale(j,:) == 'lin'
                A=[A, theta_clean(:,j)];
            elseif theta_scale(j,:) == 'log'
                A=[A, log10(theta_clean(:,j))];
            end
        end
        [R,P] = corrcoef(A);
        corr_parpar=R;
        clearvars theta_clean A P R
        
    else
        
        theta = [];
        like  = [];
        post  = [];

        like_max_value_temp = 0;
        post_max_value_temp = 0;

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
            like_temp(:,1)=Clean_Chain(:,nb_dim+1);
            post_temp(:,1)=Clean_Chain(:,nb_dim+2);

            [like_max_value_temp_temp, like_max_pos_temp_temp]=max(like_temp);
            [post_max_value_temp_temp, post_max_pos_temp_temp]=max(post_temp);
            if like_max_value_temp_temp > like_max_value_temp
                i_proc_like_max = i;
                like_max_value_temp = like_max_value_temp_temp;
            end
            if post_max_value_temp_temp > post_max_value_temp
                i_proc_post_max = i;
                post_max_value_temp = post_max_value_temp_temp;
            end

            theta = [theta; theta_temp];
            like  = [like; like_temp];
            post  = [post; post_temp];

            clearvars theta_temp like_temp post_temp

        end

        [like_max_value, like_max_pos]=max(like);
        [post_max_value, post_max_pos]=max(post);
        
        A=[];
        for j=1:nb_dim
            if theta_scale(j,:) == 'lin'
                A=[A, theta(:,j)];
            elseif theta_scale(j,:) == 'log'
                A=[A, log10(theta(:,j))];
            end
        end
        [R,P] = corrcoef(A);
        corr_parpar=R;
        clearvars A P R

    end
    
    nb_dim=size(theta,2);
                                      
    if theta_scale(i,:) == 'log'
        A_theta(:,1)=log10(theta(:,i));
        A_x_Space=logspace(log10(x_min),log10(x_max),Theta_Spaces_pcolor_nbins);
    elseif theta_scale(i,:) == 'lin'
        A_theta(:,1)=theta(:,i);
        A_x_Space=linspace(x_min,x_max,Theta_Spaces_pcolor_nbins);
    end
    if theta_scale(j,:) == 'log'
       A_theta(:,2)=log10(theta(:,j));
       A_y_Space=logspace(log10(y_min),log10(y_max),Theta_Spaces_pcolor_nbins);
    elseif theta_scale(j,:) == 'lin'
       A_theta(:,2)=theta(:,j);
       A_y_Space=linspace(y_min,y_max,Theta_Spaces_pcolor_nbins);
    end
    A_x_Space_lin=linspace(x_min,x_max,Theta_Spaces_pcolor_nbins);
    A_y_Space_lin=linspace(y_min,y_max,Theta_Spaces_pcolor_nbins);
    A_hist=hist3(A_theta,[Theta_Spaces_pcolor_nbins,Theta_Spaces_pcolor_nbins]);

    [X_post_surf,Y_post_surf] = meshgrid(A_x_Space,A_y_Space);
    f_post_surf = scatteredInterpolant(theta(:,i),theta(:,j),post(:));
    Z_post_surf = f_post_surf(X_post_surf,Y_post_surf);
    
    
    
    if plot_Theta_Spaces_Points == 1
       plot_Theta_Spaces_Points
        
        i_figure = plot_Theta_Spaces_Points_fun(PlottingFormat, i_figure, i, j, theta_scale, theta_names, theta, like_max_pos, post_max_pos, corr_parpar);      

    end
        
    
    
    if plot_Theta_Spaces_Hist == 1
       plot_Theta_Spaces_Hist

        i_figure = plot_Theta_Spaces_Hist_fun(PlottingFormat, i_figure, i, j, theta_scale, theta_names, ...
                                              A_theta, Theta_Spaces_pcolor_nbins);

    end
    
    

    if plot_Theta_Spaces_Pcolor == 1
       plot_Theta_Spaces_Pcolor
        
        i_figure = plot_Theta_Spaces_Pcolor_fun(PlottingFormat, i_figure, i, j, theta_names, ...
                                                A_x_Space, A_y_Space, A_hist);

    end
    
    

    if plot_Theta_Spaces_Posterior_Surf == 1
       plot_Theta_Spaces_Posterior_Surf

        i_figure = plot_Theta_Space_Posterior_Surf_fun(PlottingFormat, i_figure, i, j, theta_scale, theta_names, post, ...
                                                       A_theta, X_post_surf, Y_post_surf, Z_post_surf);
        
    end

    
    
    if plot_Theta_Spaces_Posterior_Pcolor == 1
       plot_Theta_Spaces_Posterior_Pcolor

        i_figure = plot_Theta_Space_Posterior_Pcolor_fun(PlottingFormat, i_figure, i, j, theta_names, ...
                                                          A_theta, X_post_surf, Y_post_surf, Z_post_surf);

    end

    
    
    if plot_Theta_Spaces_Posterior_Normalized == 1
       plot_Theta_Spaces_Posterior_Normalized

        i_figure = plot_Theta_Space_Posterior_Normalized_fun(PlottingFormat, i_figure, i, j, theta_scale, theta_names, ...
                                                             A_theta, A_x_Space_lin, A_y_Space_lin);
        
    end
    
    
    
    if plot_Theta_Spaces_Evo == 1
       plot_Theta_Spaces_Evo
        
        for ii=1:1%length(proc)
            
            i_proc=proc(ii);
             
            if clean_flag == 1
                filename = strcat('./clean_chain_',num2str(i_proc),'.dat');
            elseif clean_flag == 0
                filename = strcat('./dirty_chain_',num2str(i_proc),'.dat');
            end 
            startRow = 2;
            str_format='%20f';
            for ik=2:nb_dim-1
                str_format = strcat(str_format,'%20f');
            end
            str_format = strcat(str_format,'%f%[^\n\r]');
            formatSpec = str_format;
            fileID = fopen(filename,'r');
            dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);
            fclose(fileID);
            theta_proc_temp = [dataArray{1:end-1}];
            clearvars filename startRow formatSpec fileID dataArray ans;
            
            theta_proc=theta_proc_temp;
            
            if theta_scale(i,:) == 'log'
                A_theta_proc(:,1)=log10(theta_proc(:,i));
            elseif theta_scale(i,:) == 'lin'
                A_theta_proc(:,1)=theta_proc(:,i);
            end
            A_x_Space_lin_proc=linspace(x_min,x_max,Theta_Spaces_pcolor_nbins);
            
            if theta_scale(j,:) == 'log'
               A_theta_proc(:,2)=log10(theta_proc(:,j));
            elseif theta_scale(j,:) == 'lin'
               A_theta_proc(:,2)=theta_proc(:,j);
            end
            A_y_Space_lin_proc=linspace(y_min,y_max,Theta_Spaces_pcolor_nbins);
            
            [X_post_norm_proc,Y_post_norm_proc] = meshgrid(A_x_Space_lin_proc,A_y_Space_lin_proc);

            filename = strcat('./prop_dist_',num2str(i_proc),'.dat');
            formatSpec = '%20f%20f%20f%20f%f%[^\n\r]';
            fileID = fopen(filename,'r');
            dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '',  'ReturnOnError', false);
            fclose(fileID);
            i_sample = dataArray{:, 1};
            i_sample_def = dataArray{:, 2};
            clearvars filename formatSpec fileID dataArray ans;

            filename = strcat('./prop_dist_',num2str(i_proc),'.dat');
            str_format='%*40s%20f';
            for ii=1:(nb_dim+1)*(nb_dim)/2-2
                str_format = strcat(str_format,'%20f');
            end
            str_format = strcat(str_format,'%f%[^\n\r]');
            formatSpec = str_format;
            fileID = fopen(filename,'r');
            dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '',  'ReturnOnError', false);
            fclose(fileID);
            prop_cov_temp_temp = [dataArray{1:end-1}];
            clearvars filename formatSpec fileID dataArray ans;

            for i_chain=1:size(prop_cov_temp_temp,1)
                jk=1;
                for jj=1:nb_dim
                    for k=jj:nb_dim
                        prop_cov_temp(jj,k,i_chain)=prop_cov_temp_temp(i_chain,jk);
                        prop_cov_temp(k,jj,i_chain)=prop_cov_temp_temp(i_chain,jk);
                        jk=jk+1;
                    end 
                end
            end
            clear prop_cov_temp_temp

            prop_cov=[];
            if length(i_sample)>1
                if clean_flag==1
                    i_clean=1;
                    while i_sample_def(i_clean)<1
                        i_clean=i_clean+1;
                    end
                    prop_cov(:,:,1)=prop_cov_temp(:,:,1);
                    for ijk=i_clean:length(i_sample_def)
                        prop_cov(:,:,i_sample_def(ijk))=prop_cov_temp(:,:,ijk);
                    end
                elseif clean_flag==0
                    for ijk=2:size(prop_cov_temp,3)-1
                        for ijkl=1:i_sample(2)
                            prop_cov(:,:,ijkl)=prop_cov_temp(:,:,1);
                        end
                    end
                    for ijk=2:size(prop_cov_temp,3)-1
                        for ijkl=i_sample(ijk):i_sample(ijk+1)
                            prop_cov(:,:,ijkl)=prop_cov_temp(:,:,ijk);
                        end
                    end
                end
            else
                for i_chain=1:size(theta,1)
                    prop_cov(:,:,i_chain)=prop_cov_temp(:,:,1);
                end
            end
            
            prop_cov_daje(1,1,:)=prop_cov(i,i,:);
            prop_cov_daje(2,2,:)=prop_cov(j,j,:);
            prop_cov_daje(1,2,:)=prop_cov(i,j,:);
            prop_cov_daje(2,1,:)=prop_cov_daje(1,2,:);

            i_figure = plot_Theta_Spaces_Evo_fun(PlottingFormat, i_figure, i, j, theta_scale, theta_names, theta_proc, nb_DR, i_proc, clean_flag, ... 
                                                 A_x_Space_lin_proc, A_y_Space_lin_proc, X_post_norm_proc, Y_post_norm_proc, ... 
                                                 Evo_start, Evo_jump, Evo_cut, prop_cov_daje);
        
          
        end
            
    end
    
end
