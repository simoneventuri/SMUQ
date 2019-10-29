function []=save_all(path_)

    path_final = strcat(path_,'Figure_');

    h = get(0,'children');
    for i=1:length(h)
      saveas(h(i), [path_final num2str(i)], 'fig');
    end
    
end