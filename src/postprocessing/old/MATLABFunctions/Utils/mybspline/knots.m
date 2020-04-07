function [knots_vector] = knots(nb_cpts,k)
% m = number of knots
m = k + nb_cpts;

knots_vector = zeros(1,m);

for i = 2:m
   if i > k && i < nb_cpts+2
       knots_vector(i) = knots_vector(i-1)+1;     
   else
       knots_vector(i) = knots_vector(i-1);
   end
end

end

