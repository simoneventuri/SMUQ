function [x,y] = mybspline(cpts, k, xyl)
% cpts : [x1,y1;x2,y2;....] array of control points
% k : order
% xyl : number of x,y pairs for the curve
% knotsv : clamped knots vector

x = zeros(xyl,1);
y = zeros(xyl,1);

nb_cpts = size(cpts,1);

knotsv = knots(nb_cpts,k);

u = 0;
ustep = knotsv(length(knotsv)) / (xyl-1);

for i = 1:xyl
    
    if (knotsv(length(knotsv)) - u) < 1e-5
        u = knotsv(length(knotsv));
    end
    
    basisv = basis(u,k,nb_cpts,knotsv);
    
    x(i) = sum(cpts(:,1).*basisv');
    y(i) = sum(cpts(:,2).*basisv');
    
    u = u + ustep;

end

