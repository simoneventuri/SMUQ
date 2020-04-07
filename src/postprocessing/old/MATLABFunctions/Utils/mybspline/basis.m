function [basisv] = basis(u,k,nb_cpts,knotsv)
% degree : p
p = k-1;

basisv = zeros(1,nb_cpts);

if u == knotsv(1)
    basisv(1) = 1;
    return
elseif u == knotsv(length(knotsv))
    basisv(nb_cpts) = 1;
    return
end

basisalt = zeros(1,nb_cpts);
for i = k:length(knotsv)-1
    if u >= knotsv(i) && u< knotsv(i+1)
        j = i;
        basisalt(j) = 1;
        break
    end
end

for i = 1:p
    basisalt(j-i) = (knotsv(j+1)-u)/(knotsv(j+1)-knotsv(j-i+1)) * basisalt(j-i+1);
    
    for ii = j-i+1:j-1
        basisalt(ii) = (u-knotsv(ii))/(knotsv(ii+i)-knotsv(ii))*basisalt(ii)+(knotsv(ii+i+1)-u)/(knotsv(ii+i+1)-knotsv(ii+1))*basisalt(ii+1);
    end
    
    basisalt(j) = (u-knotsv(j))/(knotsv(j+i)-knotsv(j))*basisalt(j);
end

basisv = basisalt;
end
            





