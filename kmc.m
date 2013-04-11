p = [ 1 1; 2 1; 4 3; 5 4]
clus = 2
c=zeros(size(p))
[R, C]=size(p);
if R<=clus, 
    mp=[p, 1:R];
else
    p = randperm(size(p,2));  
    for i=1:clus
        c(i,:)%=p(p(i),:); 
    end
end

temp=zeros(R,1);

while f<1
        d=dist(p);
        [z,g]=min(d,[],2);
        for i=1:clus
            f=find(g==i);
        end
        f=f-1
end

p,g
