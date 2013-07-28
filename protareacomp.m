%% read file
prot=xlsread('X:\Elite\Mohmd\MCR6RepArea.xlsx')

%% plot
plot(prot(:,6),prot(:,11),'r.')

%% correlation
[rho val]=corr(prot(:,6)./prot(:,11),prot(:,8)./prot(:,11),'rows','pairwise')
[rho val]=corr(prot(:,6),prot(:,8),'rows','pairwise')

%% parameters
for i=6:11
    i
    m=max(isfinite(prot(:,i)./prot(:,11)))
    x=prot(:,i)\prot(:,11)
end
