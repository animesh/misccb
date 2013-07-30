%% read file
prot=xlsread('X:\Elite\Mohmd\Area.xlsx')

%% plot
hr=prot(1,:)
prot=prot(2:end,:)
hr
plot(prot(:,6),prot(:,2),'r.')

%% correlation
[rho val]=corr(prot(:,6)./prot(:,11),prot(:,8)./prot(:,11),'rows','pairwise')
[rho val]=corr(prot(:,6),prot(:,8),'rows','pairwise')

%% parameters
for i=1:6
    i
    mxv(i)=max(isfinite(prot(:,i)./prot(:,6)));
    x(i)=prot(:,6)\prot(:,i);
    [rho(i) val(i)]=corr(prot(:,i),prot(:,6),'rows','pairwise');
end
mxv,x,rho,val,hr
plot(hr,x,'b.')
