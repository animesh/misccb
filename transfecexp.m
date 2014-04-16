%% read
prot=xlsread('L:\Elite\LARS\2014\januar\SILAC 2ndparalell\PRN3R2MC3PNRhighB2gnContro2Control.xlsx');
prot=xlsread('L:\Elite\LARS\2014\januar\SILAC 2ndparalell\Multiconsensus from 18 Reports.xlsx');
prot=xlsread('L:\Elite\LARS\2014\januar\SILAC 2ndparalell\MCR18GN.xlsx');
prot=xlsread('L:\Elite\LARS\2014\januar\SILAC 2ndparalell\match2table.xlsx');

%% test
grp={'FP','FP','FP','FP','FP','FP','C','C','C','C','C','C','F','F','F','F','F','F'}
for i=1:size(prot,1)
    p(i)=anova1(prot(i,10:27),grp,'off')
end
for i=1:size(prot,1)
    s(i)=anova1(prot(i,28:45),grp,'off')
end

%% count
hist(sum(~isnan(prot),2))
sum(prot(:,5)<0.1&abs(prot(:,6))>0.58)
hist(sum(~isnan(prot)))
sum(~isnan(prot))
cnt=~isnan(prot)
hist(sum(cnt>0))
sum(prot>1)
sum(prot(cnt)',1)

%% cluster analysis
corrc=corr(prot(:,6:23))
corrprot=clustergram(corrc)
clustergram(prot(prot(:,5)<0.1&abs(prot(:,6))>0.58,13:30), 'Cluster','column', 'Colormap', redbluecmap,'ImputeFun','knnimpute')
get(ccprop)
ccprop=clustergram(corrprot, 'Colormap', redgreencmap(256),'ImputeFun','knnimpute')%,'Distance', 'mahalanobis')


%% venn

%[~,A,~]=xlsread('L:\Elite\LARS\2013\oktober\Transfection SILAC exp\140324_GLYGLY Proteiner ecfp plasmid.xls')
[~,A,~]=xlsread('L:\Elite\LARS\2013\oktober\Transfection SILAC exp\PlasmidMinusNormal.xls')
A=A(2:end,1);
%[~,B,~]=xlsread('L:\Elite\LARS\2013\oktober\Transfection SILAC exp\140324_GLYGLY Proteiner normal.xlsx');
[~,B,~]=xlsread('L:\Elite\LARS\2013\oktober\Transfection SILAC exp\140324_GLYGLY Proteiner transfection reagens.xls');
B=B(2:end,2);
comm=intersect(A,B)
inA=setdiff(A,B);
inB=setdiff(B,A);
vennX([size(inA,1)+size(comm,1) size(comm,1) size(inB,1)+size(comm,1)], 0.1)

%% write IDS
fid = fopen('L:\Elite\LARS\2013\oktober\Transfection SILAC exp\PlasmidMinusNormalReagent.csv', 'w');
fprintf(fid,'Gene\n'); 
%fprintf(fid,'%s\n', inB{:}); 
fprintf(fid,'%s\n', inA{:}); 
fclose(fid)
