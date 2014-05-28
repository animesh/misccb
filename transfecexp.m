%% read
prot=xlsread('L:\Elite\LARS\2014\januar\SILAC 2ndparalell\PRN3R2MC3PNRhighB2gnContro2Control.xlsx');
prot=xlsread('L:\Elite\LARS\2014\januar\SILAC 2ndparalell\Multiconsensus from 18 Reports.xlsx');
prot=xlsread('L:\Elite\LARS\2014\januar\SILAC 2ndparalell\MCR18GN.xlsx');
prot=xlsread('L:\Elite\LARS\2014\januar\SILAC 2ndparalell\match2table.xlsx');
prot=xlsread('L:\Elite\LARS\2014\mai\transfection 3rd paralell\TransFecMCR27logBm2Imputed.xlsx');
prot=xlsread('L:\Elite\LARS\2014\mai\transfection 3rd paralell\TransFecMCR27logBm2.xlsx');
protunan=knnimpute(protunan);
%protunan(isnan(protunan))=1;
protunan(isnan(protunan))=0;
hist(protunan)

%% impute

protunan=prot(:,65:73);
% mean_ignore_nans(prot(:,1))
% sum(~isnan(prot(:,65)))
% std(prot(~isnan(prot(:,65)),65))
% mean(prot(~isnan(prot(:,65)),65))
for i=1:size(protunan,2)
%impval(i)=mean(prot(~isnan(prot(i,65)),65))+std(prot(~isnan(prot(i,65)),65))*randn()
%protunan(isnan(protunan))=mean(protunan(~isnan(protunan(:,65)),65))+std(protunan(~isnan(protunan(:,65)),65))*randn()
    protunan(isnan(protunan(:,i)),i)=mean(protunan(~isnan(protunan(:,i)),i))-1.8+std(protunan(~isnan(protunan(:,i)),i))*randn()*0.3;
    %protunan(isnan(protunan(:,1)))=mean(protunan(~isnan(protunan(:,1)),1))-2+1/2*std(protunan(~isnan(protunan(:,1)),1))*randn()
end
hist(protunan)

%% test
clear anovaimp;
clear ttimp;
grp={'TF','TF','TF','N','N','N','R','R','R'};
%grp={'TF','TF','TF','TF','TF','TF','TF','TF','TF','N','N','N','N','N','N','N','N','N','R','R','R','R','R','R','R','R','R'};
for i=1:size(prot,1)
    %anova(i)=anova1(protunan(i,9:35),grp,'off');
    anovaknn(i)=anova1(protunan(i,:),grp,'off');
    [x ttknn(i)]=ttest(protunan(i,1:3),protunan(i,4:6));
    %anovamqimp(i)=anova1(prot(:,1:9),grp,'off');
end
%anovaknn=anovaknn';
%ttknn=ttknn';
hist(anovaknn)
hist(ttknn)

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
