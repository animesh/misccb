%% read
prot=xlsread('L:\Elite\LARS\2014\mai\transfection 3rd paralell\TransFecMCR27logBm2.xlsx');

%% impute

protunan=prot(:,67:75);

for i=1:size(protunan,2)
    i
    meanval(i)=mean(protunan(~isnan(protunan(:,i)),i))
    stdval(i)=std(protunan(~isnan(protunan(:,i)),i))
end

stp=3;
for i=1:stp:size(protunan,2)
    for j=1:stp
        i,j
        meanvalmu(i+j-1)=mean(meanval(i:i+stp-1))
        stdvalmu(i+j-1)=mean(stdval(i:i+stp-1))
    end
end

rng(1234);
ranmat=randn(size(protunan,1),size(protunan,2)).*repmat(stdvalmu,size(protunan,1),1)*0.5+repmat(meanvalmu,size(protunan,1),1)-1.5
protunan(isnan(protunan))=ranmat(isnan(protunan));
hist(protunan)

clear anovaimp;
clear ttimp;
grp={'TF','TF','TF','N','N','N','R','R','R'};
for i=1:size(prot,1)
    anovaknn(i)=anova1(protunan(i,:),grp,'off');
    [x ttknn(i)]=ttest2(protunan(i,1:3),protunan(i,4:6),0.05,'right');
end
hist(anovaknn)
hist(ttknn)
plot(prot(:,7),-log10(anovaknn),'r.')
plot(prot(:,7),-log10(ttknn),'r.')

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
