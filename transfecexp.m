%% read
prot=xlsread('L:\Elite\LARS\2014\januar\SILAC 2ndparalell\PRN3R2MC3PNRhighB2gnContro2Control.xlsx');
sum(prot(:,5)<0.1&abs(prot(:,6))>0.58)

%% cluster analysis

clustergram(prot(prot(:,5)<0.1&abs(prot(:,6))>0.58,13:30), 'Cluster','column', 'Colormap', redbluecmap,'ImputeFun','knnimpute')
get(ccprop)
corrprot=corrcoef(prot','rows','pairwise')
ccprop=clustergram(corrprot, 'Colormap', redgreencmap(256),'ImputeFun','knnimpute')%,'Distance', 'mahalanobis')


%% venn and save IDs

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
fid = fopen('L:\Elite\LARS\2013\oktober\Transfection SILAC exp\PlasmidMinusNormalReagent.csv', 'w');
fprintf(fid,'Gene\n'); 
%fprintf(fid,'%s\n', inB{:}); 
fprintf(fid,'%s\n', inA{:}); 
fclose(fid)
