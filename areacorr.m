%% read

[data,id,~]=xlsread('L:\Qexactive\Linda\MaxLFQall.xlsx');
protab = tblread('c:\users\animeshs\Desktop\MCRshalinitonje.txt','\t')

%% correlate
lfq=data(:,901:1019);
[corrprot cpv]=corrcoef((lfq),'rows','pairwise')
dlmwrite('pairwisecorrcoefnum.csv',corrprot)
dlmwrite('pairwisecorrcoefpvalue.csv',cpv)
cgprop=clustergram(corrprot, 'Colormap', redgreencmap(256),'ImputeFun','knnimpute')%,'Distance', 'mahalanobis')

prot=protab(:,7:60)
corrprot=corrcoef((prot),'rows','pairwise')
corrprot(corrprot == 1) = NaN
[val indx]=max(abs(corrprot))
val=val'
indx=indx'
cgprop=clustergram(corrprot, 'Colormap', redgreencmap(256),'ImputeFun','knnimpute')%,'Distance', 'mahalanobis')
