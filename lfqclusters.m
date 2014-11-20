%% read
%prot = tblread('L:\Elite\LARS\2014\oktober\OLUF HUNT\endelig\results\proteinGroups.txt','\t')
%prot = tblread('L:\Elite\LARS\2014\november\Kamilla\combined\txt\proteinGroups.txt','\t')
prot = xlsread('L:\Elite\kamila\Heart\Sub12LFQ.xlsx')
protlfq=prot(:,[1:36])
%protlfq=prot(:,[117:128])
%protlfq=prot(:,[501:560])
protlfqnan=protlfq(any(~is(protlfq), 2),:) % remove rows with all NaNs
protlfqzero=protlfq(protlfq~=0)

%% cluster analysis
[corrprot cpv]=corrcoef(protlfq,'rows','pairwise')
spy(cpv)
[corrprot cpv]=corrcoef(log2(protlfqnan),'rows','pairwise')
cgprop=clustergram(corrprot, 'Colormap', redgreencmap(256),'ImputeFun','knnimpute')%,'Distance', 'mahalanobis')

%% write correlation matrix

dlmwrite('pairwisecorrcoefnum.csv',corrprot)
dlmwrite('pairwisecorrcoefpvalue.csv',cpv)

%% score distribution
histfit((prot),15,'exponential')
histfit(log2(prot))
xlabel('Score')
ylabel('Count')
title('Score distribution of IDs exclusive to RT120')

