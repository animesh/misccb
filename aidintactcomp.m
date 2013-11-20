%% read files
prot=xlsread('X:\Qexactive\Berit_Sissel\MCR22Proteins.xls')
prot=prot(:,[28,36,44,52,60,68,76,84,92,100,108,116,124,132,140,148,156,164,172,180,188,196])
prot=prot(:,6:27)


%% scores and areas
scorea=xlsread('X:\Qexactive\Berit_Sissel\B005\SMAcomp.xlsx')
corr(scorea,'rows','pairwise')

%% tobias
ratio=xlsread('X:\Results\TS\MeanRatiosT0t5t12withTriplicateMedianValsNames.xls')
corrprot=corrcoef(ratio(:,[1:3]),'rows','pairwise')
ccprop=clustergram(corrprot, 'Colormap', redgreencmap(256),'ImputeFun','knnimpute')%,'Distance', 'mahalanobis')
spy(corrprot)

%% comp
[pcom, z, dev] = pca1(prot)
cumsum(dev./sum(dev) * 100)
plot3(pcom(:,1),pcom(:,2),pcom(:,3),'r.')
tags = num2str((1:size(pcom,1))','%d');
text(pcom(:,1),pcom(:,2),pcom(:,3),tags)
xlabel('PC1');
ylabel('PC2');
zlabel('PC3');
title('PCA Scatter');

%%cluster

corrprot=corrcoef(prot','rows','pairwise')
ccprop=clustergram(corrprot, 'Colormap', redgreencmap(256),'ImputeFun','knnimpute')%,'Distance', 'mahalanobis')


%% correlations

corr(prot,'rows','complete')
%corr(peparea,'rows','complete')

corr(prot(:,6:11),'rows','complete')

corr([prot(:,12),prot(:,20),prot(:,28),prot(:,36),prot(:,44),prot(:,52)],'rows','complete')
corr([prot(:,16),prot(:,24),prot(:,32),prot(:,40),prot(:,48),prot(:,56)],'rows','complete')

%% hist

hist(prot(:,6:11))


%% plot

plot(peparea(:,1),peparea(:,11),'r.')
hold
plot(peparea(:,6),peparea(:,16),'b.')
hold off
