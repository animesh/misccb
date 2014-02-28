%% read
pep=xlsread('L:\Qexactive\Ani\PepMixWJPT.xlsx');
lfq=pep(:,[16:20]); 
ibaq=pep(:,[21:40]);
int=pep(:,[1:4]);

%% regress
plot(log(mean(int(:,[1:2]),2)),log(mean(int(:,[3:4]),2)),'b.')
log(mean(int(:,[3:4])))\log(mean(int(:,[1:2])))
plot(int(:,[1]),int(:,[2]),'r.')
plot(mean(int(:,[1:2]),2),mean(int(:,[3:4]),2),'b.')

%% cluster analysis

corrpep=corrcoef(int,'rows','pairwise')
ccprop=clustergram(corrpep, 'Colormap', redgreencmap(256),'ImputeFun','knnimpute')%,'Distance', 'mahalanobis')
get(ccprop)
corrprot=corrcoef(int','rows','pairwise')
ccprop=clustergram(corrprot, 'Colormap', redgreencmap(256),'ImputeFun','knnimpute')%,'Distance', 'mahalanobis')

