%% read
prot=xlsread('X:\Elite\Aida\SS_Result\MQcombo75.xlsx')
prot=prot(:,[2:74,76]); % remove U266 replicate which did not work

%% cluster analysis

corrprot=corrcoef(prot,'rows','pairwise')
corrprot=corrcoef(prot','rows','pairwise')
ccprop=clustergram(corrprot, 'Colormap', redgreencmap(256),'ImputeFun','knnimpute')%,'Distance', 'mahalanobis')
get(ccprop)

%% subgroup analysis
protsg=prot(:,[61:63,22:24,34:36,10:12,19:21]);
corrprot=corr(protsg,'rows','pairwise')
ccprop=clustergram(corrprot, 'Colormap', redgreencmap(256),'ImputeFun','knnimpute');
dpst = linkage(ccprop, 'ward');

csize=2;
dpsg = pdist(protsg', 'euclid');
dpst = linkage(dpsg, 'ward');
getid = cluster(dpst, 'maxclust',csize);

%% comp

[pcom, z, dev] = pca(prot)
cumsum(dev./sum(dev) * 100)
plot(pcom(:,1),pcom(:,2),'r.')
tags = num2str((1:size(pcom,1))','%d');
text(pcom(:,1),pcom(:,2),tags)
xlabel('PC1');
ylabel('PC2');
title('PCA Scatter');

%% tags

cd = clusterdata(pcom(:,1:2),4);
gscatter(pcom(:,1),pcom(:,2),cd)
gname('name')


>>>>>>> 5307f70af61f5ac82ab8a44b805cae431deb83da


%% plot MM with MGUS

corr(log10(prot(:,1)),log10(prot(:,2)),'rows','pairwise')
plot(log10(prot(:,1)),log10(prot(:,2)),'b.')

%% find significant diffs

mavolcanoplot(prot(:,1), prot(:,2), mattest(prot(:,1), prot(:,2)),'LogTrans','True')
mavolcanoplot(proto(:,1), proto(:,2), mattest(proto(:,1), proto(:,2)),'LogTrans','True')



%% check rand vals

proto=randn(1000,1).*(2*pi)
proto=[proto  2*proto -1*proto sin(proto) cos(proto) sin(proto).*cos(proto)]


<<<<<<< HEAD
%% cluster analysis

corrprot=corrcoef(prot,'rows','pairwise')
corrprot=corrcoef(prot','rows','pairwise')
ccprop=clustergram(corrprot, 'Colormap', redgreencmap(256)) %,'ImputeFun',@('distance', 'mahalanobis')knnimpute)%,'Distance', 'mahalanobis')
get(ccprop)
=======
>>>>>>> 5307f70af61f5ac82ab8a44b805cae431deb83da

%% correlation plot
corrprot=corr(prot,'rows','pairwise')
HeatMap(corrprot,'Colormap', redgreencmap(256))


%% compare forward and reverse ratios against molecular weights

plot(protrev(:,15),protrev(:,20),'r.')
hold
plot(prot(:,15),1./prot(:,20),'b.')
hold

%% correlation

hist(protcomb(:,2),[100])
hist(1./protcomb(:,3),[100])
plot(protcomb(:,2),1./protcomb(:,3),'k.')
[rho val]=corrcoef(protcomb(:,2),1./protcomb(:,3),'rows','pairwise')
hist(protcomb(:,2)-1./protcomb(:,3),[100])


%% outliers

X = 1:1000; % Pseudo Time
Y = 5000 + randn(1000, 1); % Pseudo Data
Outliers = randi(1000, 10, 1); % Index of Outliers
Y(Outliers) = Y(Outliers) + randi(1000, 10, 1); % Pseudo Outliers
[YY,I,Y0,LB,UB] = hampel(X,Y);

plot(X, Y, 'b.'); hold on; % Original Data
plot(X, YY, 'r'); % Hampel Filtered Data
plot(X, Y0, 'b--'); % Nominal Data
plot(X, LB, 'r--'); % Lower Bounds on Hampel Filter
plot(X, UB, 'r--'); % Upper Bounds on Hampel Filter
plot(X(I), Y(I), 'ks'); % Identified Outlie



%% compare maxquant with proteome discoverer

mqpd=[0.825	0.772	0.774	0.306	0.252	0.302	1.672	1.729	1.779	0.977	0.999	1.023	0.778	0.709	0.788	0.972	0.980	0.928	0.385	0.369	0.383	0.970	0.963	0.998 ;
0.70866	0.71609	0.74699	0.37127	0.3181	0.323	1.2621	1.1789	1.258	0.60449	0.68233	0.84355	0.73261	0.73799	0.76839	0.8078	0.86479	0.83016	0.45036	0.45852	0.49714	0.73496	0.72891	0.71174]


[hyp pval ci stats]=ttest(mqpd(1,:),mqpd(2,:))

plot(mqpd(1,:),mqpd(2,:),'b.')
comm -12 <(sort pd.txt) <(sort mq.txt) | wc

<<<<<<< HEAD

%% fit dist

pd = fitdist(kam,'Normal')
pd = fitdist(kam,'Kernel','Kernel','epanechnikov')
x_values = 0:0.01:50;
pdf = pdf(pd,x_values);
plot(x_values,pdf,'LineWidth',2)
=======
>>>>>>> 5307f70af61f5ac82ab8a44b805cae431deb83da
