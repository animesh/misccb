%% read

prot=tblread('X:\Elite\Aida\SS_1\4\FRES\txt\proteinGroups.txt','\t')
protrev=tblread('X:\Elite\Aida\SS_1\4\RRES\txt\proteinGroups.txt','\t')
% perl mqpevol.pl /cygdrive/x/Elite/Aida/SS_1/4 2>0 > /cygdrive/x/Elite/Aida/SS_1/4/compFR.txt 
protcomb=tblread('X:\Elite\Aida\SS_1\4\compRF.txt','\t')

%% compare forward and reverse ratios against molecular weights

plot(protrev(:,15),protrev(:,20),'r.')
hold
plot(prot(:,15),1./prot(:,20),'b.')
hold

%% correlation

hist(protcomb(:,2),[100])
hist(protcomb(:,3),[100])
plot(protcomb(:,2),1./protcomb(:,3),'k.')
[rho val]=corrcoef(protcomb(:,2),1./protcomb(:,3),'rows','pairwise')


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

%% correlation plot
corrprot=corr(prot,'rows','pairwise')
HeatMap(corrprot,'Colormap', redgreencmap(256))

%% cluster analysis
corrprot=corrcoef(prot,'rows','pairwise')
clustergram(corrprot,'Colormap',redbluecmap)


%% compare maxquant with proteome discoverer

mqpd=[0.825	0.772	0.774	0.306	0.252	0.302	1.672	1.729	1.779	0.977	0.999	1.023	0.778	0.709	0.788	0.972	0.980	0.928	0.385	0.369	0.383	0.970	0.963	0.998 ;
0.70866	0.71609	0.74699	0.37127	0.3181	0.323	1.2621	1.1789	1.258	0.60449	0.68233	0.84355	0.73261	0.73799	0.76839	0.8078	0.86479	0.83016	0.45036	0.45852	0.49714	0.73496	0.72891	0.71174]


[hyp pval ci stats]=ttest(mqpd(1,:),mqpd(2,:))

plot(mqpd(1,:),mqpd(2,:),'b.')
comm -12 <(sort pd.txt) <(sort mq.txt) | wc
