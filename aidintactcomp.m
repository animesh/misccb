%% read files
prot=xlsread('X:\Qexactive\Berit_Sissel\B002_T1\Multiconsensus from 4 Comp Reports.xlsx')
peparea=xlsread('X:\Qexactive\Berit_Sissel\B002_T1\MCR2RpepWGparseNamed.xlsx')

%% correlations

corr(prot,'rows','complete')
corr(peparea,'rows','complete')

corr(prot(:,6:9),'rows','complete')
corr([prot(:,10),prot(:,18),prot(:,26),prot(:,32)],'rows','complete')
corr([prot(:,14),prot(:,22),prot(:,30),prot(:,38)],'rows','complete')


%% plot

plot(peparea(:,1),peparea(:,11),'r.')
hold
plot(peparea(:,6),peparea(:,16),'b.')
hold off
