%% read files
prot=xlsread('X:\Qexactive\Berit_Sissel\StimUnMC6RBR123protrep.xlsx')

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
