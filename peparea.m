%% read csv

pep=csvread('X:\Results\Sissel\HeK50\peparea.csv');
pepval=csvread('X:\Results\Sissel\HeK50\pep12.csv');

%% peptide area against concentration

plot(pepval(1,:),pepval(2:44382,:),'r.')
pvc=corr(pepval);
plot(pvc)
plot(pepval)

%% pep raw

boxplot(pep)
plot(pep(1,:),pep(:,:),'r.');
corrcoef(sum(pep),pep(1,:)),
plot(sum(pep(2:end,:)),pep(1,:),'b.')
