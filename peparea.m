%% read csv

pep=csvread('X:\Results\Sissel\HeK50\sequest\peparea.csv');
pep=csvread('X:\Results\Sissel\HeK50\mascot\peparea.csv');
%pepval=csvread('X:\Results\Sissel\HeK50\sequest\pep12.csv');

%% peptide area against concentration

plot(pepval(1,:),pepval(2:44382,:),'r.')
pvc=corr(pepval);
plot(pvc)
plot(pepval)

%% pep raw

boxplot(pep)
stem(pep(2:end,:))
plot(pep(1,:),pep(:,:),'r.');
corrcoef(sum(pep),pep(1,:)),
plot(sum(pep(2:end,:)),pep(1,:),'b.')

%% regression
reg=[pep(1,:)' ones(size(pep(1,:)'))]\sum(pep(2:end,:))'
aest=reg(1).*pep(1,:)'+reg(2)
plot(pep(1,:), sum(pep(2:end,:)),'b.')
hold
plot(pep(1,:), aest(:),'r')
hold

%% source
http://stackoverflow.com/questions/9315666/matlab-linear-regression
