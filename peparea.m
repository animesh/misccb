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

%% error bar plot

plot(pep(1,:), sum(pep(2:end,:))/length(pep),'ro')
hold
errorbar(pep(1,:),mean(pep(2:end,:)),std(pep(2:end,:)),'<k--');
hold



%% regression
reg=[pep(1,:)' ones(size(pep(1,:)'))]\sum(pep(2:end,:))'
aest=reg(1).*pep(1,:)'+reg(2)
plot(pep(1,:), sum(pep(2:end,:)),'b.')
hold
plot(pep(1,:), aest(:),'r')
hold

%% xrcc hpo3/total
val=csvread('X:\Qexactive\LARS\2013\mars\xrcc1\values.csv',1,1)
a1=[val(1:3,1)./val(1:3,2)]
a2=[val(4:6,1)./val(4:6,2)]
a3=[val(7:9,1)./val(7:9,2)]
a4=[val(12:14,1)./val(12:14,2)]
boxplot([a1 a2 a3 a4])
ttest([a3 a4])
anova1([a1 a2 a3 a4])

%% source
http://stackoverflow.com/questions/9315666/matlab-linear-regression
