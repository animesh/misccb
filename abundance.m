hist(score)
hist(score,[100])
hist(score(score(:,1)>0),[100])
hist(score(log(score(:,1)>0)),[100])
hist(log(score(score(:,1)>0)),[100])
hist(mw(mw(:,1)<500),[40])
hist(mw(mw(:,1)<500),[50])
hist(mw(mw(:,1)<500),[200])
hist(mw(mw(:,1)<500),[100])
xlabel('Molecular Weight')
ylabel('# of proteins')
hist(log10(abd),[100])
