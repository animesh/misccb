cd D:\animesh\projects\research\mohan\res\tox
data = load('tox_data.txt');
plot(data)
plot(data(1:4,2),data(1:4,3))
hold
Current plot held
plot(data(5:8,2),data(5:8,3),'r')
plot(data(9:12,2),data(9:12,3),'g')
plot(data(9:12,2),data(9:12,6),'b')
wt=data(:,3)./data(:,3)
m347=data(:,4)./data(:,3)
m356=data(:,5)./data(:,3)
dm=data(:,6)./data(:,3)
hold
plot(dm)
plot(m356)
plot(m347)
plot(wt)


tox<-read.table('toxdr.txt')
boxplot(tox_percent)
tox_percent=tox/tox$WT*100
boxplot(tox_percent)

p = tox_percent$WT
q = tox_percent$WT_347
r = tox_percent$WT_356
s = tox_percent$WT_347_356
scores = data.frame(p,q,r,s)
boxplot(scores)
scores = stack(scores)
names(scores)
oneway.test(values ~ ind, data=scores, var.equal=T)
