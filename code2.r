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


tox1<-read.table('toxdr.txt')
tox_percent1=tox1/tox1$WT*100
boxplot(tox_percent1$WT,tox_percent1$WT_347)

tox2<-read.table('res244219_wnc.txt')
tox_percent2=tox2/tox2$WT*100
boxplot(tox_percent2)

p = tox_percent$WT
q = tox_percent$WT_347
r = tox_percent$WT_356
s = tox_percent$WT_347_356
scores = data.frame(p,q,r,s)
boxplot(scores)
scores = stack(scores)
names(scores)
oneway.test(values ~ ind, data=scores, var.equal=T)


  Natural language support but running

R is a collaborative project with many
Type 'contributors()' for more informat
'citation()' on how to cite R or R pack

Type 'demo()' for some demos, 'help()'
'help.start()' for an HTML browser inte
Type 'q()' to quit R.

tox1<-read.table('toxdr_n.txt')
tox_percent1=tox1/tox1$WT*100
boxplot(tox_percent1)
tox2<-read.table('res244219_wnc_nn.txt')
tox_percent2=tox2/tox2$WT*100
boxplot(tox_percent2)
a=c(tox_percent2,tox_percent1)
b=a[-5]
boxplot(b)


tox1<-read.table('toxdr_n.txt')
tox_percent1=tox1/tox1$WT*100
boxplot(tox_percent1)
tox2<-read.table('res244219_wnc_n.txt')
tox_percent2=tox2/tox2$WT*100
boxplot(tox_percent2)
a=c(tox_percent2,tox_percent1)
b=a[-6]
boxplot(b)
