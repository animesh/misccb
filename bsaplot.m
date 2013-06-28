%% read files

prot=xlsread('X:\BSA Direct vs. Trad\MC8RProtein.xlsx')
pep=xlsread('X:\BSA Direct vs. Trad\MC8RPeptides.xlsx')
si=xlsread('X:\BSA Direct vs. Trad\MC8RSI.xlsx')


%% BSA detection

hist(si(si(:,1)==1,14),[100])

corr(si(si(:,14)==11,9),si(si(:,14)==1,9))

%% protein

hist(log(prot(prot(:,1)>0&prot(:,6)>0,6))) % direct 16
hist(log(prot(prot(:,1)>0&prot(:,22)>0,22))) % trad 16

hist([log(prot(prot(:,1)>0&prot(:,22)>0,22));log(prot(prot(:,1)>0&prot(:,6)>0,6))],100) % trad 16


figure; 
cc=hsv(8);
hold on;
sp=9;
for i=1:8
    (i-1)*4+sp
    [val freq]=ksdensity(log(prot(prot(:,1)>0&prot(:,(i-1)*4+sp)>0,(i-1)*4+sp))); % trad 16
    plot(freq,val,'color',cc(i,:));
    cc(i,:)
end
hold off;

ksdensity(log(prot(prot(:,1)>0&prot(:,6)>0,6))) % direct 16
hold
ksdensity(log(prot(prot(:,1)>0&prot(:,22)>0,22))) % direct 16

[rd,cd]=find(detect>0);
unique(rd);
size(ans,1)
chix=1
rd(chix)
cd(chix)
detect(rd(chix),cd(chix))


%% compare R1 with R1EX1

mze1=EL1(:,1)
rt1e1=EL1(:,3)
rt2e1=EL1(:,4)
ce1=EL1(:,6)

mze2=EL2(:,1)
rt1e2=EL2(:,3)
rt2e2=EL2(:,4)
ce2=EL2(:,6)

mze3=EL3(:,1)
rt1e3=EL3(:,3)
rt2e3=EL3(:,4)
ce3=EL3(:,6)

plot(mze1,ce1,'r.')

mz=d(:,9)
rt=d(:,12)
c=d(:,11)

mze=[mze1;mze2;mze3]

%% compare
ppm=10
cnt=0;
for i=1:size(mze1,1)
    for j=1:size(mz,1)
        if(mz(j)<=(mze1(i)+mze1(i)*(ppm/10e6)) && mz(j)>=(mze1(i)-mze1(i)*(ppm/10e6)) && rt(j)>=rt1e1(i) && rt(j)<=rt2e1(i) && (d(j,14)==10))
           cnt=cnt+1; 
        end
    end
end

cnt2=0;
for i=1:size(mze2,1)
    for j=1:size(mz,1)
        if(mz(j)==mze2(i) && rt(j)>=rt1e2(i) && rt(j)<=rt2e2(i))
           cnt2=cnt2+1; 
        end
    end
end

cnt3=0;
for i=1:size(mze3,1)
    for j=1:size(mz,1)
        if(mz(j)==mze3(i) && rt(j)>=rt1e3(i) && rt(j)<=rt2e3(i))
           cnt=cnt3+1; 
        end
    end
end

%% replicate compare

e1=d(d(:,14)==10,9)
e2=d(d(:,14)==20,9)
e3=d(d(:,14)==30,9)

[r,lags]=xcorr(e1,e2)
max(r)
plot(lags,r,'r.')

ecnt=0;
ppm=10
for i=1:size(e2,1)
    for j=1:size(e3,1)
        if(abs(e2(i)-e3(j))<=(e3(j)*ppm/10e6) || abs(e2(i)-e3(j))<=(e2(i)*ppm/10e6))
           ecnt=ecnt+1; 
        end
    end
end

%% source

http://stackoverflow.com/questions/2028818/automatically-plot-different-colored-lines-in-matlab
