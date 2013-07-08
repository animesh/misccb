%% read files

d=xlsread('X:\Qexactive\Ingvild\130629\MC6RSIMOD.xlsx')

E24IBS=xlsread('X:\Qexactive\Ingvild\130624\Excl_130624_Ingvild_BX_SV.xlsx')
E24IM=xlsread('X:\Qexactive\Ingvild\130624\Excl_130624_Ingvild_med.xlsx')
E24IS=xlsread('X:\Qexactive\Ingvild\130624\Excl_130624_Ingvild_SV.xlsx')
I24QPS=xlsread('X:\Qexactive\Ingvild\130624\Incl_130624_Q13501_PO4_S403_Searchlist for Qex.xlsx')


%% Ingvild stuff

hist(d(:,9))
hist(d(:,4))
hist(I24QPS(:,1))

%% SIM stuff

a=xlsread('X:\Elite\Alexey\Test_Incl_Excl\Multiconsensus from 3 ReportsSImod.xlsx');
a=xlsread('X:\Elite\Alexey\HCD\130617_1SegGenMSSI.xlsx')
EL=xlsread('X:\Elite\Alexey\Test_Incl_Excl\List.xlsx');
ELMI=xlsread('X:\Elite\Alexey\Test_Incl_Excl\peptides.txt.pepmonoisomass.xlsx');

a=xlsread('X:\Qexactive\Alexey\Multiconsensus from 4 ReportsQexSImod.xlsx');

%% extract vals and compare

mzt=1;
mze=EL(:,1);
cntax=0;
ppm=10;
detect=zeros(size(mze,1),size(a,1));
for i=1:size(mze,1)
    for j=1:size(a,1)
        %if(a(j,4)<=(mze(i)+mze(i)*(ppm/10e6)) && a(j,4)>=(mze(i)-mze(i)*(ppm/10e6)))
        if(((abs(a(j,9)-mze(i))<=mze(i)*(ppm/10e6))||(abs(a(j,9)-mze(i))<=a(j,9)*(ppm/10e6))) && a(j,14)==mzt)
        %if((a(j,9)>(mze(i)+mze(i)*(ppm/10e6)) || a(j,9)<(mze(i)-mze(i)*(ppm/10e6))) && a(j,14)==mzt)
           cntax=cntax+1;
           detect(i,j)=mze(i)-a(j,9);
        end
    end
end

cntax
spy(detect)
[rd,cd]=find(detect>0);
chix=1
rd(chix)
cd(chix)
detect(rd(chix),cd(chix))
unique(rd);
size(ans,1)

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

