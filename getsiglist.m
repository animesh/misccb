%% read
d=xlsread('X:\Elite\LARS\2013\April\T Slordahl\Multiconsensus from 3 ReportsAllSC.xlsx')

%% overlay
plot(d(:,12),d(:,14),'r.')
hold
plot(d(:,1),d(:,2),'r-')
hold off

%% distribution
hist(log(d(:,12)),[100])
hist(log(d(:,13)),[100])

plot(mean(d(:,6:8),2),d(:,12))

hist(mean(d(:,6:8),2)-d(:,12))
hist(std(d(:,6:8),0,2)-d(:,13))

std(d(:,6:8),0,2)