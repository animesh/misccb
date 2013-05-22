%% read
d=xlsread('X:\Elite\LARS\2013\April\T Slordahl\Multiconsensus from 3 ReportsMHSC.xlsx')
p=xlsread('X:\Elite\LARS\2013\mars\tobias\Multiconsensus from 3 Reports 0t MHSC.xlsx')

%% overlay
plot(d(:,12),d(:,14),'r.')
hold
plot(d(:,1),d(:,2),'r-')
hold off

%% distribution
hist(log(d(:,12))/log(10),[100])
hist(log(d(:,13)),[100])

plot(mean(d(:,6:8),2),d(:,12))

hist(mean(d(:,6:8),2)-d(:,12))
hist(std(d(:,6:8),0,2)-d(:,13))

std(d(:,6:8),0,2)


sum(log(d(:,12))/log(10)>0.4)

dl10=log(d(:,12))/log(10)

mean(d(~isnan(d(:,12)),12))

sum(normcdf(-abs(d(:,12)),mean(d(~isnan(d(:,12)),12)),std(d(~isnan(d(:,12)),12)))<0.05)

plot(normcdf(-abs(d(:,12)),mean(d(~isnan(d(:,12)),12)),std(d(~isnan(d(:,12)),12))))

pm=mean([p(:,9),p(:,12),p(:,15)],2)
ps=std([p(:,9),p(:,12),p(:,15)],0,2)

hist(ps)
hist(log(pm)/log(10),[100])



%% source
% http://www.mathworks.com/matlabcentral/newsreader/view_thread/298645