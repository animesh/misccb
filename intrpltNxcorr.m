%% read
d=xlsread('C:\Users\animeshs\SkyDrive\Gotland 125m Landsort Deep 400m.xls')

%% interpolate and cross correlate
plot(d(:,6),d(:,1))
d1 = interp1(d(:,1),d(:,2),d(:,6),'pchip');
[cc lags]=xcorr(d(:,2),d(:,7),'coeff')
plot(lags,cc,'r.')
hist(lag)
