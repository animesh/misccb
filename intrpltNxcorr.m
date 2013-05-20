%% read
d=xlsread('C:\Users\animeshs\SkyDrive\Gotland 125m Landsort Deep 400m.xls')

%% overlay
plot(d(:,6),d(:,7))
hold
plot(d(:,1),d(:,2),'r-')
hold off

%% interpolate and cross correlate

t1min=min([d(:,1)])
t2min=min([d(:,6)])
t3min=min([d(:,3)])
t1max=max([d(:,1)])
t2max=max([d(:,6)])
t3max=max([d(:,3)])

tmin=max([t1min,t2min,t3min])
tmax=min([t1max,t2max,t3max])

d1=d(d(:,1)>=tmin & d(:,1)<= tmax & ~isnan(d(:,2)),2)
i1=d(d(:,1)>=tmin & d(:,1)<= tmax & ~isnan(d(:,2)),1)

d3=d(d(:,3)>=tmin & d(:,3)<= tmax & ~isnan(d(:,2)) & ~isnan(d(:,3)),2)
i3=d(d(:,3)>=tmin & d(:,3)<= tmax & ~isnan(d(:,2)) & ~isnan(d(:,3)),3)

d2=d(d(:,6)>=tmin & d(:,6)<= tmax & ~isnan(d(:,7)),7)
i2=d(d(:,6)>=tmin & d(:,6)<= tmax & ~isnan(d(:,7)),6)


[uAt3, ~, ui] = unique(i3)
n = hist( ui, 1:max(ui) );
sel = n == 1
hist(n)
uAt3 = uAt3(sel, :);
uAd3 = d3(sel, :);



d3i = interp1(uAt3,uAd3,i2,'linear')
plot(d3i,d2,'r.')

[cc lags]=xcorr(d3i,d2,'coeff')
[v i]=max(cc)
i2(lag(i))

savevar=[uAt3,uAd3];
save('interpolatedd3.txt', 'savevar', '-ASCII')

%h=figure;
plot(lags,cc,'r.');
%saveas(h,'test','jpg');


%% compare

plot(i3,d3)
hold
plot(uAt3,uAd3,'r-')
plot(i2,d2,'k-')
hold off


%% source
% http://www.mathworks.se/help/matlab/ref/interp1.html
% http://stackoverflow.com/questions/13883489/remove-all-the-rows-with-same-values-in-matlab
% http://www.mathworks.com/matlabcentral/newsreader/view_thread/248731
% http://www.mathworks.com/matlabcentral/answers/48639
