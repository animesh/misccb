%% check and connect to neurosky device

instrhwinfo('Bluetooth','MindWave Mobile')
ns = Bluetooth('MindWave Mobile', 1);
fopen(ns)

%% read and plot

scanstr(ns)
readasync(ns)

t=100
initLine = plot(nan);
ms=zeros(t,size(fread(ns),1))
for i = 1:t
  disp([num2str(i),'th iteration of ',num2str(t)]);
  m=(fread(ns))
  ms(i,:)=m(:);
  %set(initLine,'YData',m); 
  periodogram(fread(ns),[],'onesided',512)
drawnow               
end
dlmwrite('eegsig.txt', ms, 'delimiter', '\t', 'precision', 4)
     

%% real time play ground

plot(hist(fread(ns)))

[n x]=(hist(fread(ns)))

hist(fread(ns)>0 & fread(ns)<10)

plot(fft(fread(ns)),'r.')

periodogram(fread(ns))

%% close connection

fclose(ns);


%% source 
http://www.mathworks.se/help/instrument/reading-and-writing-data-over-the-bluetooth-interface.html
http://stackoverflow.com/questions/3115833/real-time-plot-in-matlab

