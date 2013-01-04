%% check and connect to neurosky device

instrhwinfo('Bluetooth','MindWave Mobile')
ns = Bluetooth('MindWave Mobile', 1);
fopen(ns)

%% read and plot

scanstr(ns)
readasync(ns)

t=200

for i = 1:t
    check(:,t)=fread(ns);
end

%hold on
%for j = 1:size(check,1)
    plot(check(:,1),'r-')
%end
%hold off


%% close connection

fclose(ns);

%% source http://www.mathworks.se/help/instrument/reading-and-writing-data-over-the-bluetooth-interface.html

