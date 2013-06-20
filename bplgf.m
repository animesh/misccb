%% bi-phasic log-sigmoid function

t=100;
b=0;
s=t-b;
f=0.4;
kd1=0.5;
kd2=0.5;
m1=1;
m2=1;
x=0.01:0.001:1;
y=b+(s*f)./(1+10.^(log(kd1-x)*m1))+(s*(1-f))./(1+10.^(log(kd2-x)*m2))
plot(x,y,'r.')


%% source

http://books.google.no/books?id=tIsjh56pI0IC&lpg=PA295&ots=dP_wyva3Q_&dq=log%20sigmoid%20biphasic&pg=PA295#v=onepage&q=log%20sigmoid%20biphasic&f=false