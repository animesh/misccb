%% motivate by http://grasshoppernetwork.com/showthread.php?tid=629
%% create signal
a=5;
c=1;
m=2;
f=4;
n=10;%no of sample per second?
fs=100*f;% @least twice by nequist
ts=1/fs;
t=0:ts:n/f;
x = c+a*sin(2*pi*f*t)+m*a*sin(2*pi*2*m*f*t)+3*m*a*sin(2*pi*4*m*f*t);
plot(x);
fx=fft(x);
plot(real(fx(1:fs)))
[idx val]=max(real(fx))


%% orig
f=50;
A=5;
Fs=f*100;
Ts=1/Fs;
t=0:Ts:10/f;
x=A*sin(2*pi*f*t);
plot(x),grid on;
F=fft(x);
plot(real(F)),grid on
x1=A*sin(2*pi*(f+50)*t);
x2=A*sin(2*pi*(f+250)*t);
x=x+x1+x2; 
F2=zeros(length(F),1);
F2(10:11)=F(10:11);
xr=ifft(F2);
plot(real(xr)),grid on

%% recreate
clear all;
clc;
close all
f=50;
A=5;
Fs=f*100;
Ts=1/Fs;
t=0:Ts:10/f;
x=A*sin(2*pi*f*t);
x1=A*sin(2*pi*(f+50)*t);
x2=A*sin(2*pi*(f+250)*t);
x=x+x1+x2;
plot(x)
F=fft(x);
figure
N=Fs/length(F);
baxis=(1:N:N*(length(x)/2-1));
plot(baxis,real(F(1:length(F)/2))) 

