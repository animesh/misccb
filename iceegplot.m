%% read data
d=load('Sample.mat')
[r c]=size(d.d)
plot(d.d(r,:))

%% check
plot(d.d(1:50,:))
autocorr(d.d(r,:)')

%% symbolic test

% source http://blogs.mathworks.com/loren/2012/07/27/using-symbolic-equations-and-symbolic-functions-in-matlab/?s_eid=PSM_1986

besselODE = 't^2*D2y+t*Dy+(t^2-n^2)*y';
f = dsolve(besselODE,'y(1)=1','y(2)=n','t');
pretty(f)

