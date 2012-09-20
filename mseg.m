%% download and install orbitrap to mzxml convertor

wget http://unfinnigan.googlecode.com/files/Finnigan-0.0206.tar.gz
tar xvzf Finnigan-0.0206.tar.gz
cd Finnigan-0.0206/
cpan
install Data::Hexdumper
install CPAN
reload cpan
install Data::Hexdumper
install  Digest::SHA1
install  YAML
install Getopt::Declare
install Module::Find
install Tie::IxHash
install XML::Generator
perl Makefile.PL
make install
uf-rfi 20070522_NH_Orbi2_HelaEpo_01.RAW
uf-rfi  20070522_NH_Orbi2_HelaEpo_02.RAW
uf-meth 20070522_NH_Orbi2_HelaEpo_01.RAW
uf-mzxml -c 20070522_NH_Orbi2_HelaEpo_01.RAW > d1.mzxml
uf-mzxml -c 20070522_NH_Orbi2_HelaEpo_02.RAW > d12.mzxml

%% msviewer example
load sample_lo_res
msviewer(MZ_lo_res, Y_lo_res)
plot(Y_lo_res(:,2))
hold
plot(Y_lo_res(:,1),'r.')
hold off

%% read from MS xml
out = mzxmlread('d1.mzxml');
out2 = mzxmlread('d12.mzxml');

%% plot m z from MS xml
plot(out.index.offset.id, out.index.offset.value)
m = out.scan(1).peaks.mz(1:2:end);
z = out.scan(1).peaks.mz(2:2:end);
stem(m,z,'marker','none')
out.scan.peaksCount
m2 = out2.scan(1).peaks.mz(1:2:end);
z2 = out2.scan(1).peaks.mz(2:2:end);
stem(m,z,'MarkerFaceColor','g', 'MarkerSize',2, 'MarkerEdgeColor','k')
hold
stem(m2,z2,'MarkerFaceColor','b', 'MarkerSize',2, 'MarkerEdgeColor','k')
hold off
hist(z2)
hist(m2)
axis equal
[f,xi] = ksdensity(z2); 
plot(xi,f); 
hold
[f,xi] = ksdensity(m2); 
plot(xi,f,'r'); 
hold off


%% stem?
x=1:100
y=1:2:200
stem(x,x)
stem(y,x)
stem(x,y)


%% mem test

memory
x(2^30) = 2

