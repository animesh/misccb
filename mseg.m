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
clear MZ_lo_res Y_lo_res;

%% Read MS mzxml from HeLa grown in SILAC Arg10/Lys8 (Heavy) and without (Light) under EGF for 2 hours, combined and lysed with Trypsin and fractioned in 24 parts using isoelectric focussing (PI based fractions, MS with CID in ion trap)

out = mzxmlread('d1.mzxml');
out2 = mzxmlread('d12.mzxml');

%% heatmaps of MS spectra, log intensity as colors http://www.mathworks.se/help/bioinfo/examples/differential-analysis-of-complex-protein-and-metabolite-mixtures-using-liquid-chromatography-mass-spectrometry-lc-ms.html

[ps,ts] = mzxml2peaks(out,'level',1);
[pg,tg] = mzxml2peaks(out2,'level',1);
[MZs,Ys] = msppresample(ps,5000);
[MZg,Yg] = msppresample(pg,5000);

fh1 = msheatmap(MZs,ts,log(Ys),'resolution',0.15);
title('HeLa, SILAC+EGF, Fraction 1')
fh2 = msheatmap(MZg,tg,log(Yg),'resolution',0.15);
title('HeLa, SILAC+EGF, Fraction 2')

% Picking out specific regions in the MS spectra
ind_ser = samplealign(ts,[2000;4000]);
figure(fh1);
axis([300 600 ind_ser'])
ind_gly = samplealign(tg,[2000;4000]);
figure(fh2);
axis([300 600 ind_gly'])


%% lag between two fractions

whos('Ys','Yg','ts','tg')
plot(1:numel(ts),ts,1:numel(tg),tg)
legend('Frc 1','Frc 2','Location','NorthWest')
title('Time Vectors of the LCMS Data Sets')
xlabel('Spectrum Index')
ylabel('Retention Time (seconds)')


%% stem3 example http://www.mathworks.se/help/bioinfo/examples/visualizing-and-preprocessing-hyphenated-mass-spectrometry-data-sets-for-metabolite-and-protein-peptide-profiling.html

msdotplot(ps,ts,'quantile',.95) % check high intensity peaks
title('5 Percent Overall Most Intense Peaks')

numScans = numel(ps)
basePeakInt = [out.scan.basePeakIntensity]';
peaks_fil = cell(numScans,1);

for i = 1:numScans
    h = ps{i}(:,2) > (basePeakInt(i).*0.99);
    peaks_fil{i} = ps{i}(h,:);
end


peaks_3D = cell(numScans,1);
for i = 1:numScans
peaks_3D{i}(:,[2 3]) = peaks_fil{i};
peaks_3D{i}(:,1) = ts(i);
end
peaks_3D = cell2mat(peaks_3D);

figure(fh2);
stem3(peaks_3D(:,1),peaks_3D(:,2),peaks_3D(:,3),'marker','none')
axis([0 12000 400 1500 0 1e9])
view(60,60)
xlabel('Retention Time (seconds)')
ylabel('Mass/Charge (M/Z)')
zlabel('Relative Ion Intensity')
title('Peaks Above (0.75 x Base Peak Intensity) for Each Scan')


%% test stem3
numScans=size(Ys,1)
numel(MZs)
numScans=numel(MZs)
peaks_3D = cell(numScans,1);
for i = 1:numScans
peaks_3D{i}(:,[2 3]) = MZs{i};
peaks_3D{i}(:,1) = Ys(i);
end
peaks_3D = cell2mat(peaks_3D);
peaks_3D = cell(numScans,1)
for i = 1:numScans
peaks_3D{i}(:,[2 3]) = MZs{i};
peaks_3D{i}(:,1) = Ys(i);
end



%% plot m z from MS xml http://www.mathworks.se/help/bioinfo/ug/features-and-functions.html#bp4mcvy

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


%% 

%% peaks {Retention}(M/Z,Intensity)

[P T]=mzxml2peaks(out)
%msdotplot(P,T, 'Quantile',0.95)
[MZ,Y] = msppresample(P,5000);
msheatmap(MZ,T,log(Y))
msdotplot(P,T)
ksdensity(T)
plot(P{1}(:,:))
plot(P{1}(:,1),P{1}(:,2))
plot(P{2}(:,1),P{2}(:,2))
ksdensity(P{1}(:,1))
ksdensity(P{500}(:,1))
stem(P{500}(:,1),P{500}(:,2),'marker','none')

%% Retension time sampling m/z with threshold

RT=2798
thr=771
ksdensity(P{RT}(P{RT}(:,1)>thr,1))
%plot(P{RT}(P{RT}(:,1)>thr,1))
%stem(P{RT}(P{RT}(:,1)>thr,1),P{RT}(P{RT}(:,1)>thr,2),'marker','none')




%% read fasta file

%HP = fastaread('Homo_sapiens.GRCh37.68.pep.all.fa')
TD = fastaread('Sarcophilus_harrisii.DEVIL7.0.68.pep.all.fa')
aminolookup(HP(1).Sequence)
molweight(HP(1).Sequence)
aacount(HP(1).Sequence,'chart','bar')
isoelectric(HP(2).Sequence)
isoelectric([HP.('Sequence')])
molweight([HP.('Sequence')])
aacount([HP.('Sequence')],'chart','bar')  %leucine rich
aacount([TD.('Sequence')],'chart','bar')    %lucifer ;)


%% isoelectric point and molecular weight (Da-g/Mol)

C={HP.('Sequence')};
molweight(regexprep(cell2mat(C(57:57)),'[UX\*]',''))
molweight(strrep(cell2mat(C(2:2)),'U',''))
x=strrep(strrep(strrep([TD.('Sequence')],'U',''),'X',''),'*','');
%molweight(strrep(strrep(strrep(x,'U',''),'X',''),'*',''))
%ans =  4.2358e+09
isoelectric([C])

G2D=zeros(size(C,2),2);
for i=1:size(C,2)
    G2D(i,1)=molweight(regexprep(cell2mat(C(i)),'[UX\*]',''));
    G2D(i,2)=isoelectric(regexprep(cell2mat(C(i)),'[UX\*]',''));
end
   
%% Tasmanian Devil

CTD={TD.('Sequence')};

G2DTD=zeros(size(CTD,2),2);
for i=1:size(CTD,2)
    G2DTD(i,1)=molweight(regexprep(cell2mat(CTD(i)),'[UX\*]',''));
    G2DTD(i,2)=isoelectric(regexprep(cell2mat(CTD(i)),'[UX\*]',''));
end
   
%% fragment digestion http://www.mathworks.se/help/bioinfo/ref/cleave.html

%[partsPK, sitesPK, lengthsPK] = cleave(gpSeq.Sequence, 'trypsin', ... 
[partsPK, sitesPK, lengthsPK] = cleave(x, 'trypsin', ... 
    'exception', 'KP', ... 
    'missedsites',0);

TDHPMWPI=zeros(size(sitesPK,1),1);


for i=1:size(sitesPK,1)
    %fprintf('%5d%5d%5d %s\n',i, sitesPK(i),lengthsPK(i),partsPK{i})
    %TDHPMWPI(i,1)=molweight(partsPK{i});
    %TDHPMWPI(i,2)=isoelectric(partsPK{i});
    %fprintf('%10d\t%10d %s\n',i, TDHPMWPI(i,2),partsPK{i})
end


scatterhist(TDHPMWPI(:,2),TDHPMWPI(:,1))
hist3(TDHPMWPI,[15 15])

smoothhist2D([TDHPMWPI(:,2),TDHPMWPI(:,1)],200,[100,50])
colorbar
ylabel('Molecular Weight')
xlabel('Isoelectric point')
title('Trypsin digested human proteome')

smoothhist2D(TDHPMWPI,5,[100, 100])
smoothhist2D(TDHPMWPI,5,[100, 100],[],'surf')
ksdensity(TDHPMWPI(:,2)) % checkin the tri/quadri? modal distribution

smoothhist2D([TDHPMWPI(:,2),lengthsPK(:)],5,[100, 100],[],'surf')

DataDensityPlot(TDHPMWPI(:,2),lengthsPK(:),10)

% does not work on this one
cloudPlot(TDHPMWPI(:,2),TDHPMWPI(:,1))
ksdensity(lengthsPK(lengthsPK(:)<20))

sum((lengthsPK(:)==1))

% too slow but looks good
y=[rand(10,10);rand(10,10)]
DataDensityPlot(y(:,2),y(:,1),10)


%% random weighted protein sequence http://www.mathworks.se/help/bioinfo/ref/randseq.html

%rw=randi(20,20,1)'
%RSPW=randseq(length(x)/1000,'alphabet','amino','weights',rw/sum(rw))

RSPW=randseq(length(x)/10,'alphabet','amino','FromStructure',aacount([TD.('Sequence')]));


[RSPWpartsPK, RSPWsitesPK, RSPWlengthsPK] = cleave(RSPW, 'trypsin', ... 
    'exception', 'KP', ... 
    'missedsites',0);

RSPWMWPI=zeros(size(RSPWsitesPK,1),1);


for i=1:size(RSPWsitesPK,1)
    RSPWMWPI(i,1)=molweight(RSPWpartsPK{i});
    RSPWMWPI(i,2)=isoelectric(RSPWpartsPK{i});
    fprintf('%10d\t%10d %s\n',i, RSPWMWPI(i,2),RSPWpartsPK{i})
end

smoothhist2D(RSPWMWPI,5,[100, 100])
smoothhist2D(RSPWMWPI,5,[100, 100],[],'surf')
ksdensity(RSPWMWPI(:,2)) % checkin the tri/quadri? modal distribution




%% random protein sequence, trypsin digestion and PI values

RSP=randseq(length(x)/10,'alphabet','amino');


[RSPpartsPK, RSPsitesPK, RSPlengthsPK] = cleave(RSP, 'trypsin', ... 
    'exception', 'KP', ... 
    'missedsites',0);

RSPMWPI=zeros(size(RSPsitesPK,1),1);


for i=1:size(RSPsitesPK,1)
    RSPMWPI(i,1)=molweight(RSPpartsPK{i});
    RSPMWPI(i,2)=isoelectric(RSPpartsPK{i});
    fprintf('%10d\t%10d %s\n',i, RSPMWPI(i,2),RSPpartsPK{i})
end

smoothhist2D(RSPMWPI,5,[100, 100])
smoothhist2D(RSPMWPI,5,[100, 100],[],'surf')
ksdensity(RSPMWPI(:,2)) % checkin the tri/quadri? modal distribution


%% example for protein seq

gpSeq = getgenpept('AAB39602')
[pI Charge] = isoelectric(gpSeq, 'Charge', 7.38)


gpSeq = getgenpept('NP_002760.1')
isoelectric(gpSeq)

%% stem?

x=1:100
y=1:2:200
stem(x,x)
stem(y,x)
stem(x,y)


%% mem test

memory
x(2^30) = 2


%% plot commands

RSPW=randseq(floor(length(x)/10),'alphabet','amino','FromStructure',aacount([TD.('Sequence')]))
smoothhist2D(RSPWMWPI,5,[100, 100],[],'surf')
smoothhist2D(RSPMWPI,5,[100, 100],[],'surf')
ksdensity(RSPMWPI(:,2))
smoothhist2D(RSPWMWPI,5,[100, 100],[],'surf')
smoothhist2D(RSPMWPI,5,[100, 100])
smoothhist2D(RSPWMWPI,5,[100, 100])
smoothhist2D([TDHPMWPI(:,2),lengthsPK(:)],5,[100, 100],[],'surf')
smoothhist2D([TDHPMWPI(:,2),lengthsPK(:)],5,[100, 100])
smoothhist2D([lengthsPK(:),TDHPMWPI(:,2)],5,[100, 100])
smoothhist2D([lengthsPK(:),TDHPMWPI(:,1)],5,[100, 100])
smoothhist2D([lengthsPK(:),TDHPMWPI(:,2)],5,[100, 100])
smoothhist2D([RSPWlengthsPK(:),RSPWMWPI(:,2)],5,[100, 100])
smoothhist2D([RSPlengthsPK(:),RSPMWPI(:,2)],5,[100, 100])
smoothhist2D([RSPWlengthsPK(:),RSPWMWPI(:,2)],5,[100, 100])
smoothhist2D([lengthsPK(:),TDHPMWPI(:,2)],5,[100, 100])
aacount([TD.('Sequence')],'chart','bar')
aacount([RSPW,'chart','bar')
aacount(RSPW,'chart','bar')
aacount(RSP,'chart','bar')
aacount(HP(1).Sequence,'chart','bar')
cleave('ataahshsgsglshhsshkjsjsrshshajsrhdhd', 'trypsin')
smoothhist2D([RSPWlengthsPK(:),RSPWMWPI(:,2)],5,[100, 100])

