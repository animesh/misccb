%% in silico digestion with trypsin, lys-c and arg-c

name=digestNphosphorylate('Q9GZX7','trypsin',2,8,400,2000);
fprintf('Results written to file %s\n',name);
digestNphosphorylate('Q9GZX7','lysc',2,8,400,2000)
digestNphosphorylate('Q9GZX7','arg-c',2,8,400,2000)

digestNphosphorylate('Q9GZX7','trypsin',2,8,400,1000)
fid = fopen('Q9GZX7.trypsin.2.8.400.1000.txt');
data = textscan(fid, '%d %d %s %f %f %f %f %f','Delimiter','\t');
fclose(fid);
plot(data{7},'r.')
hist(data{7},[1000])
sum(data{7}>=1000)
[idx]=find(data{7}>=1000)
data{7}(idx)
whos data
data{1}

(molweight('VTWFTSWSPCYDCARHVADFLR')+3*1.007276466812+5*MolMass('HPO3'))/3>1000



%% in silico trypsin digestion of UNG2


UNG2 = getgenpept('P13051')
isoelectric(UNG2)
[pI Charge] = isoelectric(UNG2, 'Charge', 7.38)
[UNG2partsPK, UNG2sitesPK, UNG2lengthsPK] = cleave(UNG2, 'trypsin')
%isotopicdist(UNG2partsPK{24})

for i=1:size(UNG2sitesPK,1)
    UNG2MWPI(i,1)=molweight(UNG2partsPK{i});
    UNG2MWPI(i,2)=isoelectric(UNG2partsPK{i});
    %fprintf('%10d\t%10d %s\n',i, UNG2MWPI(i,2),UNG2partsPK{i})
    fprintf('%10d\t%10d\t%10d %s\n',i, UNG2MWPI(i,1),UNG2MWPI(i,2),UNG2partsPK{i})
end

smoothhist2D(UNG2MWPI,5,[100, 100])
plot(UNG2MWPI)
axis equal
plot(UNG2MWPI(:,1),UNG2MWPI(:,2),'r.')
aacount(UNG2.Sequence,'chart','bar')
hist3(UNG2MWPI,[size(UNG2sitesPK,1),size(UNG2sitesPK,1)])

%% isotopic distribution plot of fragments using http://www.mathworks.se/help/bioinfo/ref/isotopicdist.htm

[MD, Info, DF] = isotopicdist(UNG2.Sequence);


fileID = fopen('UNG2Frags.tab.txt','w');
for i=1:size(UNG2sitesPK,1)
    for j=1:UNG2lengthsPK(i)
        [MD, Info, DF] =isotopicdist(UNG2partsPK{i}(j:UNG2lengthsPK(i)), ...
            'nterm','acetyl','nterm','amine','nterm', 'formyl', ... 
            'cterm','amide', 'cterm','free acid', ... 
            'showplot', false)
        fprintf(fileID,'%s\t%10d\t%10d %s\n',[int2str(i),' ',int2str(j)], ...
            Info.MostAbundantMass, Info.MonoisotopicMass, UNG2partsPK{i}(j:UNG2lengthsPK(i)))
    end
end
fclose(fileID);

%% missed cleavage, if # of sites is l => (l+1)*(l+2)/2 total number of fragements

pep='GRRLKYTRLHP'
site='[KR](?!P)'

strfind(pep,'R')	%find overlapping patterns
strfind(pep,'K')	%cannot handle regex?

idx=regexp(pep, site)

[MisspartsPK, MisssitesPK, MisslengthsPK] = cleave(pep , 'trypsin','missedsites',size(idx,2))

%% Acetylation (K N-ter) Pho (S T Y) O-GlcNac (S T) [HR??] GlyGly (K) Oxd-M


for i=1:size(MisssitesPK,1)
    for j=1:MisslengthsPK(i)
        [MD, Info, DF] =isotopicdist(MisspartsPK{i}(j:MisslengthsPK(i)), 'nterm','acetyl','showplot', false);
        fprintf('%s\t%10d\t%s\n',[int2str(i),' ',int2str(j)], Info.MonoisotopicMass, MisspartsPK{i}(j:MisslengthsPK(i)))
    end
end

%% non linear PCA over molecular weights

data = [MisssitesPK;MisslengthsPK];
plot(data)
[c,net]=nlpca(data, 1,  'type','inverse',  'circular','yes' );
nlpca_plot(net)  


