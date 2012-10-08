%% in silico trypsin digestion of UNG2


ung2 = getgenpept('P13051')
isoelectric(ung2)
[pI Charge] = isoelectric(gpSeq, 'Charge', 7.38)
[UNG2partsPK, UNG2sitesPK, UNG2lengthsPK] = cleave(UNG2, 'trypsin')

for i=1:size(UNG2sitesPK,1)
    %UNG2MWPI(i,1)=molweight(UNG2partsPK{i});
    %UNG2MWPI(i,2)=isoelectric(UNG2partsPK{i});
    %fprintf('%10d\t%10d %s\n',i, UNG2MWPI(i,2),UNG2partsPK{i})
    fprintf('%10d\t%10d\t%10d %s\n',i, UNG2MWPI(i,1),UNG2MWPI(i,2),UNG2partsPK{i})
end

smoothhist2D(UNG2MWPI,5,[100, 100])
plot(UNG2MWPI,'r.')
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
