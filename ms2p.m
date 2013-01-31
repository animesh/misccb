%%  monoisotopic mass of fragments

fo='X:\Results\Alexey\peptides.txt';
fw=[fo,'.pepmonoisomass','.txt'];

peph=fopen(fo);
pep=textscan(peph,'%s');
pepw = fopen(fw,'w');

em=0.0005485799094;
pm=1.007276466812;

pepmol=zeros(size(pep{1},1),1);
for i = 1:size(pep{1},1)
    aa=pep{size(pep{1},2)}{i};
    [MD, Info, DF] =isotopicdist(aa, ...
            'showplot', false);
    pepmol(i)=Info.MonoisotopicMass
    fprintf(pepw,'%d\t%d\t%s\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\n',i,size(aa,2), ...
     aa, Info.MonoisotopicMass,  ...
      Info.MonoisotopicMass+pm, (Info.MonoisotopicMass+2*pm)/2, (Info.MonoisotopicMass+3*pm)/3, (Info.MonoisotopicMass+4*pm)/4, molweight(aa));
end

fclose(peph);
fclose(pepw);

%% pep to fas

fo='X:\Results\Alexey\peptides.txt';
fw=[fo,'.fasta'];

peph=fopen(fo);
pep=textscan(peph,'%s');
pepw = fopen(fw,'w');

for i = 1:size(pep{1},1)
    aa=pep{size(pep{1},2)}{i};
    fprintf(pepw,'>PEP %d LEN %d \n%s\n',i, size(aa,2), aa);
end

fclose(peph);
fclose(pepw);

