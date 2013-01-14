%MG132_3 = mzxmlread('M:\RAW\Lars\130107_MG132_3_HCD.mzXML');
mzxmlread('M:\RAW\Lars\t1.mzXML')

mzxmlread('results.mzxml')

pep=load 'X:\Results\Alexey\peptides.txt' --ascii

%%  peptides to ions

fo='X:\Results\Alexey\peptides.txt';
fw=[fo,'.pepmass','.txt'];

peph=fopen(fo);
pep=textscan(peph,'%s');
pepw = fopen(fw,'w');

em=0.0005485799094;
pm=1.007276466812;

pepmol=zeros(size(pep{1},1),1);
for i = 1:size(pep{1},1)
    aa=pep{size(pep{1},2)}{i};
    pepmol(i)=molweight(aa)
    fprintf(pepw,'%d\t%d\t%s\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\n',i,size(aa,2), ...
     aa, molweight(aa), ...
      molweight(aa)+pm, (molweight(aa)+2*pm)/2, (molweight(aa)+3*pm)/3, (molweight(aa)+4*pm)/4);
end

fclose(peph);
fclose(pepw);


