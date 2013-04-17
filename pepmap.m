%% create and read file 
%awk '{print ">Pep"NR;print $0}' P02768pep.txt
%http://www.uniprot.org/uniprot/P02768
prot=fastaread('X:\Elite\Mohmd\P02768.fasta')
peps=fastaread('X:\Elite\Mohmd\P02768pep.fasta')

%% map
plot([1 length(prot.Sequence)],[1 1],'k-')
ylim([0 size(peps,1)+4])
hold
for i=1:size(peps,1)
    pos(i)=strfind(upper(prot.Sequence),upper(peps(i).Sequence));
    len(i)=length(peps(i).Sequence);
    plot([pos(i) pos(i)+len(i)],[i+2 i+2],'r-')
    plot([pos(i) pos(i)+len(i)],[2 2],'b-')
    for j=pos(i):(pos(i)+len(i)-1)
        id(j)=1;
    end
end
hold
covered=sum(id)/length(prot.Sequence)*100

%% check
%http://blast.ncbi.nlm.nih.gov/Blast.cgi#alnHdr_113576
[i chk]=max(pos)
len(chk),pos(chk),upper(peps(chk).Sequence),prot.Sequence

%% view

plot(pos,len,'r.')
%proteinplot(prot.Sequence)

