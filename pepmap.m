%% create and read file 
%awk '{print ">Pep"NR;print $0}' peppho.txt
%http://www.uniprot.org/uniprot/P05198
prot=fastaread('X:\FastaDB\IFA2P05198.fasta')
peps=fastaread('X:\Elite\Aida\SS_1\peppho.fasta')

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

