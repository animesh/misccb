%% data
[~,~,ss]=xlsread('X:\Elite\Aida\SS_1R\SS1RPGsortMGUS2MMmedvals.xls');

%% fasta
ff = fastaread('X:\FastaDB\uniprot-human-may-13.fasta');

%% compare
aacount(ff(1).Sequence,'chart'x,'bar')
hist(cell2mat(ss(:,13)))

%% extract Uniprot IDs and their ratios
delimiter = '|'
for i = 1:size(ff,1)
    delim = find(ff(i).Header == delimiter)
    val=cell2mat([ss(find(ismember(ss(:,1),ff(i).Header(delim(1)+1:delim(2)-1))),13)])
end
