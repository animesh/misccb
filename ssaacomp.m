%% data
[~,~,ss]=xlsread('X:\Elite\Aida\SS_1R\SS1RPGsortMGUS2MMmedvals.xls');
[cu,~,~]=xlsread('L:\Elite\gaute\test\CDS_CU_EntrezID.xls');
od=xlsread('L:\Elite\kamila\proteinGroupsOdenseUGcombo.xlsx');

%% handling NaN's and getting out codon values
gene=cu(:,1)
cu=cu(:,2:end)
cu=cu*10;
cu(~isfinite(cu))=1;

%% cluster 
od(~isfinite(od))=0;
corrcu=corrcoef(od,'rows','pairwise')
corrcu=corrcoef(cu)
plot(corrcu)
%corrprot=corrcoef(prot','rows','pairwise')
ccprop=clustergram(corrcu, 'Colormap', redbluecmap)
get(ccprop)


%% fasta
ff = fastaread('X:\FastaDB\uniprot-human-may-13.fasta');

%% compare
aacount(ff(1).Sequence,'chart','bar')
%hist(cell2mat(ss(:,13)))

aaa(1)=aacount(ff(1).Sequence)
aaa(3)=aacount(ff(1).Sequence).*1.2

aacount(aaa(1),'chart','bar')

hist([aaa(:).A].*[1.2 0.8])

[aaa(:).A].*[1.2 0.8]
aaa(1)*1.5
hist([aaa(:).A],[50])
hist([aaa(:).A].*val,[50])


%% extract Uniprot IDs and their ratios
delimiter = '|'
cnt=0
for i = 1:size(ff,1)
    delim = find(ff(i).Header == delimiter);
    val=cell2mat([ss(find(ismember(ss(:,1),ff(i).Header(delim(1)+1:delim(2)-1))),13)]);
    if isnumeric(val) & val > 0
        cnt=cnt+1;
        valarr(cnt)=val;
        aacarr(cnt)=aacount(ff(i).Sequence);
    end
end

%% compare
aastr='H'
[vm im]=max([aacarr(:).(aastr)].*valarr)
max(valarr(im))
ss(find(isequal(ss(:,13),max(valarr(im)))),1)
sum(valarr))
histfit(valarr)
aastr='V'
hist(([[aacarr(:).(aastr)]' ([aacarr(:).(aastr)].*valarr)']))

hist([aacarr(:).(aastr)].*valarr, bins)
hist([aacarr(:).(aastr)], bins)

bins = linspace(100,1000,10)
y1 = hist([aacarr(:).(aastr)].*valarr, bins);   
y2 = hist([aacarr(:).(aastr)], bins);
bar(bins, [y1;y2]');

%% boxplot
aastr=''
boxplot([[aacarr(:).(aastr)]'  ([aacarr(:).(aastr)].*(valarr))'])

sum([aacarr(:).(aastr)].*(valarr))/sum([aacarr(:).(aastr)])
boxplot([[aacarr(:).(aastr)]'.*sum([aacarr(:).(aastr)].*(valarr))/sum([aacarr(:).(aastr)])  ([aacarr(:).(aastr)].*(valarr))'])

%% loop it
fn=fieldnames(aacarr)
for i = 1:numel(fn)
    aastr=fn{i}
    sum([aacarr(:).(aastr)].*(valarr))/sum([aacarr(:).(aastr)])
end