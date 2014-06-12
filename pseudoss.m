%% read
prot=xlsread('L:\Elite\LARS\2014\juni\Pseudomonas\PesudoSproteinGroups.xls');
prot=prot(:,5:8);

%% check
histfit(prot(~isnan(log2(prot))))

