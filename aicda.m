%% download AICDA sequence

AICDA=getgenpept('Q9GZX7')
AICDA.Sequence

%% in silico trypsin digestion of AICDA with Trypsin

[AICDApartsT, AICDAsitesT, AICDAlengthsT] = cleave(AICDA, 'trypsin','missedsites',1)
plot(AICDAlengthsT)
hist(AICDAlengthsT)
size(AICDAlengthsT)


%% add PO3 to S/T/Y and calculate+write molecular weight of fragments to file

po3mm=MolMass('HPO3')


fileID = fopen('AICDAfragsT.tab.txt','w');

for i=1:size(AICDAsitesT,1)
    if(AICDAlengthsT(i)>=8)
        stypos=regexp(upper(AICDApartsT{i}),'[STY]')
        fprintf(fileID,'%s\t%s\t%5.5f\n',[int2str(i),' ',int2str(AICDAlengthsT(i))], ...
            AICDApartsT{i}, molweight(AICDApartsT{i}));
        for j=1:size((stypos'),1)
            fprintf(fileID,'%s\t%s\t%5.5f\n',[int2str(i),' ',int2str(stypos(j))], ...
                AICDApartsT{i}(1:stypos(j)), molweight(AICDApartsT{i})+j*po3mm);
        end
    end
end
fclose(fileID);



