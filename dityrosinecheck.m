%% read file
band4=xlsread('L:\MALDI\pso\141203_hclhydrolysis_herb3_band4.xlsx');
band3=xlsread('L:\MALDI\pso\141203_psoextract_band3.xlsx');
band2=xlsread('L:\MALDI\pso\141203_psoextract_band2.xlsx');
band1=xlsread('L:\MALDI\pso\141203_psoextract_band1.xlsx');

%% extract area and mass
maxarea=max([band1(:,7);band2(:,7);band3(:,7);band4(:,7)])+1;
minarea=min([band1(:,7);band2(:,7);band3(:,7);band4(:,7)])-1;
massmat=[band1(:,12);band2(:,12);band3(:,12);band4(:,12)];
maxmass=max(massmat)+1;
minmass=min(massmat)-1;
%% database creation
s='P29508';
%upper(seq.Sequence(74:349))
%upper(seq.Sequence(301:331))
%upper(seq.Sequence)
l=length(seq.Sequence)
e='trypsin';
m=2;
msite='Y';
fn=[s,'.',e,'.',int2str(m),'.',int2str(l),'.',int2str(minmass),'.',int2str(maxmass),'.txt'];
fileID = fopen(fn,'w');
seq=getgenpept(s);
[parts, sites, lengths] = cleave(seq, e,'missedsites',m);
stypos=regexp(upper(parts),msite);
pm=1.007276466812;
val=0;
for i=1:size(sites,1)
    for j=1:size(sites,1)
        if(i<j)
            [MD, Infoi, DF] =isotopicdist(parts{i},'showplot', false);
            [MD, Infoj, DF] =isotopicdist(parts{j},'showplot', false);
            styposi=regexp(upper(parts{i}),msite);
            styposj=regexp(upper(parts{j}),msite);
            for k=1:size((styposi'),1)
                for l=1:size((styposj'),1)
                    combmass=Infoi.MonoisotopicMass+Infoj.MonoisotopicMass+pm;
                    for cnt=1:size(massmat,1)
                        %if(combmass>minmass&combmass<maxmass)
                        if(abs(combmass-massmat(cnt))<pm)
                            val=val+1;
                            cma(val)=combmass;
                            fprintf(fileID,'%s\t%s\t%d\t%d\t%d\n',upper(parts{i}),upper(parts{j}),combmass,massmat(cnt),cnt);
                        end
                    end
                end
            end
        end
    end
end
fclose(fileID);
