function fn = digest(s,e,m,l,dct,tct,cl,ch,mod,msite)
    fn=[s,e,'MC',int2str(m),'L',int2str(l),'MZl',int2str(dct),'MZh',int2str(tct),'CHl',int2str(cl),'CHh',int2str(ch),'Mod',mod,'Site',msite,'.csv'];
    fprintf('Sequence %s\t Enzyme %s\t Missed Cleavage %d\t Minimum Length %d MZ Range %d-%d\t Charge Range %d-%d\t Modification %s-%s',s,e,m,l,dct,tct,cl,ch,mod,msite);
    pm=1.007276466812;
    [parts, sites, lengths] = cleave(getgenpept(s), e,'missedsites',m)
    fileID = fopen(fn,'w');
    fprintf(fileID,'Mass [m/z],Polarity,Start [min],End [min],nCE,CS [z],Comment\n');
    for i=1:size(sites,1)
        [MD, Info, DF] =isotopicdist(parts{i}, 'showplot', false);
        for cz=cl:ch
            if(lengths(i)>=l & ((Info.MonoisotopicMass+cz*pm)/cz > dct) & ((Info.MonoisotopicMass+cz*pm)/cz < tct) & ((tct-((Info.MonoisotopicMass+ch*pm)/ch))>eps(tct)))
                fprintf(fileID,'%6.6f,Positive,,,,%d,%s\n',(Info.MonoisotopicMass+cz*pm)/cz,cz, parts{i});
            end
            if(~isempty(mod) & ~isempty(msite))
                modw=MolMass(mod);
                stypos=regexp(upper(parts{i}),msite);
                for j=1:size((stypos'),1)
                    if(lengths(i)>=l & ((Info.MonoisotopicMass+j*modw+cz*pm)/cz > dct) & ((Info.MonoisotopicMass+j*modw+cz*pm)/cz < tct) & ((tct-((Info.MonoisotopicMass+ch*pm)/ch))>eps(tct)))
                        fprintf(fileID,'%6.6f,Positive,,,,%d,%s\n',(Info.MonoisotopicMass+j*modw+cz*pm)/cz,cz, parts{i}(1:stypos(j)));
                    end
                end
            end
        end
    end
    fclose(fileID);
end

% functional version using ideas from http://stackoverflow.com/questions/3569933/is-it-possible-to-define-more-than-one-function-per-file-in-matlab
