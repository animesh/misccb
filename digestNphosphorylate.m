function fn = digestNphosphorylate(s,e,m,l,cl,ch, dct,tct)
    fn=[s,e,int2str(m),int2str(l),int2str(dct),int2str(tct),'.csv'];
    fprintf('Sequence %s\t Enzyme %s\t Missed Cleavage %d\t Minimum Length %d 2+Threshold %d\t 3+Threshold %d\n',s,e,m,l,dct,tct);
    pm=1.007276466812;
    ph3m=MolMass('HPO3');
    [parts, sites, lengths] = cleave(getgenpept(s), e,'missedsites',m)
    fileID = fopen(fn,'w');
    fprintf(fileID,'Mass [m/z],Polarity,Start [min],End [min],nCE,CS [z],Comment\n');
    for i=1:size(sites,1)
        [MD, Info, DF] =isotopicdist(parts{i}, 'showplot', false);
        if(lengths(i)>=l & (((Info.MonoisotopicMass+2*pm)/2) > dct) & ((tct-((Info.MonoisotopicMass+3*pm)/3))>eps(tct)))
            fprintf(fileID,'%6.6f,Positive,,,,%d,%s\n',Info.MonoisotopicMass+pm,1, parts{i});
            fprintf(fileID,'%6.6f,Positive,,,,%d,%s\n',(Info.MonoisotopicMass+2*pm)/2, 2, parts{i});
            fprintf(fileID,'%6.6f,Positive,,,,%d,%s\n',(Info.MonoisotopicMass+3*pm)/3, 3, parts{i});
            fprintf(fileID,'%6.6f,Positive,,,,%d,%s\n',(Info.MonoisotopicMass+4*pm)/4, 4, parts{i});
        end
    end
    fclose(fileID);
end
% functional version using ideas from http://stackoverflow.com/questions/3569933/is-it-possible-to-define-more-than-one-function-per-file-in-matlab
