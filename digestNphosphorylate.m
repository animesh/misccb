function funs = digestNphosphorylate(s,e,m,l)
    fprintf('Sequence %s\t Enzyme %s\t Missed Cleavage %d\t Minimum Length %d\n',s,e,m,l);
    em=0.0005485799094;
    pm=1.007276466812;
    ph3m=MolMass('HPO3');
    [parts, sites, lengths] = cleave(getgenpept(s), e,'missedsites',m)
    fileID = fopen([s,'.',e,'.',int2str(m),'.',int2str(l),'.txt'],'w');
    for i=1:size(sites,1)
        if(lengths(i)>=l)
            stypos=regexp(upper(parts{i}),'[STY]');
            fprintf(fileID,'%s\t%s\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\n',[int2str(i),' ',int2str(lengths(i))], ...
                parts{i}, molweight(parts{i}), ... 
                molweight(parts{i})+pm, (molweight(parts{i})+2*pm)/2, (molweight(parts{i})+3*pm)/3, (molweight(parts{i})+4*pm)/4);
            for j=1:size((stypos'),1)
                fprintf(fileID,'%s\t%s\t%6.6f\t%6.6f\t%6.6f\t%6.6f\t%6.6f\n',[int2str(i),' ',int2str(stypos(j))], ...
                    parts{i}(1:stypos(j)), molweight(parts{i})+j*ph3m, ...
                    molweight(parts{i})+j*ph3m+pm, (molweight(parts{i})+j*ph3m+2*pm)/2, (molweight(parts{i})+j*ph3m+3*pm)/3, (molweight(parts{i})+j*ph3m+4*pm)/4);
            end
        end
    end
    fclose(fileID);
end
% functional version using ideas from http://stackoverflow.com/questions/3569933/is-it-possible-to-define-more-than-one-function-per-file-in-matlab
