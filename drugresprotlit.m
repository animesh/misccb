%% get pubmed record for key words

%DRL=getpubmed('drug+AND+resistance+AND+proteomics','NUMBEROFRECORDS',1000)

DRL=getpubmed('drug+effect','NUMBEROFRECORDS',200)

%% do frequency count

word = regexp(lower([DRL(:).Abstract]),' ','split')';
[val,idxW, idxV] = unique(word);
num = accumarray(idxV,1);
[num idxW] 

[counts bins]=hist(num.*idxW)
plot(bins, counts)

ksdensity(num.*idxW)


%% source

http://www.mathworks.com/matlabcentral/answers/39759
http://www.mathworks.se/help/bioinfo/ug/creating-get-functions.html
http://stackoverflow.com/questions/2597743/matlab-frequency-distribution
