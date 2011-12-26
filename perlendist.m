%% load file
a=load('res.tab');
%% density plot
[uniqueC,~,idx] = unique(a(:,2));
counts = accumarray(idx(:),1,[],@sum); 
smoothhist2d([uniqueC(:),counts(:)],50,[50,50])
colorbar
xlabel('Percent Identity')
ylabel('Number of Match')
title('Solexa reads mapped to Celera assembly')
%% length Vs percent
smoothhist2d([a(:,2)],a(:,1)],50,[50,50])
colorbar
xlabel('Percent Identity')
ylabel('Length of Match')
title('Solexa reads mapped to Celera assembly')
