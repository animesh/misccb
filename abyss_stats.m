k = [23 25 27 29 31 33];
c = [0 2 3 4 5 6 7 8 10 12];


max_contig_all = zeros(length(k),length(c));
number_of_contigs_all = zeros(length(k),length(c));
number_of_contigs_200_all = zeros(length(k),length(c));
total_contig_size_all = zeros(length(k),length(c));
total_contig_size_200_all = zeros(length(k),length(c));
n50_contigs_all = zeros(length(k),length(c));


for i = 1:length(k)
  for j = 1:length(c)
      
       name = ['contigs-k' num2str(k(i)) '-c' num2str(c(j)) '.fa'];
       
       [status output] = unix(['/Volumes/linux-bastion/perl/assembly_stats.pl ' name]);
       output = sscanf(output, '%d\n%d\n%d\n%d\n%d\n%d');
   
       max_contig_all(i,j) = output(1);
       number_of_contigs_all(i,j) = output(2);
       number_of_contigs_200_all(i,j) = output(3);
       total_contig_size_all(i,j) = output(4);
       total_contig_size_200_all(i,j) = output(5);
       n50_contigs_all(i,j) = output(6);
     
  end
end

   
%% Plot total coverage against N50
figure;plot(n50_contigs_all,total_contig_size_all,'-x')

cvi = 4;
for i = 1:6
    text(n50_contigs_all(i,cvi),total_contig_size_all(i,cvi),num2str(k(i)))
end
set(gca,'FontSize',11)
legend(num2str(c')) 
legendtitle('CovCut')
xlabel('N50')
ylabel('total coverage')

