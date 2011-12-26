%% INPUT TO BE SPECIFIED TO ASSEMBLY DRIVER SCRIPT

% Which assemblies do you want to align to the reference to connect
% pseudonodes? More will take more time...
Assemblies_for_refnodes = [1 2 3];
 
contig_locs = ...
    {'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs/'; ...
     '/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs/';...
     '/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/contigs/'};


% Define weights, which are used for z-score combination
% The following weights have to be defined:
% z_weights(1) = Contig length
% z_weights(2) = Alignment length
% z_weights(3) = Non aligned overlap
% z_weights(4) = Assembly quality
z_weights = [.25 .25 .25 .25];

% Define a clipping threshold. Alignments that have overhang of more then
% this threshold are not considered
clipping_thrs = 10;

% Specifify the z-score distance that edges towards a 'reference node' will
% get
ref_distance = -10;

% Specify an 'assembly quality' score for the reference nodes
ref_quality = 1E-5;

% Specify a maximum distance (in number of nt's) of two alignments allowed 
% to connect two contigs with a reference node.
max_chromosome_dist = 'opt';
%max_chromosome_dist = 2600;




% Chromosome 1
% Reference chromosome
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr1-gi-144228165-ref-NC_001133.7.fasta';
Startnode = 'chr1_contig_1';
Endnode = 'chr1_contig_233';
%Endnode = 'chr1_contig_227';

 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome1.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

% Do the trick.....
[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);

save chr1.mat


% Chromosome 2
% Reference chromosome
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr2-gi-50593115-ref-NC_001134.7.fasta';
Startnode = 'chr2_contig_454';
Endnode = 'chr2_contig_505';

 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome2.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

 [adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
 save ch2.mat
 
 

% Chromosome 3
% Reference chromosome
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr3-gi-85666111-ref-NC_001135.4.fasta';
Startnode = 'chr3_contig_506';
Endnode = 'chr3_contig_528';

 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome3.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

 [adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
 save chr3.mat


% Chromosome 4
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr4-gi-93117368-ref-NC_001136.8.fasta';
Startnode = 'chr4_contig_530';
Endnode = 'chr4_contig_673';

 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome4.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chr4.mat
 
 
 

% Chromosome 5
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr5-gi-7276232-ref-NC_001137.2.fasta';
Startnode = 'chr5_contig_674';
Endnode = 'chr5_contig_697';

 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome5.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chr5.mat 
 
% Chromosome 6
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr6-gi-42742172-ref-NC_001138.4.fasta';
Startnode = 'chr6_contig_698';
Endnode = 'chr6_contig_715';

Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome6.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chr6.mat  


% Chromosome 7
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr7-gi-162949218-ref-NC_001139.8.fasta';
Startnode = 'chr7_contig_718';
Endnode = 'chr7_contig_744';

Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome7.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chr7.mat  



% Chromosome 8
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr8-gi-82795252-ref-NC_001140.5.fasta';
Startnode = 'chr8_contig_745';
Endnode = 'chr8_contig_782';


Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome8.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chr8.mat  


% Chromosome 9
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr9-gi-6322016-ref-NC_001141.1.fasta';
Startnode = 'chr9_contig_783';
Endnode = 'chr9_contig_801';

 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome9.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chr9.mat  
 
 
% Chromosome 10
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr10-gi-116006492-ref-NC_001142.7.fasta';
Startnode = 'chr10_contig_237';
Endnode = 'chr10_contig_292';


 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome10.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chr10.mat  

% Chromosome 11
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr11-gi-83722562-ref-NC_001143.7.fasta';
Startnode = 'chr11_contig_293';
Endnode = 'chr11_contig_304';

 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome11.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chr11.mat  

% Chromosome 12
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr12-gi-85666119-ref-NC_001144.4.fasta';
Startnode = 'chr12_contig_306';
Endnode = 'chr12_contig_353';

 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome12.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chr12.mat  


% Chromosome 13
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr13-gi-44829554-ref-NC_001145.2.fasta';
Startnode = 'chr13_contig_354';
Endnode = 'chr13_contig_388';


 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome13.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chr13.mat  


% Chromosome 14
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr14-gi-117937805-ref-NC_001146.6.fasta';
Startnode = 'chr14_contig_389';
Endnode = 'chr14_contig_389';

 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome14.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chr14.mat  


% Chromosome 15
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr15-gi-84626310-ref-NC_001147.5.fasta';
Startnode = 'gi_contig_803';
Endnode = 'gi_contig_814';


 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome15.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chr15.mat  



% Chromosome 16
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/chr16-gi-50593503-ref-NC_001148.3.fasta';
Startnode = 'chr16_contig_448';
Endnode = 'chr16_contig_453';


 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosome16.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chr16.mat  



% Chromosome MT
Ref_chrom = '/Domain/tudelft.net/Users/jfnijkamp/yeast_genomes/s288c/contigs/mt-gi-6226515-ref-NC_001224.1.fasta';
Startnode = 'mt_contig_815';
Endnode = 'mt_contig_873';

 Assemblies = ...
    {'s288c' ,'/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/s288c/contigs_per_chromosome/chromosomeMT.contigs.fasta', 2; ...
     'velvet','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/velvet/velvet27-cv4-filter-q21-x2-no-scaffold/contigs.fa',3; ...
     'yjm789','/Domain/tudelft.net/Users/jfnijkamp/data/assembly/maq/yjm789/yjm789_contigs.fasta',1 };

[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
save chrMT.mat  




%%
 


% Do the trick.....
%[adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,Ref_chrom,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);



%% Extend the 5' and 3' ends of the assembly

assembly_path = extend_path(adj_direct,contig_direct,overlap,assembly_path,[],'forward');
assembly_path = extend_path(adj_direct,contig_direct,overlap,assembly_path,[],'backward');


% Split and extend the assembly

% Define the maximum size of a reference node to have in your assembly
max_ref_node_size = 250;
% Split the assembly at thos refnodes, backtrack to a split position and extend
ap_splitted = split_and_extend(adj_direct, assembly_path, contig_direct,overlap, max_ref_node_size)
% 


%% Consensus

% Make alignment matrix from path
%[am as] = assemble_path(assembly_path, contig_direct, Assemblies, deltafiles,deltafiles_ref, contig_locs, Ref_chrom, overlap);
[ams ass] = assemble_paths(ap_splitted, contig_direct, Assemblies, deltafiles,deltafiles_ref, contig_locs, Ref_chrom, overlap);


% Call consensus
[s, f] = call_consensuses(ams,ass, Assemblies);


%% Visualize the graph using Graphviz
numlabs = 1:size(adj_direct,1);                % Give the nodes a numerical label
numlabs = cellstr(num2str(numlabs'));   % in the order they appear in the adjency matrix

%  %draw_assembly_graph(adj,names,numlabs,Startnode,[],'NODE','refnode','ch
%  r11','chr[A-Z]');
%  %draw_assembly_graph(adj_direct,names,numlabs,Startnode,adj_direct,'NODE
%  ','
%  %refnode','chr11','chr[A-Z]');


draw_assembly_graph(adj_direct,names,contig_direct,numlabs,assembly_path(1),adj_direct);
%draw_assembly_graph(adj_direct,names,numlabs,assembly_path(1),adj_direct,'NODE','refnode','gi','chr[A-Z]');