function s = FillScaffoldGaps(S,A)
% Tries to fill gaps in scaffold S with contigs of assembly A.
% 
% Gaps in de S should be represented by the character 'N'



[header, seq] = fastaread(S);

% Split scaffold in contigs, maintain order
r = regexp(seq,'n+','split');
gapstart = regexp(seq,'n+','start');
gapend   = regexp(seq,'n+','end');

fas = struct;

% Take pairs of two contigs and try to connect them with MAIA
% Assign start and end node to be the contigs
for i = 1:length(r)-1
    
    % Get contigs flanking gap
    if i == 1
        c1  = r{i};
        c2  = r{i+1};
    else
        c1  = s{1};
        c2  = r{i+1};
    end
   
    % Take part of scaffold as 'reference genome'
    if i == 1
        ref.Sequence = seq(1:gapstart(2)-1);
    elseif i == length(r)-1
        ref.Sequence = seq(gapend(i-1)+1:end);
    else
        ref.Sequence = seq(gapend(i-1)+1:gapstart(i+1)-1);
    end

    % Write to contigs to disk as assembly
    fas(1).Header    = 'MAIAcontig1';
    fas(2).Header    = 'MAIAcontig2';
    fas(1).Sequence = c1;
    fas(2).Sequence = c2;
    fastawrite('MAIAContig1',fas(1))
    fastawrite('MAIAContig2',fas(2))
    fastawrite('MAIAScaffold.fa',fas)
    AssemblyLoc      = [pwd '/MAIAScaffold.fa'];
    contig_locs      = {[pwd '/'];[pwd '/']}; 
    
    % Write reference chromosome to disk
    ref.Header = 'RefScaf';
    fastawrite('MAIARef.fa',ref)
    RefLoc = [pwd '/MAIARef.fa'];
    
    Assemblies = {'MAIAScaffold', AssemblyLoc, 3; ...
                 'MAIAFillerAssembly',A,2};
    
    Startnode = 'MAIAcontig1';
    Endnode   = 'MAIAcontig2';
    
    % Parameters
    Assemblies_for_refnodes = [1];
    z_weights = [.35 .25 .15 .25];
    clipping_thrs = 10;
    ref_distance = -10;
    ref_quality = 1E-5;
    max_chromosome_dist = 'opt';
    
    [adj_direct contig_direct overlap names assembly_path weigth_direct deltafiles deltafiles_ref] = assembly_driver(Assemblies,RefLoc ,Startnode, Endnode, 'ref_node_assemblies',Assemblies_for_refnodes,'max_chromosome_dist',max_chromosome_dist);
    [ams ass pns] = assemble_paths(assembly_path, contig_direct, Assemblies, deltafiles,deltafiles_ref, contig_locs, RefLoc, overlap);
    [s, f] = call_consensuses(ams,ass, Assemblies);
   

    !rm MAIAContig1
    !rm MAIAContig2
    !rm MAIAScaffold.fa
    !rm MAIARef.fa
    
end
    





