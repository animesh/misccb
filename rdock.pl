#!/usr/bin/perl -w

#change following lines if you are using files in a different position.
$create_lig_c_program="./create_lig";
$ace_c_program="./deltaG";

#change following lines if you are using a different CHARMm version
$bin_file_1="./amino.rtf";
$bin_file_2="./param.prm";
$charmm_read_parameter_file_command="CARD NAME";

if($#ARGV<0){
    print STDERR "\n";
    print STDERR "[USAGE:]\n\n";
    print STDERR "      $0 -i zdock_output_file [-o rdock_output_file] [-x from to] [-d output_directory] [-keep]\n\n";
    print STDERR "      This program performs RDOCK on the specified zdock output file. Output directory is optional.\n";
    print STDERR "      Paralleization of the program can be achieved by running multiple copies of the program simultaneously.\n";
    print STDERR "      Use different \"-x from to\" in each copy to cover all rows of the zdock output file. \n";
    print STDERR "      -x: To run the program on specified rows in the zdock output file.\n";
    print STDERR "         (default setting: -x 1 1)\n";
    print STDERR "      -d: To print result to specified directory.\n";
    print STDERR "         (defualt setting: current directory)\n";
    print STDERR "      -keep: To keep intermediat results of both minimization steps.\n";
    print STDERR "         (default setting: turned off)\n";
    print STDERR "      -o: To print result to specified file.\n";
    print STDERR "         (default setting: \$zdock_output_file.rdockoutput)\n";
    print STDERR "      To run this script properly, certain files have to be in place. Check error messages and make changes accordingly.\n\n";
    print STDERR "              ****************************************************************\n";
    print STDERR "                    All directory names and file names must be in lower case.\n";
    print STDERR "              ****************************************************************\n\n";
    exit;
}


#read in parameters
($zdock_output_file_name,$energy_output_file_name,$output_directory,$xfrom,$xto,$keep_output)=read_arguments(@ARGV);

#temporary files
$identifier=$energy_output_file_name;
$charmm_input_first_run=$output_directory.$energy_output_file_name."_charmm_input_first_run.inp";
$charmm_input_second_run=$output_directory.$energy_output_file_name."_charmm_input_second_run.inp";
$energy_output_first_run=$output_directory.$energy_output_file_name."_first_energy_output";
$energy_output_second_run=$output_directory.$energy_output_file_name."_second_energy_output";
$ligand_chain1_template_of_ligands=$output_directory.$energy_output_file_name."_ligand_chain1_template_of_ligands.pdb";
$ligand_chain1_temp_rotated=$output_directory.$energy_output_file_name."_ligand_chain1_temp_rotated.pdb";
$ligand_chain1_second_charmm_input=$output_directory.$energy_output_file_name."_ligand_chain1_minimized_first_step.pdb";
$ligand_chain1_first_minimization_output=$output_directory.$energy_output_file_name."_ligand_chain1_first_minimization.pdb";
$ligand_chain1_second_minimization_output=$output_directory.$energy_output_file_name."_ligand_chain1_second_minimization.pdb";
$ligand_chain2_template_of_ligands=$output_directory.$energy_output_file_name."_ligand_chain2_template_of_ligands.pdb";
$ligand_chain2_temp_rotated=$output_directory.$energy_output_file_name."_ligand_chain2_temp_rotated.pdb";
$ligand_chain2_second_charmm_input=$output_directory.$energy_output_file_name."_ligand_chain2_minimized_first_step.pdb";
$ligand_chain2_first_minimization_output=$output_directory.$energy_output_file_name."_ligand_chain2_first_minimization.pdb";
$ligand_chain2_second_minimization_output=$output_directory.$energy_output_file_name."_ligand_chain2_second_minimization.pdb";
$ligand_minimized_first=$output_directory.$energy_output_file_name."_ligand_minimized_first_step.pdb";
$ligand_minimized_second=$output_directory.$energy_output_file_name."_ligand_minimized_second_step.pdb";
$receptor_chain1_template_of_receptors=$output_directory.$energy_output_file_name."_receptor_chain1_template_of_receptors.pdb";
$receptor_chain1_second_charmm_input=$output_directory.$energy_output_file_name."_receptor_chain1_second_charmm_input.pdb";
$receptor_chain1_first_minimization_output=$output_directory.$energy_output_file_name."_receptor_chain1_first_minimization.pdb";
$receptor_chain1_second_minimization_output=$output_directory.$energy_output_file_name."_receptor_chain1_second_minimization.pdb";
$receptor_chain2_template_of_receptors=$output_directory.$energy_output_file_name."_receptor_chain2_template_of_receptors.pdb";
$receptor_chain2_second_charmm_input=$output_directory.$energy_output_file_name."_receptor_chain2_second_charmm_input.pdb";
$receptor_chain2_first_minimization_output=$output_directory.$energy_output_file_name."_receptor_chain2_first_minimization.pdb";
$receptor_chain2_second_minimization_output=$output_directory.$energy_output_file_name."_receptor_chain2_second_minimization.pdb";
$receptor_minimized_first=$output_directory.$energy_output_file_name."_receptor_minimized_first_step.pdb";
$receptor_minimized_second=$output_directory.$energy_output_file_name."_receptor_minimized_second_step.pdb";
$junk=$output_directory.$energy_output_file_name.".junk";
$deltag_output_filename=$output_directory.$energy_output_file_name."_deltaG.output";
$energy_output_file_name=$output_directory.$energy_output_file_name;

@ligand=();
@receptor=();

#check avaliability of all files and directories
if(open(CRAP, "$create_lig_c_program")){}else{print STDERR "\nERROR: Can't find file $create_lig_c_program. Program exited.\n\n";exit;}
if(open(CRAP, "$ace_c_program")){}else{print STDERR "\nERROR: Can't find file $ace_c_program. Program exited.\n\n";exit;}
if(open(CRAP, "$bin_file_1")){}else{print STDERR "\nERROR: Can't find file $bin_file_1. Program exited.\n\n";exit;}
if(open(CRAP, "$bin_file_2")){}else{print STDERR "\nERROR: Can't find file $bin_file_2. Program exited.\n\n";exit;}
if(open(ZDOCK_OUTPUT, "$zdock_output_file_name")){
    @zdock_output=<ZDOCK_OUTPUT>;chomp @zdock_output;
    ($n, $spacing)=split(" ", $zdock_output[0]);
    ($rand1, $rand2, $rand3)=split(" ", $zdock_output[1]);
    ($rec, $r1, $r2, $r3) = split (" ", $zdock_output[2]);
    ($lig, $l1, $l2, $l3) = split (" ", $zdock_output[3]);
    if($xto+3>$#zdock_output){print STDERR "ERROR: xto out of boundary. Reset to ",$#zdock_output-3,"\n\n"; $xto=$#zdock_output-3; exit;}
    if($xfrom<1){print STDERR "ERROR: xfrom less than one. Reset to 1.\n\n";$xfrom=1; exit;}
}
else{print STDERR "\nERROR: Can't find zdock_output_file $zdock_output_file_name. Program exited.\n\n";exit;}
if(open(RECEPTOR, "$rec")){@receptor_original=<RECEPTOR>;chomp @receptor_original;}
else{print STDERR "\nERROR: Can't find receptor_pdb_file $rec. Program exited.\n\n";exit;}
if(open(LIGAND, "$lig")){@ligand_original=<LIGAND>;chomp @ligand_original;}
else{print STDERR "\nERROR: Can't find ligand_pdb_file $lig. Program exited.\n\n";exit;}

if(opendir (CRAP,"$output_directory")){}else{print STDERR "\nERROR: Can't open output_directory. Program exited.\n\n";exit;}
      
#add an extra colomn in ligand and receptor pdb and make charged residues neutual
foreach $line (@ligand_original){
    if ($line =~/^ATOM/){
	$length=length($line);
	if ($length < 72){
	    for($i=$length; $i<76; $i++){
		$line=$line . " ";
	    }
	}
	substr($line, 72, 4)="LIG1";
	$res=substr($line, 17, 4);
	$atom=substr($line, 13, 3);
	# change CD1 of ILE to CD
        if (($atom eq "CD1")&&($res eq "ILE ")){
            substr($line, 13, 3)="CD ";
        }
        elsif ($res eq "ASP "){
            substr($line, 17, 4)="ASPH";
        }
        elsif ($res eq "GLU "){
            substr($line, 17, 4)="GLUH";
        }
        elsif ($res eq "ARG "){
            substr($line, 17, 4)="ARGN";
        }
        elsif ($res eq "LYS "){
            substr($line, 17, 4)="LYSN";
        }
        if ($atom eq "OXT"){
            substr($line, 13, 4)="OCT2";
        }
	push @ligand, $line;
    }
}

foreach $line (@receptor_original){
    if ($line =~ /^ATOM/){
	$length=length($line);
	if ($length < 72){
	    for($i=$length; $i<76; $i++){
		$line=$line . " ";
	    }
	}
	substr($line, 72, 4)="REC1";
	$res=substr($line, 17, 4);
	$atom=substr($line, 13, 3);
	# change CD1 of ILE to CD
        if (($atom eq "CD1")&&($res eq "ILE ")){
            substr($line, 13, 3)="CD ";
        }
        elsif ($res eq "ASP "){
            substr($line, 17, 4)="ASPH";
        }
        elsif ($res eq "GLU "){
            substr($line, 17, 4)="GLUH";
        }
        elsif ($res eq "ARG "){
            substr($line, 17, 4)="ARGN";
        }
        elsif ($res eq "LYS "){
            substr($line, 17, 4)="LYSN";
        }
        if ($atom eq "OXT"){
            substr($line, 13, 4)="OCT2";
        }
	push @receptor, $line;
    }
}
 
#split ligand if it contains multiple chains.
#split receptor if it contains multiple chains.
#ERROR if receptor has >2 chains or ligand has >2 chains

$previours_domain = substr($ligand[0],21,1);
$flag_l=0;
$ligand_two_chains=0;
@ligand_chain2=();
for $i (0 .. $#ligand)
{
    $domain = substr($ligand[$i],21,1);
    if($domain ne $previours_domain){
	$flag_l++;
	if($flag_l==2){print STDERR "ERROR: Ligand file contains more than two chains. modify program.\n\n";exit;}
    }
    if($flag_l){
	substr($ligand[$i], 72, 4)="LIG2";
	push @ligand_chain2, $ligand[$i];
	delete $ligand[$i];
	$ligand_two_chains=1;
    }
    $previours_domain = $domain;
}

$previours_domain = substr($receptor[0],21,1);
$flag_r=0;
$receptor_two_chains=0;
@receptor_chain2=();
for $i (0 .. $#receptor)
{
    $domain = substr($receptor[$i],21,1);
    if($domain ne $previours_domain){
	$flag_r++;
	if($flag_r==2){print STDERR "ERROR: receptor file contains more than two chains. modify program.\n\n";exit;}
    }
    if($flag_r){
	substr($receptor[$i], 72, 4)="REC2";
	push @receptor_chain2, $receptor[$i];
	delete $receptor[$i];
	$receptor_two_chains=1;
    }
    $previours_domain = $domain;
}

#make ligand and receptor gapfree and give ligand residue_index an offset equals to the last residue_index of receptor
format PDBLINE=
ATOM   @>>>  @<<<@<<<@@>>>    @>>>>>>>@>>>>>>>@>>>>>>>@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$atom_index,$atom,$residue,$chain,$residue_index_new,$x,$y,$z,$crap
.
$residue_index_new=0;
$last_residue="";
$last_residue_index="-1";

open PDBLINE, ">$receptor_chain1_template_of_receptors";
foreach $line (@receptor){
    $atom_index=substr($line, 4,7);
    $atom_index*=1;
    $atom=substr($line, 13, 4);
    $atom=~s/\s+//g;
    $residue=substr($line,17,4);
    $residue=~s/\s+//g;
    $chain=substr($line,21,1);
    $chain=~s/\s+//g; 
    $residue_index=substr($line,22,5);
    if($last_residue ne $residue || $last_residue_index ne $residue_index){
	$residue_index_new++;
    }
    $x=substr($line,30,8);
    $y=substr($line,38,8);
    $z=substr($line,46,8);
    $crap=substr($line,54,100);

    write (PDBLINE);
    $last_residue=$residue;
    $last_residue_index=$residue_index;
}
close PDBLINE;

if($receptor_two_chains){
    open PDBLINE, ">$receptor_chain2_template_of_receptors";
    foreach $line (@receptor_chain2){
	$atom_index=substr($line, 4,7);
	$atom_index*=1;
	$atom=substr($line, 13, 4);
	$atom=~s/\s+//g;
	$residue=substr($line,17,4);
	$residue=~s/\s+//g;
	$chain=substr($line,21,1);
	$chain=~s/\s+//g; 
	$residue_index=substr($line,22,5);
	if($last_residue ne $residue || $last_residue_index ne $residue_index){
	    $residue_index_new++;
	}
	$x=substr($line,30,8);
	$y=substr($line,38,8);
	$z=substr($line,46,8);
	$crap=substr($line,54,100);
	
	write (PDBLINE);
	$last_residue=$residue;
	$last_residue_index=$residue_index;
    }
    close PDBLINE;
}    


open PDBLINE, ">$ligand_chain1_template_of_ligands";
$last_residue="";
$last_residue_index="-1";
foreach $line (@ligand){
    $atom_index=substr($line, 4,7);
    $atom_index*=1;
    $atom=substr($line, 13, 4);
    $atom=~s/\s+//g;
    $residue=substr($line,17,4);
    $residue=~s/\s+//g;
    $chain=substr($line,21,1);
    $chain=~s/\s+//g; 
    $residue_index=substr($line,22,5);
    if($last_residue ne $residue || $last_residue_index ne $residue_index){
	$residue_index_new++;
    }
    $x=substr($line,30,8);
    $y=substr($line,38,8);
    $z=substr($line,46,8);
    $crap=substr($line,54,100);

    write (PDBLINE);
    $last_residue=$residue;
    $last_residue_index=$residue_index;
}

if($ligand_two_chains){
    open PDBLINE, ">$ligand_chain2_template_of_ligands";
    foreach $line (@ligand_chain2){
	$atom_index=substr($line, 4,7);
	$atom_index*=1;
	$atom=substr($line, 13, 4);
	$atom=~s/\s+//g;
	$residue=substr($line,17,4);
	$residue=~s/\s+//g;
	$chain=substr($line,21,1);
	$chain=~s/\s+//g; 
	$residue_index=substr($line,22,5);
	if($last_residue ne $residue || $last_residue_index ne $residue_index){
	    $residue_index_new++;
	}
	$x=substr($line,30,8);
	$y=substr($line,38,8);
	$z=substr($line,46,8);
	$crap=substr($line,54,100);
	
	write (PDBLINE);
	$last_residue=$residue;
	$last_residue_index=$residue_index;
    }
}


close PDBLINE;

open LIGAND, "$ligand_chain1_template_of_ligands";
@ligand=<LIGAND>;
chomp @ligand;
if($ligand_two_chains){
    open LIGAND_CHAIN2, "$ligand_chain2_template_of_ligands";
    @ligand_chain2=<LIGAND_CHAIN2>;
    chomp @ligand_chain2;
}    


open RECEPTOR, "$receptor_chain1_template_of_receptors";
@receptor=<RECEPTOR>;
chomp @receptor;
if($receptor_two_chains){
    open RECEPTOR_CHAIN2, "$receptor_chain2_template_of_receptors";
    @receptor_chain2=<RECEPTOR_CHAIN2>;
    chomp @receptor_chain2;
}    

#find disulfide bond
@ligand_disulfide_1=();
@ligand_disulfide_2=();
@x=();
@y=();
@z=();
@res_num=();
foreach $line (@ligand){
  
    $atom = substr($line, 13, 2);
    $res = substr($line, 17, 3);
    if (($atom eq "SG")&&($res eq "CYS")){
	push @res_num,substr($line, 22, 4);
	push @x,substr($line, 30, 8);
	push @y,substr($line, 38, 8);
	push @z,substr($line, 46, 8);
    }
}
for $i (0 .. $#x){
    for $j ($i+1 .. $#x){
	$dis=sqrt(($x[$i]-$x[$j])*($x[$i]-$x[$j])+($y[$i]-$y[$j])*($y[$i]-$y[$j])+($z[$i]-$z[$j])*($z[$i]-$z[$j]));
	if ($dis <= 2.5){
	    push @ligand_disulfide_1, $res_num[$i];
	    push @ligand_disulfide_2, $res_num[$j];
	}
    }
}
if($ligand_two_chains){
    @ligand_chain2_disulfide_1=();
    @ligand_chain2_disulfide_2=();
    @x=();
    @y=();
    @z=();
    @res_num=();
    foreach $line (@ligand_chain2){
	$atom = substr($line, 13, 2);
	$res = substr($line, 17, 3);
	if (($atom eq "SG")&&($res eq "CYS")){
	    push @res_num,substr($line, 22, 4);
	    push @x,substr($line, 30, 8);
	    push @y,substr($line, 38, 8);
	    push @z,substr($line, 46, 8);
	}
    }
    for $i (0 .. $#x){
	for $j ($i+1 .. $#x){
	    $dis=sqrt(($x[$i]-$x[$j])*($x[$i]-$x[$j])+($y[$i]-$y[$j])*($y[$i]-$y[$j])+($z[$i]-$z[$j])*($z[$i]-$z[$j]));
	    if ($dis <= 2.5){
		push @ligand_chain2_disulfide_1, $res_num[$i];
		push @ligand_chain2_disulfide_2, $res_num[$j];
	    }
	}
    }
}    



@receptor_disulfide_1=();
@receptor_disulfide_2=();
@x=();
@y=();
@z=();
@res_num=();
foreach $line (@receptor){
    $atom = substr($line, 13, 2);
    $res = substr($line, 17, 3);
    if (($atom eq "SG")&&($res eq "CYS")){
	push @res_num,substr($line, 22, 4);
	push @x,substr($line, 30, 8);
	push @y,substr($line, 38, 8);
	push @z,substr($line, 46, 8);
    }
}
for $i (0 .. $#x){
    for $j ($i+1 .. $#x){
	$dis=sqrt(($x[$i]-$x[$j])*($x[$i]-$x[$j])+($y[$i]-$y[$j])*($y[$i]-$y[$j])+($z[$i]-$z[$j])*($z[$i]-$z[$j]));
	if ($dis <= 2.5){
	    push @receptor_disulfide_1, $res_num[$i];
	    push @receptor_disulfide_2, $res_num[$j];
	}
    }
}

if($receptor_two_chains){
    @receptor_chain2_disulfide_1=();
    @receptor_chain2_disulfide_2=();
    @x=();
    @y=();
    @z=();
    @res_num=();
    foreach $line (@receptor_chain2){
	$atom = substr($line, 13, 2);
	$res = substr($line, 17, 3);
	if (($atom eq "SG")&&($res eq "CYS")){
	    push @res_num,substr($line, 22, 4);
	    push @x,substr($line, 30, 8);
	    push @y,substr($line, 38, 8);
	    push @z,substr($line, 46, 8);
	}
    }
    for $i (0 .. $#x){
	for $j ($i+1 .. $#x){
	    $dis=sqrt(($x[$i]-$x[$j])*($x[$i]-$x[$j])+($y[$i]-$y[$j])*($y[$i]-$y[$j])+($z[$i]-$z[$j])*($z[$i]-$z[$j]));
	    if ($dis <= 2.5){
		push @receptor_chain2_disulfide_1, $res_num[$i];
		push @receptor_chain2_disulfide_2, $res_num[$j];
	    }
	}
    }
}    

open (ENERGY_OUTPUT, ">$energy_output_file_name");
print ENERGY_OUTPUT "index\tfirst:ELEC\tfirst:VDW\tfirst:ACE\tsecond:ELEC\tsecond:VDW\tsecond:ACE\n";

#foreach line of zdock_output:
#1. creat rotated ligand file
#2. creat charmm input
#3. run charmm
#4. change neutral back to charged
#5. creat charmm input
#6. run charmm
#7. clean up temporary files

for $i ($xfrom .. $xto){
# creat rotated ligand file
    ($angl_x, $angl_y, $angl_z, $tran_x, $tran_y, $tran_z, $crap)=split (" ", $zdock_output[$i+3] );
    system "$create_lig_c_program $ligand_chain1_temp_rotated $ligand_chain1_template_of_ligands $rand1 $rand2 $rand3 $r1 $r2 $r3 $l1 $l2 $l3 $angl_x $angl_y $angl_z $tran_x $tran_y $tran_z $n $spacing\n";

    if(open(CRAP,"$ligand_chain1_temp_rotated")){}else{print STDERR "ERROR: Unable to creat rotated ligand file $ligand_chain1_temp_rotated. Program exited.\n\n";exit;}
    if($ligand_two_chains){
	system "$create_lig_c_program $ligand_chain2_temp_rotated $ligand_chain2_template_of_ligands $rand1 $rand2 $rand3 $r1 $r2 $r3 $l1 $l2 $l3 $angl_x $angl_y $angl_z $tran_x $tran_y $tran_z $n $spacing\n";
	if(open(CRAP,"$ligand_chain2_temp_rotated")){}else{print STDERR "ERROR: Unable to creat rotated ligand file $ligand_chain2_temp_rotated. Program exited.\n\n";exit;}
    }
#creat charmm input
    open INP, ">$charmm_input_first_run";
    print INP "* This CHARMM input file calculates the interaction energy\n";
    print INP "*\n";
    print INP "set prnlev 15\n";
    print INP "set iolev 1\n";
    print INP "OPEN READ UNIT 11 $charmm_read_parameter_file_command $bin_file_1\n";
    print INP "READ RTF UNIT 11 $charmm_read_parameter_file_command\n";
    print INP "CLOSE UNIT 11\n";
    print INP "OPEN READ UNIT 12 $charmm_read_parameter_file_command $bin_file_2\n";
    print INP "READ PARAMETERS UNIT 12 $charmm_read_parameter_file_command\n";
    print INP "CLOSE UNIT 12\n";
    print INP "\n";
    print INP "OPEN NAME $energy_output_first_run UNIT 19 WRIT FORM\n";
    print INP "\n";
    print INP "OPEN READ UNIT 14 CARD NAME $receptor_chain1_template_of_receptors\n";
    print INP "READ SEQU PDB UNIT 14\n";
    print INP "GENERATE REC1 SETUP\n";
    if($#receptor_disulfide_1>=0){
	for $j (0 .. $#receptor_disulfide_1){
	    print INP "PATCH DISU REC1 $receptor_disulfide_1[$j] REC1 $receptor_disulfide_2[$j]\n";
	}
    }
    print INP "REWIND UNIT 14\n";
    print INP "READ COOR PDB UNIT 14\n";
    print INP "CLOSE UNIT 14\n";
    print INP "\n";
    if($receptor_two_chains){
	print INP "OPEN READ UNIT 15 CARD NAME $receptor_chain2_template_of_receptors\n";
	print INP "READ SEQU PDB UNIT 15\n";
	print INP "GENERATE REC2 SETUP\n";
	if($#receptor_chain2_disulfide_1>=0){
	    for $j (0 .. $#receptor_chain2_disulfide_1){
		print INP "PATCH DISU REC2 $receptor_chain2_disulfide_1[$j] REC2 $receptor_chain2_disulfide_2[$j]\n";
	    }
	}
	print INP "REWIND UNIT 15\n";
	print INP "READ COOR PDB UNIT 15\n";
	print INP "CLOSE UNIT 15\n";
	print INP "\n";
    }	
    print INP "OPEN READ UNIT 16 CARD NAME $ligand_chain1_temp_rotated\n";
    print INP "READ SEQU PDB UNIT 16\n";
    print INP "GENERATE LIG1 SETUP\n";
    if($#ligand_disulfide_1>=0){
	for $j (0 .. $#ligand_disulfide_1){
	    print INP "PATCH DISU LIG1 $ligand_disulfide_1[$j] LIG1 $ligand_disulfide_2[$j]\n";
	}
    }
    print INP "REWIND UNIT 16\n";
    print INP "READ COOR PDB UNIT 16\n";
    print INP "CLOSE UNIT 16\n";
    print INP "\n";
    if($ligand_two_chains){
	print INP "OPEN READ UNIT 17 CARD NAME $ligand_chain2_temp_rotated\n";
	print INP "READ SEQU PDB UNIT 17\n";
	print INP "GENERATE LIG2 SETUP\n";
	if($#ligand_chain2_disulfide_1>=0){
	    for $j (0 .. $#ligand_chain2_disulfide_1){
		print INP "PATCH DISU LIG2 $ligand_chain2_disulfide_1[$j] LIG2 $ligand_chain2_disulfide_2[$j]\n";
	    }
	}
	print INP "REWIND UNIT 17\n";
	print INP "READ COOR PDB UNIT 17\n";
	print INP "CLOSE UNIT 17\n";
	print INP "\n";
    }

    print INP "IC FILL PRESERVE\n";
    print INP "IC PARAMETERS\n";
    print INP "IC BUILD\n";
    print INP "HBUILD\n";
    print INP "\n";
    print INP "! minimize all atoms 50 steps by vdw\n";
    print INP "NBONDS NOELEC VSHIFT RDIE EPS 4. CUTNB 17.0\n";
    print INP "MINIMIZE ABNR NSTEP 50 NPRINT 50 TOLENG 0.0001 TOLGRD 0.0001\n";
    print INP "\n";
    print INP "! minimize hydrogen atoms 60 steps by elec \n";
    print INP "CONS HARM SELE .NOT. (type h*) END FORCE 20 MASS \n";
    print INP "NBONDS ELEC VSHIFT RDIE EPS 4. CUTNB 17.0\n";
    print INP "MINIMIZE ABNR NSTEP 10 NPRINT 10 TOLENG 0.0001 TOLGRD 0.0001\n";
    print INP "MINIMIZE ABNR NSTEP 10 NPRINT 10 TOLENG 0.0001 TOLGRD 0.0001\n";
    print INP "INTER SELE SEGID REC1 ";
    if($receptor_two_chains){
	print INP ".or. SEGID REC2 ";
    }
    print INP "END SELE SEGID LIG1 ";
    if($ligand_two_chains){
	print INP ".or. SEGID LIG2";
    }
    print INP " END\n";

    print INP "WRIT TITL UNIT 19\n";
    print INP "*?elec ?vdw ?ENER\n";
    print INP "*\n";
    print INP "close unit 19\n";
    print INP "\n";
#print receptor(s)
    print INP "OPEN UNIT 23 NAME $receptor_chain1_first_minimization_output WRITE FORM\n";
    print INP "WRITE COOR SELE SEGID REC1 END PDB UNIT 23\n";
    print INP "*\n";
    print INP "close unit 23\n";
    if($receptor_two_chains){
	print INP "OPEN UNIT 24 NAME $receptor_chain2_first_minimization_output WRITE FORM\n";
	print INP "WRITE COOR SELE SEGID REC2 END PDB UNIT 24\n";
	print INP "*\n";
	print INP "close unit 24\n";
    }
#print ligand(s)
    print INP "\n";
    print INP "OPEN UNIT 25 NAME $ligand_chain1_first_minimization_output WRITE FORM\n";
    print INP "WRITE COOR SELE SEGID LIG1 END PDB UNIT 25\n";
    print INP "*\n";
    print INP "close unit 25\n";
    if($ligand_two_chains){
	print INP "OPEN UNIT 26 NAME $ligand_chain2_first_minimization_output WRITE FORM\n";
	print INP "WRITE COOR SELE SEGID LIG2 END PDB UNIT 26\n";
	print INP "*\n";
	print INP "close unit 26\n";
    }	
	
    print INP "\n";
    print INP "DELETE ATOM SELE ATOM * * * END\n";
    print INP "stop\n";
close INP;
# run charmm

    printf "%s/%s  %d  ...   %2.0f%%\n",$rec,$lig,$i,100.0*($i-$xfrom+1)/($xto-$xfrom+1);
    $ENV{'chmstdin'} = $charmm_input_first_run;
    unlink ("$junk");
    system "charmm < $charmm_input_first_run > $junk";

#change charmm output pdb residues from neutral to charge and print
   if($receptor_two_chains){
	if(open(RECEPTOR_CHAIN1, "$receptor_chain1_first_minimization_output")){@receptor_chain1=<RECEPTOR_CHAIN1>;chomp @receptor_chain1;}
	else{print STDERR "\nFirst CHARMM run error: Check \"charmm < $charmm_input_first_run\"\n"; exit;}
	open RECEPTOR_MINIMIZED_FIRST, ">$receptor_minimized_first";
	open RECEPTOR_CHAIN1_SECOND_CHARMM_INPUT, ">$receptor_chain1_second_charmm_input";
	foreach $line (@receptor_chain1){
	    if ($line =~ /^ATOM/){
		$res = substr($line, 17, 4);
		if ($res eq "ASPH"){
		    substr($line, 17, 4)="ASP ";
		}
		elsif ($res eq "GLUH"){
		    substr($line, 17, 4)="GLU ";
		}
		elsif ($res eq "ARGN"){
		    substr($line, 17, 4)="ARG ";
		}
		elsif ($res eq "LYSN"){
		    substr($line, 17, 4)="LYS ";
		}
		($trash, $trash, $head )= split (" ", $line);
		$atom_name=substr($head,0,1);
		if (!(($atom_name eq "H") && (($res eq "ASPH") ||($res eq "GLUH") || ($res eq "ARGN") || ($res eq "LYSN")))){ 	
		    print RECEPTOR_MINIMIZED_FIRST "$line\n";
		    print RECEPTOR_CHAIN1_SECOND_CHARMM_INPUT "$line\n";
		}
	    }
	}
	print RECEPTOR_CHAIN1_SECOND_CHARMM_INPUT "END\n";

	if(open(RECEPTOR_CHAIN2, "$receptor_chain2_first_minimization_output")){@receptor_chain2=<RECEPTOR_CHAIN2>;chomp @receptor_chain2;}
	else{print STDERR "\nFirst CHARMM run error: Check \"charmm < $charmm_input_first_run\"\n"; exit;}	
	open RECEPTOR_CHAIN2_SECOND_CHARMM_INPUT, ">$receptor_chain2_second_charmm_input";
	foreach $line (@receptor_chain2){
	    if ($line =~ /^ATOM/){
		$res = substr($line, 17, 4);
		if ($res eq "ASPH"){
		    substr($line, 17, 4)="ASP ";
		}
		elsif ($res eq "GLUH"){
		    substr($line, 17, 4)="GLU ";
		}
		elsif ($res eq "ARGN"){
		    substr($line, 17, 4)="ARG ";
		}
		elsif ($res eq "LYSN"){
		    substr($line, 17, 4)="LYS ";
		}
		($trash, $trash, $head )= split (" ", $line);
		$atom_name=substr($head,0,1);
		if (!(($atom_name eq "H") && (($res eq "ASPH") ||($res eq "GLUH") || ($res eq "ARGN") || ($res eq "LYSN")))){ 	
		    print RECEPTOR_CHAIN2_SECOND_CHARMM_INPUT "$line\n";
		    print RECEPTOR_MINIMIZED_FIRST "$line\n";
		}
	    }
	}
	print RECEPTOR_CHAIN2_SECOND_CHARMM_INPUT "END\n";
	print RECEPTOR_MINIMIZED_FIRST "END\n";
    }
    else{
	if(open(RECEPTOR, "$receptor_chain1_first_minimization_output")){@receptor=<RECEPTOR>;chomp @receptor;}
	else{print STDERR "\nFirst CHARMM run error: Check \"charmm < $charmm_input_first_run\"\n"; exit;}
	open RECEPTOR_CHAIN1_SECOND_CHARMM_INPUT, ">$receptor_chain1_second_charmm_input";
	open RECEPTOR_MINIMIZED_FIRST, ">$receptor_minimized_first";
	foreach $line (@receptor){
	    if ($line =~ /^ATOM/){
		$res = substr($line, 17, 4);
		if ($res eq "ASPH"){
		    substr($line, 17, 4)="ASP ";
		}
		elsif ($res eq "GLUH"){
		    substr($line, 17, 4)="GLU ";
		}
		elsif ($res eq "ARGN"){
		    substr($line, 17, 4)="ARG ";
		}
		elsif ($res eq "LYSN"){
		    substr($line, 17, 4)="LYS ";
		}
		($trash, $trash, $head )= split (" ", $line);
		$atom_name=substr($head,0,1);
		if (!(($atom_name eq "H") && (($res eq "ASPH") ||($res eq "GLUH") || ($res eq "ARGN") || ($res eq "LYSN")))){ 	
		    print RECEPTOR_CHAIN1_SECOND_CHARMM_INPUT "$line\n";
		    print RECEPTOR_MINIMIZED_FIRST "$line\n";
		}
	    }
	}
	print RECEPTOR_CHAIN1_SECOND_CHARMM_INPUT "END\n";
	print RECEPTOR_MINIMIZED_FIRST "END\n";
    }

   if($ligand_two_chains){
	if(open(LIGAND_CHAIN1, "$ligand_chain1_first_minimization_output")){@ligand_chain1=<LIGAND_CHAIN1>;chomp @ligand_chain1;}
	else{print STDERR "\nFirst CHARMM run error: Check \"charmm < $charmm_input_first_run\"\n"; exit;}
	open LIGAND_MINIMIZED_FIRST, ">$ligand_minimized_first";
	open LIGAND_CHAIN1_SECOND_CHARMM_INPUT, ">$ligand_chain1_second_charmm_input";
	foreach $line (@ligand_chain1){
	    if ($line =~ /^ATOM/){
		$res = substr($line, 17, 4);
		if ($res eq "ASPH"){
		    substr($line, 17, 4)="ASP ";
		}
		elsif ($res eq "GLUH"){
		    substr($line, 17, 4)="GLU ";
		}
		elsif ($res eq "ARGN"){
		    substr($line, 17, 4)="ARG ";
		}
		elsif ($res eq "LYSN"){
		    substr($line, 17, 4)="LYS ";
		}
		($trash, $trash, $head )= split (" ", $line);
		$atom_name=substr($head,0,1);
		if (!(($atom_name eq "H") && (($res eq "ASPH") ||($res eq "GLUH") || ($res eq "ARGN") || ($res eq "LYSN")))){ 	
		    print LIGAND_MINIMIZED_FIRST "$line\n";
		    print LIGAND_CHAIN1_SECOND_CHARMM_INPUT "$line\n";
		}
	    }
	}
	print LIGAND_CHAIN1_SECOND_CHARMM_INPUT "END\n";

	if(open(LIGAND_CHAIN2, "$ligand_chain2_first_minimization_output")){@ligand_chain2=<LIGAND_CHAIN2>;chomp @ligand_chain2;}
	else{print STDERR "\nFirst CHARMM run error: Check \"charmm < $charmm_input_first_run\"\n"; exit;}	
	open LIGAND_CHAIN2_SECOND_CHARMM_INPUT, ">$ligand_chain2_second_charmm_input";
	foreach $line (@ligand_chain2){
	    if ($line =~ /^ATOM/){
		$res = substr($line, 17, 4);
		if ($res eq "ASPH"){
		    substr($line, 17, 4)="ASP ";
		}
		elsif ($res eq "GLUH"){
		    substr($line, 17, 4)="GLU ";
		}
		elsif ($res eq "ARGN"){
		    substr($line, 17, 4)="ARG ";
		}
		elsif ($res eq "LYSN"){
		    substr($line, 17, 4)="LYS ";
		}
		($trash, $trash, $head )= split (" ", $line);
		$atom_name=substr($head,0,1);
		if (!(($atom_name eq "H") && (($res eq "ASPH") ||($res eq "GLUH") || ($res eq "ARGN") || ($res eq "LYSN")))){ 	
		    print LIGAND_CHAIN2_SECOND_CHARMM_INPUT "$line\n";
		    print LIGAND_MINIMIZED_FIRST "$line\n";
		}
	    }
	}
	print LIGAND_CHAIN2_SECOND_CHARMM_INPUT "END\n";
	print LIGAND_MINIMIZED_FIRST "END\n";
    }
    else{
	if(open(LIGAND, "$ligand_chain1_first_minimization_output")){@ligand=<LIGAND>;chomp @ligand;}
	else{print STDERR "\nFirst CHARMM run error: Check \"charmm < $charmm_input_first_run\"\n"; exit;}
	open LIGAND_CHAIN1_SECOND_CHARMM_INPUT, ">$ligand_chain1_second_charmm_input";
	open LIGAND_MINIMIZED_FIRST, ">$ligand_minimized_first";
	foreach $line (@ligand){
	    if ($line =~ /^ATOM/){
		$res = substr($line, 17, 4);
		if ($res eq "ASPH"){
		    substr($line, 17, 4)="ASP ";
		}
		elsif ($res eq "GLUH"){
		    substr($line, 17, 4)="GLU ";
		}
		elsif ($res eq "ARGN"){
		    substr($line, 17, 4)="ARG ";
		}
		elsif ($res eq "LYSN"){
		    substr($line, 17, 4)="LYS ";
		}
		($trash, $trash, $head )= split (" ", $line);
		$atom_name=substr($head,0,1);
		if (!(($atom_name eq "H") && (($res eq "ASPH") ||($res eq "GLUH") || ($res eq "ARGN") || ($res eq "LYSN")))){ 	
		    print LIGAND_CHAIN1_SECOND_CHARMM_INPUT "$line\n";
		    print LIGAND_MINIMIZED_FIRST "$line\n";
		}
	    }
	}
	print LIGAND_CHAIN1_SECOND_CHARMM_INPUT "END\n";
	print LIGAND_MINIMIZED_FIRST "END\n";
    }



#calculate first energy outputs : elec VDW from charmm and ACE of minimized structures changed back to charge
    open CRAP, "$energy_output_first_run";
    @crap=<CRAP>;
    chomp $crap[0];
    ($elec_output,$vdw_output,$crap)=split /\s+/, $crap[0];
    $ace_output="---";
    if(open(CRAP, "$receptor_minimized_first")){}
    else{print STDERR "\nFirst CHARMM run error: Can't find \"$receptor_minimized_first\"\n"; exit;}
    if(open(CRAP, "$ligand_minimized_first")){}
    else{print STDERR "\nFirst CHARMM run error: Can't find \"$ligand_minimized_first\"\n"; exit;}

    open CRAP, "$ace_c_program $receptor_minimized_first $ligand_minimized_first $deltag_output_filename $identifier|";
    @crap=<CRAP>;
    foreach (@crap){
        chomp $_;
        if($_=~/^COUL:/){
            ($crap,$crap,$crap,$crap,$crap,$ace_output)=split /\s+/, $_;
            last;
        }
    }
    print ENERGY_OUTPUT "$i\t$elec_output\t$vdw_output\t$ace_output\t";



#creat second CHARMM input

    open INP, ">$charmm_input_second_run";
    print INP "* This CHARMM input file calculates the interaction energy\n";
    print INP "*\n";
    print INP "set prnlev 15\n";
    print INP "set iolev 1\n";
    print INP "OPEN READ UNIT 11 $charmm_read_parameter_file_command $bin_file_1\n";
    print INP "READ RTF UNIT 11 $charmm_read_parameter_file_command\n";
    print INP "CLOSE UNIT 11\n";
    print INP "OPEN READ UNIT 12 $charmm_read_parameter_file_command $bin_file_2\n";
    print INP "READ PARAMETERS UNIT 12 $charmm_read_parameter_file_command\n";
    print INP "CLOSE UNIT 12\n";
    print INP "\n";
    print INP "OPEN NAME $energy_output_second_run UNIT 19 WRIT FORM\n";
    print INP "\n";
    print INP "OPEN READ UNIT 14 CARD NAME $receptor_chain1_second_charmm_input\n";
    print INP "READ SEQU PDB UNIT 14\n";
    print INP "GENERATE REC1 SETUP\n";
    if($#receptor_disulfide_1>=0){
	for $j (0 .. $#receptor_disulfide_1){
	    print INP "PATCH DISU REC1 $receptor_disulfide_1[$j] REC1 $receptor_disulfide_2[$j]\n";
	}
    }
    print INP "REWIND UNIT 14\n";
    print INP "READ COOR PDB UNIT 14\n";
    print INP "CLOSE UNIT 14\n";
    print INP "\n";
    if($receptor_two_chains){
	print INP "OPEN READ UNIT 15 CARD NAME $receptor_chain2_second_charmm_input\n";
	print INP "READ SEQU PDB UNIT 15\n";
	print INP "GENERATE REC2 SETUP\n";
	if($#receptor_chain2_disulfide_1>=0){
	    for $j (0 .. $#receptor_chain2_disulfide_1){
		print INP "PATCH DISU REC2 $receptor_chain2_disulfide_1[$j] REC2 $receptor_chain2_disulfide_2[$j]\n";
	    }
	}
	print INP "REWIND UNIT 15\n";
	print INP "READ COOR PDB UNIT 15\n";
	print INP "CLOSE UNIT 15\n";
	print INP "\n";
    }	
    
    print INP "OPEN READ UNIT 16 CARD NAME $ligand_chain1_second_charmm_input\n";
    print INP "READ SEQU PDB UNIT 16\n";
    print INP "GENERATE LIG1 SETUP\n";
    if($#ligand_disulfide_1>=0){
	for $j (0 .. $#ligand_disulfide_1){
	    print INP "PATCH DISU LIG1 $ligand_disulfide_1[$j] LIG1 $ligand_disulfide_2[$j]\n";
	}
    }
    print INP "REWIND UNIT 16\n";
    print INP "READ COOR PDB UNIT 16\n";
    print INP "CLOSE UNIT 16\n";
    print INP "\n";
    if($ligand_two_chains){
	print INP "OPEN READ UNIT 17 CARD NAME $ligand_chain2_second_charmm_input\n";
	print INP "READ SEQU PDB UNIT 17\n";
	print INP "GENERATE LIG2 SETUP\n";
	if($#ligand_chain2_disulfide_1>=0){
	    for $j (0 .. $#ligand_chain2_disulfide_1){
		print INP "PATCH DISU LIG2 $ligand_chain2_disulfide_1[$j] LIG2 $ligand_chain2_disulfide_2[$j]\n";
	    }
	}
	print INP "REWIND UNIT 17\n";
	print INP "READ COOR PDB UNIT 17\n";
	print INP "CLOSE UNIT 17\n";
	print INP "\n";
    }

    print INP "HBUILD\n";
    print INP "\n";
    print INP "! minimize all atoms 20 steps by vdw\n";
    print INP "NBONDS VSHIFT RDIE EPS 4. CUTNB 17.0\n";
    print INP "MINIMIZE ABNR NSTEP 10 NPRINT 10 TOLENG 0.0001 TOLGRD 0.0001\n";

    print INP "INTER SELE SEGID REC1 ";
    if($receptor_two_chains){
	print INP ".or. SEGID REC2 ";
    }
    print INP "END SELE SEGID LIG1 ";
    if($ligand_two_chains){
	print INP ".or. SEGID LIG2";
    }
    print INP " END\n";

    print INP "WRIT TITL UNIT 19\n";
    print INP "*?elec ?vdw ?ENER\n";
    print INP "*\n";
    print INP "close unit 19\n";
    print INP "\n";
#write receptor(s)
    print INP "OPEN UNIT 23 NAME $receptor_chain1_second_minimization_output WRITE FORM\n";
    print INP "WRITE COOR SELE SEGID REC1 END PDB UNIT 23\n";
    print INP "*\n";
    print INP "close unit 23\n";
    print INP "\n";
    if($receptor_two_chains){
	print INP "OPEN UNIT 24 NAME $receptor_chain2_second_minimization_output WRITE FORM\n";
	print INP "WRITE COOR SELE SEGID REC2 END PDB UNIT 24\n";
	print INP "*\n";
	print INP "close unit 24\n";
	print INP "\n";
    }	
    print INP "\n";
#write ligand(s)
    print INP "OPEN UNIT 25 NAME $ligand_chain1_second_minimization_output WRITE FORM\n";
    print INP "WRITE COOR SELE SEGID LIG1 END PDB UNIT 25\n";
    print INP "*\n";
    print INP "close unit 25\n";
    print INP "\n";
    if($ligand_two_chains){
	print INP "OPEN UNIT 26 NAME $ligand_chain2_second_minimization_output WRITE FORM\n";
	print INP "WRITE COOR SELE SEGID LIG2 END PDB UNIT 26\n";
	print INP "*\n";
	print INP "close unit 26\n";
	print INP "\n";
    }

    print INP "DELETE ATOM SELE ATOM * * * END\n";
    print INP "stop\n";
    close INP;  

    if(open(CRAP, "$receptor_chain1_second_charmm_input")){}
    else{print STDERR "\nFirst CHARMM run error: Can't find file $receptor_chain1_second_charmm_input\"\n"; exit;}
    if(open(CRAP, "$ligand_chain1_second_charmm_input")){}
    else{print STDERR "\nFirst CHARMM run error: Can't find file $ligand_chain1_second_charmm_input\"\n"; exit;}

#run charmm

    $ENV{'chmstdin'} = $charmm_input_second_run;
    unlink ("$junk");
    system "charmm < $charmm_input_second_run > $junk";

    if(open(CRAP, "$receptor_chain1_second_minimization_output")){}
    else{print STDERR "\nSecond CHARMM run error: Check \"charmm < $charmm_input_second_run\"\n"; exit;}
    if(open(CRAP, "$ligand_chain1_second_minimization_output")){}
    else{print STDERR "\nSecond CHARMM run error: Check \"charmm < $charmm_input_second_run\"\n"; exit;}    

    open CRAP, "$energy_output_second_run";
    @crap=<CRAP>;
    chomp $crap[0];
    ($elec_output,$vdw_output,$crap)=split /\s+/, $crap[0];
    if($receptor_two_chains){
	open RECEPTOR_TEMP, ">$receptor_minimized_second";
	open RECEPTOR_MINIMIZED, "$receptor_chain1_second_minimization_output";
	foreach (<RECEPTOR_MINIMIZED>){
	    if ($_ !~ /END/){
		print RECEPTOR_TEMP "$_";
	    }
	}
        open RECEPTOR_TEMP, ">>$receptor_minimized_second";
        open RECEPTOR_TEMP_CHAIN2, "$receptor_chain2_second_minimization_output";
        foreach (<RECEPTOR_TEMP_CHAIN2>){
            print RECEPTOR_TEMP "$_";
        }
    }
    else{
	open RECEPTOR_TEMP, ">$receptor_minimized_second";
	open RECEPTOR_MINIMIZED, "$receptor_chain1_second_minimization_output";
	foreach (<RECEPTOR_MINIMIZED>){
	    print RECEPTOR_TEMP "$_";
	}
    }
    if($ligand_two_chains){
	open LIGAND_TEMP, ">$ligand_minimized_second";
	open LIGAND_MINIMIZED, "$ligand_chain1_second_minimization_output";
	foreach (<LIGAND_MINIMIZED>){
	    if ($_ !~ /END/){
		print LIGAND_TEMP "$_";
	    }
	}
        open LIGAND_TEMP, ">>$ligand_minimized_second";
        open LIGAND_TEMP_CHAIN2, "$ligand_chain2_second_minimization_output";
        foreach (<LIGAND_TEMP_CHAIN2>){
            print LIGAND_TEMP "$_";
        }
    }
    else{
	open LIGAND_TEMP, ">$ligand_minimized_second";
	open LIGAND_MINIMIZED, "$ligand_chain1_second_minimization_output";
	foreach (<LIGAND_MINIMIZED>){
	    print LIGAND_TEMP "$_";
	}
    }



    open CRAP, "$ace_c_program $receptor_minimized_second  $ligand_minimized_second $deltag_output_filename $identifier|";
    @crap=<CRAP>;
    foreach (@crap){
	chomp $_;
	if($_=~/^COUL:/){
	    ($crap,$crap,$crap,$crap,$crap,$ace_output)=split /\s+/, $_;
	    last;
	}
    }
    print ENERGY_OUTPUT "\t$elec_output\t$vdw_output\t$ace_output\n";

    unlink ("$deltag_output_filename");
    unlink ("$energy_output_file_name"."_orientedL.pdb");
    unlink ("$energy_output_file_name"."_orientedR.pdb");
    unlink ("$receptor_chain1_second_charmm_input");
    unlink ("$receptor_chain2_second_charmm_input");
    unlink ("$receptor_chain1_first_minimization_output");
    unlink ("$receptor_chain2_first_minimization_output");
    unlink ("$ligand_chain1_second_charmm_input");
    unlink ("$ligand_chain2_second_charmm_input");
    unlink ("$ligand_chain1_first_minimization_output");
    unlink ("$ligand_chain2_first_minimization_output");
    unlink ("$energy_output_first_run");
    unlink ("$energy_output_second_run");
    unlink ("$junk");
    unlink ("$receptor_chain1_second_minimization_output");
    unlink ("$receptor_chain2_second_minimization_output");	
    unlink ("$ligand_chain1_second_minimization_output");
    unlink ("$ligand_chain2_second_minimization_output");	
    if($keep_output eq "none"){
	unlink ("$receptor_minimized_first");
	unlink ("$receptor_minimized_second");
	unlink ("$ligand_minimized_first");
	unlink ("$ligand_minimized_second");
    }
    else{
	system "mv $receptor_minimized_first $receptor_minimized_first.$i";
	system "mv $receptor_minimized_second $receptor_minimized_second.$i";
	system "mv $ligand_minimized_first $ligand_minimized_first.$i";
	system "mv $ligand_minimized_second $ligand_minimized_second.$i";
    }
}
unlink ($receptor_chain1_template_of_receptors);
unlink ($receptor_chain2_template_of_receptors);
unlink ($ligand_chain1_template_of_ligands);
unlink ($ligand_chain2_template_of_ligands);
unlink ($ligand_chain1_temp_rotated);
unlink ($ligand_chain2_temp_rotated);
unlink ($charmm_input_first_run);
unlink ($charmm_input_second_run);

sub read_arguments{
    my @argv=@_;
    my $zdock_output_file_name="";
    open CRAP, "pwd |";
    @crap=<CRAP>;
    chomp $crap[0];
    my $output_directory=$crap[0]."/";
    my $energy_output_file_name="?";
    my $keep_output="none";
    my $xfrom=1;
    my $xto=1;
    for my $v1 ( 0 .. $#argv){
        if($argv[$v1] eq "-i"){
            $zdock_output_file_name=$argv[$v1+1];
        }
	elsif($argv[$v1] eq "-o"){
            $energy_output_file_name=$argv[$v1+1];
        }
        elsif($argv[$v1] eq "-d"){
            $output_directory=$argv[$v1+1];
	    if($output_directory!~/\/$/){$output_directory.="/";}
        }
        elsif($argv[$v1] eq "-keep"){
            $keep_output="all";
        }
        elsif($argv[$v1] eq "-x"){
            $xfrom=$argv[$v1+1]*1;
            $xto=$argv[$v1+2]*1;
	    if($xfrom>$xto){print STDERR "ERROR: x_from is greater than x_to. Program exited.\n\n"; exit;}
        }
        else{
            if($argv[$v1] =~ /^-/){
                print "Wrong option(s)\n";
            }
        }
    }
    if($energy_output_file_name eq "?"){
	$energy_output_file_name=$zdock_output_file_name.".rdockoutput";
    }
    return ($zdock_output_file_name,$energy_output_file_name,$output_directory,$xfrom,$xto,$keep_output);
}

