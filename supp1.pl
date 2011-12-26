#!/usr/bin/perl -w

# Function: Construct builds representing genomic repeats from random sample sequence reads.
# Algorithm: Find a sequence read that has many hits with high identity, by BLAST. Then in both 
#directions, use hits to build a multiple sequence alignment, then use the product of the Multiple 
#Alignment to "walk" out the repeat family.
#License: GNU Lesser Public License (http://www.gnu.org/copyleft/lesser.html)
# Authors: Jeremy DeBarry and Renyi Liu 

#-------+
# USAGE |
#-------+
use strict;
use Bio::SeqIO;
use Bio::Seq;
use Bio::Tools::Run::Alignment::Clustalw;
use Bio::Tools::Run::Alignment::TCoffee;
use Bio::SimpleAlign;
use Bio::Tools::Run::StandAloneBlast;
use Bio::SearchIO;
use Log::Log4perl;

#-------------------------------------------------------+
#LOGGER INITITATION AND VARIABLES FOR VARIOUS FUNCTIONS |
#-------------------------------------------------------+	
#Variable that stores the logger level
my $AAARF_Log_Level = 'DEBUG';
#Instructions for log file production
my $AAARF_Log = 'log4perl.logger='. $AAARF_Log_Level.', A1
                 log4perl.appender.A1=Log::Log4perl::Appender::File
                 log4perl.appender.A1.filename=AAARF_log
                 log4perl.appender.A1.mode=append
                 log4perl.appender.A1.layout=Log::Log4perl::Layout::PatternLayout
                 log4perl.appender.A1.layout.ConversionPattern=%p-%m%n';
#These two lines initiate the logger. See logger documentation for more information.
Log::Log4perl::init(\$AAARF_Log);
my $logger = Log::Log4perl->get_logger();
#Hashes used to evaluate which seqs have been used by the program
my %usedSeqs;  my %usedSeqs_int; my %usedSeqs_Three; 
#Used to report sequences that were used for particular builds
my @buildseqs; my $element; my $buildSeqs = "Seqs_Used_In_Build";my $Sequence_Counter; 
#Used to make sure that the build size doesn't decrease during a build
my $last_build_size; 
#Used to report the extracted subsequences during NQ construction
my $extracted_seq_left; my $extracted_seq_right; 
#used to ensure that identical sample sequence sections are not used in the build. This can result in tandem duplications at build  ends
my %tandem_guard;
#Used to report total number of seqs read in and that the process is done. This may not be necessary. I will check later.
my $quiet = 0; 
#Declaring the input file variable
my $inputFile; 
#Declaring the output file variable
my $outputFile; 
#Variable for naming the builds. Used in sub that names the builds. I may change this name to something more appropriate.
my $consensusSeqNamePre = "New_Query_Sequence_";	
#Variable for build counting. Used in sub that names the builds
my $consensusSerialNum = 0;	

#-----------------------------------------------------------------------+
#PROGRAM PARAMETERS (MCS CONSTRUCTION---EXTENSION---MULTIPLE ALIGNMENT) |
#-----------------------------------------------------------------------+
#--------------------------------------+
#BLAST AND MCS CONSTRUCTION PARAMETERS |
#--------------------------------------+
my $maxBlastHits = 100;       # maximum number of hits used to construct coverage matrix
my $minBlastMatch = 150;      # minimum length for a BLAST hit
my $minBlastIdentity = 0.89;  # minimum identity for a BLAST hit
my $minBlastCoverDepth = 2;   # minimum coverage depth for MCS 
my $minBlastConsenLen = 150;  # minimum length for MCS
my $BLAST_e = '1e-25';        # maximum evalue for BLAST hit (replace value inside quotes)
my $BL2SEQ_e = '1e-10';       # maximum evalue for BL2SEQ hit (replace value inside quotes)

#---------------------+
#EXTENSION PARAMETERS |
#---------------------+
my $maxExtendHits = 1000;      # maximum number of BLAST hits used to extend build
my $minExtendHits = 1;         # minmum number of BLAST hits used to extend build, must be >= 1
my $maxExtendLen = 50;         # maximum extension length (step size)
my $minExtendLen = 0;          # minimumal extension length 
my $minCoverLen = 150;         # Controls: 1) required size of overlap between MCS and NQ 
                               # 2) minimum coverage for extension (overlap between sample sequences 
                               # and MCS during NQ construction) 3) NQ must be at least this long
my $minOverlapLen = 90;        # minimum required overlap between MCS and New Query Sequence for BL2SEQ,
                               # based on 60% of minCoverLen
my $times_used = 13;           # Maximum number of times that a sequence is used in each direction

#-----------------------------+
#MULTIPLE ALIGNMENT PARAMETERS|
#-----------------------------+
my $alignmentThresholdPercentage = 0.8;  # DO NOT ALTER!! This is the required percent identity between 
										 #each column in multiple alignment. Currently not in effective use.  
                                         # The 0.8 value is an actual 0.8 and not 80%. This will be updated.
                                         
#----------------------------------------------+
#FILEPATH TO BLAST DATABASE OF INPUT SEQUENCES |                                      
#----------------------------------------------+
my $database = "/Users/Jeremy/Programs/repeatWalk/Maize_Shotgun/TEST_13/BLAST_DB/ZU_1000_db"; 
my $allSeqFile = "/Users/Jeremy/Programs/repeatWalk/Maize_Shotgun/TEST_13/BLAST_DB/ZU_1000_db";

#----------------+               
#AAARF MAIN BODY |
#----------------+
#Reading in the sequence input file and preparing output file based on user input
if(@ARGV > 0){
$inputFile = $ARGV[0];
	}else{
	die "USAGE: perl AAARF.pl <input file> [<output file>]";
     	 }#close else
if(@ARGV > 1){
	$outputFile = $ARGV[1];
#If no output file name is given, AAARF makes its own based on the name of the input file	
	}else{
	#split the input file name into an array up using "." as a delimiter
	my @temp = split /\./, $inputFile;
	my $exten = "";
		#If the split produces more than one take the last one
		if(@temp > 1){
		$exten = pop @temp;
       		         }
    #outfile name is concatenated @temp contents plus "_AAARF." plus contents of $exten
	$outputFile = join(".", @temp) . "_AAARF.". $exten;
          }#close else 

#formatting input file for program use      
my $seqin = Bio::SeqIO->new(-format=>'fasta', -file=>$inputFile); 
#formatting output file for program use
my $seqout = Bio::SeqIO->new(-format=>'fasta', -file=>">$outputFile"); 
#formatting output file for collection of sequences used in multiple alignments
#my $buildseqout = Bio::SeqIO->new(-format=>'fasta',-file=>">$buildSeqs"); #deactivated until update JD 6_1_07


#read in all sequences and store in a hash
my $allSeqIn = Bio::SeqIO->new(-format=>'fasta', -file=>$allSeqFile);
my %allSeqs;
while(my $seq = $allSeqIn->next_seq()){
	$allSeqs{$seq->id()} = $seq;
                                       }
#Report number of sequences read into AAARF
if($logger->is_fatal()){
	$logger->fatal("Number of input sequences: ", scalar(keys(%allSeqs)));
                        }

#Initializong a variable to number each input sequence. This will be used to keep track of the sequences throughout the process
my $numSeqs = 0; 
#Beginning build process, repeat with each sequence in input file
while(my $seq = $seqin->next_seq()){
	$numSeqs++;
	
	#Before a sequence is used the hash %usedSeqs is checked to make sure that it hasn't been used in another build
	if (exists $usedSeqs{$seq->id()}){ 
		if($logger->is_error()){
			$logger->error("Sequence Number $numSeqs: ", $seq->id(),", not used to initiate build because it has already been used.");		
			                    }
	        	next;
	                                  }
	#Report the sequence that is being used to the terminal window
	if(!$quiet){
		print "Build initiated with sequence #", $numSeqs, " ", $seq->id(), "\n"; 
	           }
	           
	#clears %usedSeqs_int for use in the next build, this is where seq ids that have been used in a build are stored during the build           
	%usedSeqs_int = ();	
	#This adds initial query to the intermediate restriction hash. It will only be added to the restricted hash '%used_seqs' if the build is 		
	#successful
	$usedSeqs_int{$seq->id()} = $seq;   
	#The next two lines clear and array and initialize a counter used to produce "SEQS_USED_IN_BUILD" File	
	@buildseqs = ();#
	$Sequence_Counter = 0;
	#Variable for storage of build sequence during build
	my $consensus = "";
	#Maximum steps allowed in either direction. UPDATE:This may not be necessary now that I control this in a different way
	my $maxNumIteration = 1000; 
	#Search Directions
	my @dirs = ("right","left");

	foreach my $dir (@dirs){
		#This is for tracking the number of times a sequence is used within the build.
   		%usedSeqs_Three =(); 
   		#Used to guard against tandem repeats on build ends. Hash is cleared at start of each directional extension
   		%tandem_guard = ();
   		#Tracks number of iterations for each directional build
		my $it = 0;
		#Controls build stops
		my $stopNow = 0;
		#Current initializing sequence
		my $nextQuery = $seq;
		
			while(!$stopNow && $it < $maxNumIteration){	
  				($nextQuery, $stopNow, $consensus , $last_build_size) = One_Step($nextQuery, $consensus, $dir);
  				$it++;
 				my $Current_build_size = length($consensus); 
 				
 					#Check to make sure that build size doesn't decrease from one step to the next
  					if ($Current_build_size < $last_build_size){ 
						if($logger->is_debug()){
							$logger->debug("ALERT! The build size has decreased. This should not happen and may indicate a problem with this build." , "The previous build size was: " , $last_build_size , " The current build size is: " , $Current_build_size , "\n\n");		
			                                   }
			                                                    }#if ($Current_build_size < $last_build_size)
			            #Report iteration, length of build, and the sequence of the build so far                                        }
  						if($logger->is_fatal()){
							$logger->fatal("Build Progress So Far: --- Direction: ",$dir," --- Iterations: $it", " --- Current Build length: ", length($consensus));
							if ($consensus){
							$logger->warn("Current Build: ", $consensus);
										   }
							#Report the New Query id, length and sequence
								if(defined $nextQuery){
           						$logger->fatal("New Query Sequence:"," --- Id: ", $nextQuery->id(), " --- Length: ", $nextQuery->length());
	  						    $logger->warn("New query sequence: ", $nextQuery->seq());
                                                      }#if(defined $nextQuery)
                                                }#if($logger->is_debug())
                                                       }#while(!$stopNow && $it < $maxNumIteration)
	}#foreach my $dir (@dirs){
#-----------------------------------------------------------------+
#SINGLE BUILD COMPLETED AS FAR AS POSSIBLE---EXIT ALL SUBROUTINES |
#-----------------------------------------------------------------+
#Now that the build has progressed as far as possible, check size of the final build and report the length and sequence
if(length($consensus) > $seq->length()){
my $outSeq = Bio::Seq->new(-id=>"Build_With_Start_Clone_".$seq->id()."_Length_".length($consensus) ,  -seq=>$consensus);
#Report that the build has finished
if($logger->is_fatal()){
	$logger->fatal("This Build Cannot Progress Any Further Using This Parameter Set");}
#Write the build to the output file
$seqout->write_seq($outSeq);

#following 5 lines output the sequences used in multiple alignments to the "Seqs_Used_In_Build" file (deactivated until update JD 6_1_07)
#foreach $element (@buildseqs){
#$Sequence_Counter ++;
#my $buildoutseq = Bio::Seq->new(-id=>"Sequence_used_in_build_with_start_clone_".$seq->id()."_number:".$Sequence_Counter, -seq=>$element);
#$buildseqout->write_seq($buildoutseq);
#                              }

#This pushes everything in usedSeqs_int into %usedSeqs. This only happens if the build was successful
%usedSeqs = (%usedSeqs,%usedSeqs_int);
			
my @keys = keys %usedSeqs; 
my @keysint = keys %usedSeqs_int;

#Report all sequence used in build to logger file
if($logger->is_info()){
		foreach my $keysint(@keysint) {
		$logger->info("Sequence " , $keysint,", added to used sequences list because it was used to construct the build above.");
                                      }
                       }
#breaks up logger file between builds if build was successful
if($logger->is_fatal()){
	$logger->fatal("
	
******************************************************************
	");
	                    }
#breaks up logger file between builds if build was not successful
	}else{#if(length($consensus) > $seq->length())
	$logger->fatal("Attempted build with sequence ", $seq->id(), " had no good result. Build stopped because final build is smaller than the sequence that initiated the build. This means that no useful build was constructed.

******************************************************************
	");
         }#close else
}#while(my $seq = $seqin->next_seq()){
                                        
                                        
#Terminal output after AAARF has finished
if(!$quiet){
	print "AAARF process completed with total number of sequence: $numSeqs\n";
            }

#------------------+
#AAARF SUBROUTINES |
#------------------+

#--------------+
#WALK_ONE_STEP |
#--------------+
#Function: This main subroutine feeds data into two main program subs (Build_BLAST_Consensus and Build_New_Query)
#and processes each build step to extend build and report results to the user
# input : The query seq object, current consensus sequence, search direction
# output: next blast query sequence, should stop, new consensus sequence
sub One_Step{
#Reports to logger that the program has entered this sub
my $logger = Log::Log4perl->get_logger("One_Step"); 
#Inputs into sub
my ($qSeq, $consen, $direction) = @_;  
#Variables that the sub returns (except for $last_build_size, that is declared at the start of the program)
my ($nextSeq, $stop, $newConsen); 
#Variables for subsequence coordinate extraction and HSP screen   	
my $bl2seqHsp; my $bl2seqSStart; my $bl2seqSEnd; my $bl2seqQStart; my $bl2seqQEnd;
#Gives $last_build_size a value for comparison in main body in case sub doesn't get that far
$last_build_size = 0;

#Check to make sure that a seqrch direction is passed to the sub	
if(!($direction =~ /left/i || $direction =~ /right/i)){
	die "Search direction must be \'left\' or \'right\'";
	                                                   }
#Report that program is in this sub and give current build length and direction	
if($logger->is_fatal()){
	$logger->fatal("Starting a new round of extension --- Current Build Length: ", length($consen), " --- Current Search Direction: $direction");
	                   }
#BLAST sequence against all other sequences
my $blastOut = getBlastOutput($qSeq, $database);
#Generates second BLAST report for use in BNQ. Fixes the problem of the program not using the first ten hits anywhere but BBC
my $blastOuttest = getBlastOutput($qSeq, $database);

#If there are no results in the BLAST, report it, exit sub and stop build
if(!$blastOut){
	logStop ($logger, $direction, "there was no BLAST Report generated.");
	$stop = 1;
	return ($nextSeq, $stop, $consen, $last_build_size);
	          }

#Load BLAST reports into variables	          
my $res = $blastOut->next_result();	
my $test = $blastOuttest->next_result(); 

#If there is no "Next Result" in the BLAST output, report it, exit sub and stop build
if(!$res){
	logStop ($logger, $direction, "there was no 'Next Result' in the BLAST Report.");
	$stop = 1;
	return ($nextSeq, $stop, $consen, $last_build_size);
         }

#first: build MCS from BLAST result using Build_Blast_Consensus sub
my ($blastConsen, $coverStart, $coverEnd) = MCS_Search($qSeq, $res);
        
#Report the size and sequence of the MCS
if ($blastConsen){
	if($logger->is_warn()){
	#$logger->debug("Length of MCS: " , length($blastConsen));
	$logger->warn("MCS: $blastConsen");
	                        }
	              }          
	
#If there is no MCS or if it is too small, report it, exit sub and stop build	
if(!defined $blastConsen || length($blastConsen) < $minBlastConsenLen ||
		$coverEnd - $coverStart < $minBlastConsenLen){
		$stop = 1;
		logStop ($logger, $direction, "MCS doesn't exist or is too short.");
		return ($nextSeq, $stop, $consen, $last_build_size);
	                                                  }
# next: select sequences that cover and extend in the search direction and construct a new query sequence using the Build_New_Query sub
($nextSeq, $stop) = Build_New_Query($qSeq, $test, $direction, $coverStart, $coverEnd);

#I need to check these lines out, is there another stop message if the BNQ is not successful?
#	if($stop){
#		return ($nextSeq, $stop, $consen);
#	}

#If this is the first iteration use the MCS as the current build and report to logger
if(!$consen){
	$consen = $blastConsen;
	if($logger->is_debug()){
		$logger->debug("The MCS has been assigned as the entire current build. This should only happen on the first step to the right.");
	                       }
	         }
	
#For any iteration except the first assign current build to this variable
$newConsen = $consen; 

#check the current build size against the last build size to make sure that the size has not decreased. This would indicate a build error.
$last_build_size = length($newConsen);

# run a bl2seq to locate the overlap region between the Current MCS and the NQ 
if(defined $nextSeq){  
	my $blastConsenObj = Bio::Seq->new(-id=>'Current_MCS', -seq=>$blastConsen);
	my @param = (program=>'blastn',e=> $BL2SEQ_e, F=>'F' , o=>'bl2seq_out');
	my $factory = Bio::Tools::Run::StandAloneBlast->new(@param);
	my $bl2seqOut = $factory->bl2seq($blastConsenObj, $nextSeq);

       #Reports the BL2SEQ result into a file that overwrites itself each time a BL2SEQ is run. This is used to report the BL2SEQ to the log file. 
		my $BL2SEQ_variable = 'bl2seq_out';
		open (FILE , $BL2SEQ_variable);
	    my @bl2seq_outfile = <FILE>;
	    if($logger->is_debug()){
			$logger->debug("BL2SEQ_OUTFILE: " , @bl2seq_outfile);}
        
        #If there is no result from the BL2SEQ...
		my $bl2seqRes = $bl2seqOut->Bio::SearchIO::blast::next_result();
		if($bl2seqRes->num_hits() < 1){
			$stop = 1;
		        logStop($logger, $direction, "there was no hit between MCS and the New Query Sequence. This shouldn't happen. Examine BL2SEQ parameters.");	
			return ($nextSeq, $stop, $newConsen, $last_build_size);
		                              }
		
		#Information from the BL2SEQ is stored in these variables
		my $bl2seqHit = $bl2seqRes->next_hit();
		my $bl2seqQLen = $bl2seqRes->query_length();
	    my $bl2seqHsp = $bl2seqHit->next_hsp();
		my ($bl2seqQStart, $bl2seqQEnd) = $bl2seqHsp->range('query');
	    my ($bl2seqSStart, $bl2seqSEnd) = $bl2seqHsp->range('hit');
	        
	    #For the left direction, check for overlap
	    if ($direction =~ /left/i){
	    #Evaluate position of the HSPs in the BL2SEQ 
	    	while (my $bl2seqHsp2 = $bl2seqHit->next_hsp()){
	    		#HSP ends should be within 15 bps of the sequence ends (arbitraty value here, if it is not within this range, no overlap
	    		if (($bl2seqSStart < 15) || ($bl2seqQStart > 15)){
	    			#If this position test fails load coordinated for next HSP and check again
	        		($bl2seqQStart, $bl2seqQEnd) =   $bl2seqHsp2->range('query');
	        		($bl2seqSStart, $bl2seqSEnd) = $bl2seqHsp2->range('hit');
	        		#replace the old HSP with the new one in the event of a pass over
	        		($bl2seqHsp = $bl2seqHsp2); 
	        			#Report that the initial HSP was rejected
	        			if($logger->is_debug()){
			             	$logger->debug("The first HSP in the BL2SEQ file has been skipped.");
			                                    }#if($logger->is_debug())
			                                                      }#if (($bl2seqSStart < 15) || ($bl2seqQStart > 15))
			                                                }#while (my $bl2seqHsp2 = $bl2seqHit->next_hsp())
			                         }#if ($direction =~ /left/i)
		#For the right direction, check for overlap	
		if($direction =~ /right/i){
			#Evaluate position of the HSPs in the BL2SEQ 
			while (my $bl2seqHsp2 = $bl2seqHit->next_hsp()){
				#HSP ends should be within 15 bps of the sequence ends
				if ($bl2seqSEnd > ($bl2seqHit->length()-15) || $bl2seqQEnd < ($bl2seqQLen - 15)){
					#If this position test fails load coordinated for next HSP and check again
					($bl2seqQStart, $bl2seqQEnd) = $bl2seqHsp2->range('query');
					($bl2seqSStart, $bl2seqSEnd) = $bl2seqHsp2->range('hit');
					#replace the old HSP with the new one in the event of a pass over
					($bl2seqHsp = $bl2seqHsp2); 
	        			if($logger->is_debug()){
		            		$logger->debug("The first HSP in the BL2SEQ file has been skipped.");
	        		                            }#if($logger->is_debug())
	        		                                                                             }#$bl2seqQEnd < ($bl2seqQLen - 15))
	        		                                        }#while (my $bl2seqHsp2 = $bl2seqHit->next_hsp())
	        		                 }#if($direction =~ /right/i)
	        
		#Report information from BL2SEQ
		if($logger->is_error()){
			$logger->error("BL2SEQ Results: ", "---Query_name=", $bl2seqRes->query_name(),"---Hit_name=",$bl2seqHit->name(), "---Hsp_length=",$bl2seqHsp->length(),"---Query_range=(", $bl2seqQStart, ",", $bl2seqQEnd, ")---Subject_range=(",$bl2seqSStart, ",", $bl2seqSEnd, ")");
		                       }
		                            
		#Check the size of the overlap between the build and the NQ                       
		if($bl2seqHsp->length('total') < $minOverlapLen){
			$stop = 1;
			logStop($logger, $direction,"the hit between current build and New Query is not big enough. Size: " . $bl2seqHsp->length());
			return ($nextSeq, $stop, $newConsen);
			}else{#if($bl2seqHsp->length('total') < minOverlapLen)
		    
		#Concatenation step: If the overlap is large enough attach the NQ to the current build, (direction specific)
		if($direction =~ /left/i){
			#attach NQ to current build (left direction)
			$newConsen = $nextSeq->subseq(1, $bl2seqSStart) . substr($consen, $bl2seqQStart);
				#report concatenation specifics				
				if($logger->is_debug()){
					$logger->debug("This is the concatenation step for the left direction. This adds the extending sequence to the current build. The section of the New Query Sequence from position 1 to the position corresponding to the Subject Start: " ,$bl2seqSStart," is added to the section of the current build corresponding to the region from the Query Start: ",$bl2seqQStart," to the end of current build." );
                                            }
			}else{#if($direction =~ /left/i)
			#attach NQ to current build (right direction)					  
			$newConsen = substr($consen, 0, length($consen) - $bl2seqQLen + $bl2seqQEnd) . $nextSeq->subseq($bl2seqSEnd, $nextSeq->length());
				#report concatenation specifics				
				if($logger->is_debug()){
					$logger->debug("This is the concatenation step for the right direction. This adds the extending sequence to the current build. The section of the New Query ranging from the Subject End, ",$bl2seqSEnd,", to the end of the New Query is added to the right side of the current build. Calculation for concatenation point on current build: --- length of current build: ",length($consen),", minus the Query Length: ",$bl2seqQLen,", plus Query End: ",$bl2seqQEnd," or, (",length($consen),"-",$bl2seqQLen,"+",$bl2seqQEnd , "=>",length($consen) - $bl2seqQLen + $bl2seqQEnd,")");
                                        }#if($logger->is_debug()){				          
		            }#Closing the 'else' statement for the left direction concatenation
		            }#Closing the 'else' statement for the overlap length check
	#If the BL2SEQ was not successful	
		}else{#if(defined $nextSeq){
			logStop ($logger, $direction, "there was no New Query Sequence produced from the Build_New_Query sub.");
              }
	return ($nextSeq, $stop, $newConsen, $last_build_size); #return to program main body
}#Close sub "One_Step"

#--------+
#LOGSTOP |
#--------+
# Function: logs any reason for build termination
# input: ref to logger, search direction, stop message from AAARF
# output: directly to diagnostic log
sub logStop{
	my ($logger, $direct, $mes) = @_;
	if($logger->is_fatal()){
		$logger->fatal("Walking at direction ", $direct, " stopped because $mes");
	                        }
           }

#----------------+
#BUILD_NEW_QUERY |
#----------------+
# Given blast output, use hit coordinates and a multiple alignment to build a new query sequence according to search direction
# input: ref to query seq, ref to blast result, search direction, start and end position of query consensus
# output: new query sequence (object), flag indicating whether to stop or continue
sub Build_New_Query{
#Input into sub
#q0 is cover start and q1 is cover end from Build_BLAST_consensus sub
my ($qSeq, $res, $direct, $q0, $q1) = @_; 
#Report that the program has entered this sub	
if($logger->is_fatal()){
	$logger->fatal("Constructing a New Query Sequence");		
			           }
#array to hold hits that overlap and extend
my @hits; 
#Hash that stores the names and sequences that have been used to extend. They will only be passed to %usedSeqs_int if the M.A. is successful.
my %usedSeqs_Two = (); 
#This is used to make sure that Seqs are only used a certain number of times in a given direction
my $Seq_Counter; 
#Set the stop flag to 0 to begin
my $shouldStop = 0; 

#first get hits that cover part of the previous consensus, and part at search direction

#For each hit in the BLAST report (until the maximum alowed number of hits is reached)
while((my $hit = $res->next_hit()) && @hits < $maxExtendHits){
#binary switch that is turned on if the sequence being considered for extension is in opposite orientation to the MCS. If the switch is activated there are an alternative set of collection methods used. Reset at each pass through the sub.
my $oppo = 0; 
	  
	#Report the binary oppo switch state. Currently off, remove comment markers to activate
	#@if($logger->is_debug()){
	#$logger->debug("At the beginning of BNQ the Opposite Orienatation Flag is : $oppo ") ;		
	#		               }
	
	#Report sequence name as it is evaluated
	if($logger->is_error()){
	$logger->error("Evaluating HSPs for Sequence: ",$hit->name()) ;		
	   		                }
	
	#Check to make sure that the query name and subject name are not the same, if they are report it and move to the next sequence for extension
	if($hit->name() eq $res->query_name()){
		if($logger->is_error()){
			$logger->error($hit->name()," skipped because the Query ID is the same as the Subject ID") ;		
			                   }
			next;	
			                              }
		
	#Check that the sequence being considered has not been used in a previous build, if it has report it and move to the next sequence for extension
	if (exists $usedSeqs{$hit->name()}){
		if($logger->is_error()){
			$logger->error($hit->name(),", not used for extension because it has already been used.");		
			                    }
			next;
	                                    }
	
	#Check that the sequence being considered has not been used the maximum number of allowed times, if it has report it and move to the next sequence for extension

	if ($usedSeqs_Three{$hit->name()}){
		if (my $Number_times_used = $usedSeqs_Three{$hit->name()} >= $times_used){
			if($logger->is_error()){
				$logger->error($hit->name()," not used for extension because it has already been used " ,$times_used , " times, and that is all that is allowed in one direction.");		
				                   }
				next;
	        	                                                                  }
	                                  }   
	                                          
	#For each HSP between the subject and the query... 
	while (my $hsp = $hit->next_hsp()){		
		#If there is no HSP report it and move on
		if(!defined $hsp){
			if($logger->is_error()){
				$logger->error("There is no hsp in this hit. This shouldn't happen.");		
			                       }
		    	next;	
						}
		#If an HSP from this hit has already been used, report it and move on to next hit
		if (exists $usedSeqs_Two{$hit->name()}){
			if($logger->is_error()){
				$logger->error($hit->name()," not used for extension because an HSP from this sequence has already been used for this round of extension.");		
			                       }
				next;
	                                            }
		#Checking to see that the hits meet the requirments. If not it will tell me about it.
		if($hsp->length('total') < $minBlastMatch || $hsp->frac_identical() < $minBlastIdentity){
			if($logger->is_error()){
				$logger->error($hit->name()," did not satisfy the hit requirments for extension. Hit Length: ",$hsp->length('total')," Identity Value: ",$hsp->frac_identical());
			                       }
				next; 
	                                                                                            } 
	    #Collect hit statistics
		if($hsp->length('total') >= $minBlastMatch && $hsp->frac_identical() >= $minBlastIdentity){
			my ($qStart, $qEnd) = $hsp->range('query');
			my ($sStart, $sEnd) = $hsp->range('hit');
			my $qStrand = $hsp->strand('query');
			my $sStrand = $hsp->strand('hit');
				#Report hit information. Inactive now. Remove Comment Markers to Activate
				#if($logger->is_debug()){
				#	$logger->debug(" Hit information before test for strandedness."," Query Name: ",$res->query_name()," Query Start: ",$qStart," Query End: ",$qEnd," Subject Name: ",$hit->name()," Subject Start: ",$sStart,"  Subject End: ",$sEnd," Query Strand: ",$qStrand," Subject Strand: ",$sStrand," Direction: ",$direct);		
			     #                       }
			
			#This checks the hash to see if the subject start and stop coordinates are the same as any other step. If they are then this indicates a tandem duplication at the ends of the build and the sequence is not used.
			if (exists $tandem_guard{$hit->name()}){
				if ($sStart.$sEnd == $tandem_guard{$hit->name()}){
					if($logger->is_error()){
						$logger->error($hit->name(),", not used for extension because the subject start and stop are the same that that were used for this sequence in the last round of extension. The start and stop coordinates of this subject for this step are: " , $sStart , " and " , $sEnd , "." , " The concatenated start and stop for this sequence in the last step is: " , $tandem_guard{$hit->name()});		
				next;
	                                        }                         
	                                                             }#if ($sStart.$sEnd == $tandem_guard{$hit->name()}){
	                                                }#if (exists $tandem_guard{$hit->name()})
			
	#variablecontains coverage length (initially 0)
	my $coverLen = 0; 
	#variable contains extension length (initially 0)
	my $extendLen = 0; 
			
			#Evaluate hits based on ability to overlap and extend build process. Evaluation is direction and strand specific
			if($direct =~ /left/i){
				if($qStart > $q0 || $qEnd < $q0){ # hit must cover the end region
					if($logger->is_error()){
						$logger->error($hit->name(), " ,did not cover the end of the MCS in the search direction: ", $direct,"\n"," Query Start: ",$qStart," Query End: ",$qEnd," MCS Coverage Start: ",$q0," MCS Coverage Stop: ",$q1);		
			                                }
						next;
						                         }
			#left direction: calculate coverage length and store in variable                                 }
			$coverLen = $qEnd - $q0;
				
			}elsif($direct =~ /right/i){
				if($qEnd < $q1 || $qStart > $q1){
					if($logger->is_error()){
						$logger->error($hit->name(), ", did not cover the end of the MCS in the search direction: ", $direct," --- Query Start: ",$qStart," --- Query End: ",$qEnd," --- MCS Coverage Start: ",$q0," --- MCS Coverage Stop: ",$q1);		
			                                }
						next;
				                                }
				#left direction: calculate coverage length and store in variable                                 }
				$coverLen = $q1 - $qStart;
			                             }#}elsif($direct =~ /right/i){ ##for coverage length calculation
			                             
			#Make sure that coverage length is big enough                             
			if($coverLen < $minCoverLen){ 
				if($logger->is_error()){
					$logger->error($hit->name(),", did not cover the Query enough to be used for extension. Coverage: " , $coverLen);		
			                            }
					next;
			                            }
			                          
			#get name of subject sequence from input file hash                          
			my $hitSeq = $allSeqs{$hit->name()}; 
			
			# if the strand of the subject doesn't match the query (I assume query is always 1 here)
			if($sStrand != 1){ 
				if($logger->is_error()){
					$logger->error($hit->name(),", is in opposite orientation.");		
			                            }
			        #set orientation flag to 1
				    $oppo = 1;
			                  }#if($sStrand != 1)
			                  
			# this the point of q0 (for left direction) or q1 (for right direction) mapped to the hit sequence  
			my $sMiddle = 0; 
			#the start and end position of hit sequence to extract
			my ($subStart, $subEnd); 
			
		    if($direct =~ /left/i){
		    	if($oppo){
		        		$sMiddle = $sEnd - ($q0 - $qStart);
		        		$extendLen = $hitSeq->length() - $sMiddle;
		        		if($logger->is_error()){
							$logger->error("Opposite Orientation calculation for sMiddle: ",$sEnd ,  "-" ,  "(",$q0 ,  "-" ,  $qStart,")");		
			                                    }
				}else{#if oppo flag was 0
					$sMiddle = $sStart + ($q0 - $qStart);
					$extendLen = $sMiddle - 1;
						if($logger->is_error()){
							$logger->error("Same Strand Calculation for sMiddle: ",  $sStart ,  "+" ,  "(",$q0 ,  "-" ,  $qStart,")");		
			                                    }
			         }#closing the left direction $oppo flag for #sMiddle calculation
			}else{#If direction is to the right
				if($oppo){
		        		$sMiddle = $sStart + ($qEnd - $q1);
		        		$extendLen = $sMiddle - 1;
		        			if($logger->is_error()){
								$logger->error("Opposite Orientation calculation for sMiddle: ",  $sStart ,  "+" ,  "(",$qEnd ,  "-" ,  $q1,")");		
			                                        }
				}else{#if oppo flag was 0
				$sMiddle = $sEnd - ($qEnd - $q1);
				$extendLen = $hitSeq->length() - $sMiddle;#hitseq is the length of the whole subject sequence
					if($logger->is_error()){
						$logger->error("Same Strand Calculation for sMiddle: ",  $sEnd ,  "-" ,  "(",$qEnd ,  "-" ,  $q1,")");		
			                                }
			         }#Closing the right direction $oppo flag for $sMiddle calculation
			       }#Closing the right direction $sMiddle calculation
			
			#Report HSP stats after $sMiddle Calculation
			if($logger->is_error()){
				$logger->error( "Hit Evaluation Summary --- Query Name: ",$qSeq->id()," --- Query Length: ", $qSeq->length()," --- Query Start: " , $qStart , " --- Query End: " , $qEnd," --- Query Strandedness: ",$hsp->strand('query')," --- Subject Name: ",$hit->name()," --- Subject Length: ",$hitSeq->length()," --- Subject Start: ",$sStart," --- Subject End: ",$sEnd," --- Subject Strandedness: ",$hsp->strand('hit')," --- sMiddle: " , $sMiddle , " --- Extend Length: " , $extendLen," --- MCS Coverage Start: ",$q0," --- MCS Coverage Stop: ",$q1);	
			                         }
			
			#If the sequence cannot extend enough skip it
			if($extendLen < $minExtendLen){
				if($logger->is_error()){
					$logger->error($hit->name(),", did not extend enough to be used for extension. Extension:" , $extendLen);
			                            }
				    next;
			                               }
			
			#adds sequence name and start and stop coordinates to the tandem_guard hash
			$tandem_guard{$hit->name()} = $sStart.$sEnd;
			
			#collection of subsequences for multiple alignment (based on direction and strandedness)
			if($direct =~ /left/i){
				if ($oppo) {
				$subEnd	 = $hitSeq->length() - $sMiddle > $maxExtendLen ? ($sMiddle + $maxExtendLen) : $hitSeq->length();
				$subStart = $sMiddle > $minCoverLen ? $sMiddle - $minCoverLen : 1;
				$extracted_seq_left = $hitSeq->trunc($subStart, $subEnd);
					if($logger->is_warn()){
						$logger->warn("Extraction points for subsequence (opposite orientation) --- Start: " , $subStart , " --- End: " , $subEnd) 
                                            }
					if($logger->is_warn()){
						$logger->warn("Extracted Subsequence: " , $extracted_seq_left -> seq() );
			                                }
				}else{#if oppo flag is 0
				$subStart = $sMiddle > $maxExtendLen ? ($sMiddle - $maxExtendLen+1) : 1;
				$subEnd = ($hitSeq->length() - $sMiddle) > $minCoverLen ? ($sMiddle + $minCoverLen) : $hitSeq->length();
				$extracted_seq_left = $hitSeq->trunc($subStart, $subEnd);
					if($logger->is_warn()){
						$logger->warn("Extraction points for subsequence-> Start: " , $subStart , " --- End: " , $subEnd)
                                            }
					if($logger->is_warn()){
						$logger->warn("Extracted Subsequence: " , $extracted_seq_left -> seq() );
			                                }
			           }#Closing left direction $oppo check for subsequence collection
			#Subsequence Collection for right direction           
			}elsif($direct =~ /right/i){
		    	if ($oppo){
		        $subStart = $sMiddle > $maxExtendLen ? $sMiddle - $maxExtendLen : 1;
				$subEnd = $hitSeq->length() -$sMiddle > $minCoverLen ? $sMiddle + $minCoverLen : $hitSeq->length();
				$extracted_seq_right = $hitSeq->trunc($subStart, $subEnd);
					if($logger->is_warn()){
						$logger->warn("Extraction points for subsequence (opposite orientation) --- Start: " , $subStart , " --- End: " , $subEnd)
                                           }
					if($logger->is_warn()){
						$logger->warn("Extracted Subsequence: " , $extracted_seq_right -> seq());
                                           }
		       
				}else{#if oppo flag is 0
			    $subStart = $sMiddle > $minCoverLen ? ($sMiddle - $minCoverLen+1) : 1;
				$subEnd = ($hitSeq->length() - $sMiddle) > $maxExtendLen ? ($sMiddle + $maxExtendLen+1) : $hitSeq->length();
			    $extracted_seq_right = $hitSeq->trunc($subStart, $subEnd);
					if($logger->is_warn()){
						$logger->warn("Extraction points for subsequence-> Start: " , $subStart , " --- End: " , $subEnd)
                                           }
					if($logger->is_warn()){
						$logger->warn("This is the extracted sequence: " , $extracted_seq_right -> seq());
			                               }
			         }#Closing right direction $oppo call for subsequence collection
			                           }#Closing right direction subsequence collection
			
			#This adds sequences to a temporary hash that can be used to upload sequences to usedSeqs if the M.A. goes well.
			$usedSeqs_Two{$hit->name()} = $hitSeq; 
			
			#Can be activated to report what is in this hash to terminal
			#my @blah = keys %usedSeqs_Two;
			#print "\n\n*********Here is what is in %usedSeqs_Two*****" , @blah , "\n\n";
			
			#Report sequence chosen for multiple alignment
			if($logger->is_fatal()){
				$logger->fatal($hitSeq->id , " chosen to construct New Query Sequence.")
				                    }
             
             #Collect portion of sequence that overlaps and extends and push it onto an array for multiple alignment  		
			 if ($oppo){
			 $hitSeq = $hitSeq->trunc($subStart, $subEnd);
			 $hitSeq = $hitSeq->revcom();
			 push @hits, $hitSeq;
			 }else{#if oppo flag is 0
			 push @hits, $hitSeq->trunc($subStart, $subEnd);
		           }#Closing $oppo flag for push of seqs to @hits
		           }#if($hsp->length('total') >= $minBlastMatch && $hsp->frac_identical() >= $minBlastIdentity){
		           }#while (my $hsp = $hit->next_hsp()){
		           }#while((my $hit = $res->next_hit()) && @hits < $maxExtendHits){

#next: construct a multiple alignment of the collected sequences

#variable for multiple alignment sequence object
my $newSeqObj;
	
#Report collected sequences for extension.Currently only reports bioseq reference. Update later
#if($g->is_debug()){
#	$logger->debug("Hits collected for extension:");
#	foreach my $hit(@hits){
#			$logger->debug(join("\t", @hits));
#	                      }
#	                   }

#Check to make sure that there are a minimum number of sequences collected for extension    
if(@hits < $minExtendHits){
$shouldStop = 1;
	logStop($logger, $direct, "the number of hits available for multiple alignment is " . ($#hits+1) .  " and this is less than ".$minExtendHits); 
		return ($newSeqObj, $shouldStop);
	                       }
#Variable to store New Query Sequence
my $newQuerySeq;

#Is there more than 1 sequence for extension? If the minimum is set to 1 then this is acceptable
if(@hits > 1){
	# construct a multiple alignment using those (truncted) hits by passing this info the the Do_Multiple_Alignment sub
	my $aln = doMultipleAlign(\@hits, "clustalw"); 
	   
	#Reports the multiple alignment result into a file that overwrites itself each time a multiple alignment is run. This is used to report the alignment to the log file.
	my $variable = 'clustalw_out';
	open (FILE , $variable);
	my @clustalw_outfile = <FILE>;
	if($logger->is_debug()){
	$logger->debug("CLUSTALOUTFILE: " , @clustalw_outfile);}
	   
		if($logger->is_fatal()){
			$logger->fatal("Multiple Alignment Complete");
	                            }
	    #Filter MA product based on minimum identity set at parameters section. If any column in the MA is not present at at least a certain percentage the spot is filled in with a "?".
	    $newQuerySeq = $aln->consensus_string($alignmentThresholdPercentage); 
	    
#It is possible to only use a single sequence for extension. In this case AAARF reports this to the user and returns the single sequence as the New Query without performing a multiple alignment
}elsif(@hits == 1){
$newQuerySeq = $hits[0] -> seq(); 
		
	#Report that only one sequence was used for extension
	if($logger->is_fatal()){
		$logger->fatal("Only one sequence was available for extension so no multiple alignment took place and the single sequence was used for extension.");
	                        }
#If there are no sequences for extension, die		
}else{
	die "Minimum number hits for extension must be at least 1.\n";
	 }

#Check to make sure that the New Query Sequence is long enough
if(length($newQuerySeq) < $minCoverLen){
	# If it is not then report it
	logStop($logger, $direct,"the new query sequence length is ".(length($newQuerySeq)).". This is less than the required minimum ".$minCoverLen);
	$shouldStop = 1;
                                       }else{
	
#This takes info from the usedSeqs_Two and puts it into usedSeqs_int. usedSeqs_int will not pass info into usedSeqs until the build is done. That way each sequence can be used to the fullest 
%usedSeqs_int = (%usedSeqs_int, %usedSeqs_Two);

#Can be activated to report the contents of this hash to the terminal
#my @argh = keys %usedSeqs_int; 
#print "\n\n*****Here is what is in %usedSeqs_int right now*****" , @argh , "\n\n";
	
#following 3 lines collect the sequences used for multiple alignments for later output
#Variable for storing sequences chosen for multiple alignment
my $hitelement; 
#Add each of these to the array
foreach $hitelement (@hits){
push @buildseqs , $hitelement->seq;

#reports the number of times that a sequence has been used so far
if (exists $usedSeqs_Three{$hitelement->id()}) {
	$Seq_Counter = $usedSeqs_Three{$hitelement->id()};
	$Seq_Counter++;
		if($logger->is_info()){
			$logger->info($hitelement->id() , " has been used " , $Seq_Counter , " times.");
	                            }
		$usedSeqs_Three{$hitelement->id()} = $Seq_Counter;
}else{
		$Seq_Counter =1;
		$usedSeqs_Three{$hitelement->id()} = $Seq_Counter;
			if($logger->is_info()){
				$logger->info($hitelement->id() , " has been used " , $Seq_Counter , " time.");
	                                }
	  }#Closing the if conditional checking if a sequence has been used before
		
	                                          }#foreach $hitelement (@hits){
	                                          
#The new query sequence is named using the Get_New_Consensus_Seq_Name sub
$newSeqObj = Bio::Seq->new(-id=>getNewConsensusSeqName(), -seq=>$newQuerySeq);
                                              }#Closing check for New Query length

return ($newSeqObj, $shouldStop);
}#Close Build_New_Query Sub

#-----------------------+
#GET_NEW_CONSENSUS_NAME |
#-----------------------+
#Function: Names the new build based on the number of builds generated
sub getNewConsensusSeqName{
 	$consensusSerialNum++;
	return $consensusSeqNamePre . $consensusSerialNum;
}

#----------------------+
#BUILD_BLAST_CONSENSUS |
#----------------------+
#Function: given blast output, locates MCS for Query Sequence
# input: a reference to query sequence and a ref to the blast result (searchIO object)
# output: MCS, start and end position of MCS in query
sub MCS_Search{
	#input to sub
	my ($qSeq, $res) = @_;
	#report to logger that the program has entered this sub
	my $logger = Log::Log4perl->get_logger("MCS_Search");
	#report the sequence id and size of the sequence that is being used
	if($logger->is_fatal()){
		$logger->fatal("Searching for Minimum Covered Sequence (MCS) boundaries --- Sequence Id: ", $qSeq->id() , " --- Sequence Length: " , $qSeq->length());
	                        }
	
	# array to store hits that meet length and identity requirements
    my @hits;  
    #variable for query name
	my $qName = $res->query_name();
	#variable for query length
	my $qLen = $res->query_length();
	
	# consider each subject sequence in the BLAST output until the maximum number allowed is reached
	while((my $hit = $res->next_hit()) && (@hits < $maxBlastHits)){
		#if the query id is the same as the subject id skip the hit
		if($hit->name eq $qName){
			next;
		                         }
		#if the sequence has been used in a previous build skip it
		if (exists $usedSeqs{$hit->name()}){
			if($logger->is_info()){
				$logger->info("Sequence ", $hit->name(),", not searched for MCS boundaries because it has already been used.");		
			                        }
				next;
	                                        }
	#for each hsp between the query and subject...
	while (my $hsp = $hit->next_hsp()){ 
	#binary switch for the HSP screen below
	my $HSP_Screen = 1; 
		#if the hsp meets size and identity requirments...
		if($hsp->length('total') >= $minBlastMatch  && $hsp->frac_identical() >= $minBlastIdentity){
		
		#record the hit stats in the array @stats
		my @stats = ($hit->name(), $hsp->start('query'),$hsp->end('query'), $hsp->start('hit'),$hsp->end('hit'), $hsp->strand('hit'));
		
			# for each element in the array @hits 
			for(my $j = 0; $j < @hits; $j++){
			
				#This is the start of the HSP screen. This is to ensure that overlapping HSPs are not used to construct the MCS. This could inflate the MCS matrix making a sequence appear repetitive when it is not.These four hsp screens make sure that if the subject and array element IDs are identical that there is no overlap in the coordinates that would indicate a duplicate HSP                             
				if (($hit->name() eq $hits[$j][0] && $hits[$j][1] <= $hsp->start('query') && $hsp->start('query') <= $hits[$j][2]) or ($hit->name eq $hits[$j][0] && $hits[$j][1] <= $hsp->end('query') && $hsp->end('query') <= $hits[$j][2]) or ($hit->name() eq $hits[$j][0] && $hits[$j][1] >= $hsp->start('query') && $hsp->end('query') >= $hits[$j][1]) or ($hit->name eq $hits[$j][0] && $hits[$j][2] >= $hsp->end('query') && $hsp->start('query') >= $hits[$j][2])) {
					# Report the overlapping HSPs
					if($logger->is_error()){
						$logger->error("An HSP from sequence ", $hit->name()," was not used to for MCS search because it is a duplicate overlapping HSP. Query Start: " , $hsp->start('query') , " Query Stop: " , $hsp->end('query') , " Compare to sequence: " , $hits[$j][0] , " Query Start: " , $hits[$j][1] , " Query Stop: " , $hits[$j][2]);		
			                                }
	#Binary switch to make sure that duplicate HSPs are not pushed to @hits
	$HSP_Screen = 0;
										}#if conditional for checking the HSPs
											}#for each element in @hits
				if ($HSP_Screen){
					push @hits, \@stats;	
				                }#$HSP_Screen if		 
				                
	}#Length and Identity requirment
	}#While there is another HSP
	}#While there is another hit in $res

	#report hits chosen for MCS
	if (@hits >=1){
	if($logger->is_fatal()){
		$logger->fatal("Hits chosen for MCS: (Id---Query Start and Stop Coordinates---Subject Start and Stop Coordinates---Subject Strandedness)");
		foreach my $hit(@hits){
			$logger->fatal(join("\t", @$hit));
		                      }
	                        }
		}else{
			if($logger->is_fatal()){
			$logger->fatal("There were no significant hits to the query sequence using this parameter set.");
									}
			 }

	#next: calculate coverage at each position over the length of query sequence
	#array to hold the coverage information for wach position on the query
	my @coverage;
	#set the coverage for each position on the query...
	for(my $i = 0; $i < $qLen; $i++){ 
		#...to 0
		$coverage[$i] = 0; 
	                                 }
	    #at each position on the query sequence...
		for(my $i = 0; $i < $qLen; $i++){ 
			#...for each member of @hits...
			for(my $j = 0; $j < @hits; $j++){ 
				#...look into the array @hits, and since this is an array of arrays then look at the first element in the array @stats that makes up a member of @hits. Looking at the query start and stop in the array @stats. If the query start is less than the current position AND the query end is greater than the current position...1_26_07 Added equal signs to the if conditional for both the Query start and stop so that during matrix construction values that matched the start and stop would register
				if($i >= $hits[$j][1] && $i <= $hits[$j][2]){ 
				#...increase coverage count by 1...1_26_07 Changed this line to $i-1 to offset the fact that the matrix array starts counting form 0. 
				$coverage[($i-1)]++; 
					#1_26_07 There was a problem with the matrix formation that the last place on the matrix was not covered if because of the offset problems. I have added this coverage increment to take care of this problem.
					if($i == ($qLen -1) && $i >= $hits[$j][1] && $i == ($hits[$j][2]-1)){
					$coverage[$i]++;
					                                                                     } 
			                                                  }#if($i >= $hits[$j][1] && $i <= $hits[$j][2])
		                                        }#for(my $j = 0; $j < @hits; $j++)
	                                  }#for(my $i = 0; $i < $qLen; $i++)
	                                  
	#report MCS to logger
	if (@hits >=1){
		if($logger->is_error()){
		$logger->error("Coverage Matix (coverage depth for each position on query sequence):","\n", join(" ", @coverage));
	                           }
	               }

	
	#based on the coverage get the sequence that covers the middle region with minimum required coverage
	#TO DO : it is possible that the middle region has less coverage, in the future need to replace this simple algorithm
	#set coverstart to 0
	my $coverStart = 0;
	#increment the position of the beginning of the minimum covered area as long as you are less than the length of the query and the current element of @coverage is less than the minimum required coverage
	while($coverStart < $qLen && $coverage[$coverStart] < $minBlastCoverDepth){
	$coverStart++; 
	                                                                           }
	#1_26_07 Adjusting the actual coverStart to be extracted becuase it is off by one in the coverage matrix
	$coverStart++; 
	#Set initially as the end of the sequence
	my $coverEnd = $qLen - 1; 
	# Moving backwards through sequence to find end of the area
	while($coverEnd >= 0 && $coverage[$coverEnd] < $minBlastCoverDepth){
	$coverEnd--;
	                                                                    }
	# 1_26_07 Move End forward 1 to clear up offset         
	$coverEnd=$coverEnd + 1;
	
	#This can be activated to control the size of the MCS.
	#if (($coverEnd - $coverStart) > 500){
	#	if($logger->is_debug()){
	#	$logger->debug("The blast_Consensus sequence is greater than 500 bp. I am trimming it back to 500");
	#}
	#	while (($coverEnd - $coverStart) > 500){
	#		$coverStart = $coverStart +1;
	#		$coverEnd = $coverEnd -1;
	#}}
	
	#Variable to hold MCS, initially empty
	my $consen = "";
	#, Check to make sure that the end coordinate is bigger than the start, if not, then $consen remains empty
    if($coverEnd > $coverStart){ 
	$consen = $qSeq->subseq($coverStart, $coverEnd);
	                            }	
	
	#Report the stats for the MCS
	if($logger->is_fatal()){
		$logger->fatal("MCS search complete --- MCS Length: ", length($consen), " --- Coverage Start: ", $coverStart, " --- Coverage End: ", $coverEnd);
	                       }
	return ($consen, $coverStart, $coverEnd);
}#Close sub Build_Blast_Consensus

#------------------+
#DO_MULTIPLE_ALIGN |
#------------------+
#Function: performs multiple alignment of sequences that overlap and extend build 
#input: array of sequences and multiple alignement program choice from BUILD_NEW_QUERY sub
#output: consensus sequence from multilple alignment
sub doMultipleAlign{
    my $clustalw_out;
    my $seqsRef = shift;
    my $program = "clustalw";
    if(@_){
	$program = shift;
           }
    my $factory;
    if($program =~ /clustalw/i){
	my @params = ('type'=>'DNA' , 'outfile' => 'clustalw_out');
        $factory = Bio::Tools::Run::Alignment::Clustalw->new(@params);
    }else{
	$factory = Bio::Tools::Run::Alignment::TCoffee->new();
          }
    my $aln = $factory->align($seqsRef);
    return $aln;
                     }

#-----------------+
#GET_BLAST_OUTPUT |
#-----------------+
#Function: Performs BLAST of query sequence against sample sequence database
#input: query sequence and BLAST database
#output: BLAST report
sub getBlastOutput{
	my ($s, $db) = @_;
	my @param = (program=>'blastn', database=>$db,
		     e=> $BLAST_e, F=>'F');
	my $factory = Bio::Tools::Run::StandAloneBlast->new(@param);
	my $blastOut = $factory->blastall($s);
	return $blastOut;
}