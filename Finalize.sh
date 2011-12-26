#! /usr/bin/tcsh

# Evaluate assembly against finished sequence and generate report.


##
# Arguments.
#
set LOOKUP = /wga/scr5/Fungi/Neurospora/Finished/mips23.lookup
set DATA   = projects/Neurospora
set RUN    = sante_2003mar17/work
set SUB    = backup_mc11
set KNOWN  = contigs.fasta


##
# Setup and count contigs.
#
set FULL_SUB = $ARACHNE_PRE/$DATA/$RUN/$SUB
set FASTB    = $FULL_SUB/mergedcontigs.fastb
set NTIGS    = `FeudalSize FILE=$FASTB NH=True | head -1 | Col 1`


##
# Generate alignments.
#
AlignAssembly \
    DATA=$DATA \
    RUN=$RUN \
    SUBDIR=$SUB \
    L=$LOOKUP \
    K=12 \
    ARGS="MF=100 MO=200 END_STRETCH=20" \
    ARGS1="MAX_NQS_PERCENT=0.5" \
    ARGS2="MC=0.02" \
    REJECT_ABS=10000 \
    REJECT_REL=0.4 \
    AI=True >& $FULL_SUB/QueryLookupTable.out

LookAlignsToNobbits \
    INPUT=$FULL_SUB/QueryLookupTable.out \
    OUTPUT=$FULL_SUB/QueryLookupTable.out.nobbits \
    ID2_SHIFT=$NTIGS

RefineConsensusAlignments \
    DATA=$DATA \
    RUN=$RUN \
    SUBDIR=$SUB \
    KNOWN_CONTIGS=$KNOWN \
    ALIGNMENT_FILE=QueryLookupTable.out.nobbits


##
# Generate report.
#
FinishNewReport \
    DATA=$DATA \
    RUN=$RUN \
    SUBDIR=$SUB \
    KNOWN_CONTIGS=$KNOWN \
    NOBBITS_FILE=QueryLookupTable.out.nobbits_refined
