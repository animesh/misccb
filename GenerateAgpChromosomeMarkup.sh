#! /usr/bin/tcsh -f

###
# GenerateAgpChromosomeMarkup.sh
#
# Run CreateChromosomeQual on the markup mergedcontigs files (errorsb,
# indelsb, and misjoinsb), generated by FeudalMarkup.
#
# DRAFT: the base name of the output dir of the module ForDistribution
##
set PRE    = "$ARACHNE_PRE"
set DATA   = "projects/Bushbaby"
set RUN    = "run/work"
set SUB    = "assisted_3"
set DRAFT  = "Draft_v1"

##
# Derived path names.
set pdr      = "PRE=$PRE DATA=$DATA RUN=$RUN"
set fulldata = "$PRE/$DATA"
set fullrun  = "$PRE/$DATA/$RUN"
set fullsub  = "$fullrun/$SUB"

##
# Create agp.chromosome.misjoins
if ( ! -e $fulldata/$DRAFT/Draft_v1.agp.chromosome.misjoins.gz ) then
    CreateChromosomeQual $pdr \
	SUBDIR=$SUB \
	OUTDIR=$SUB \
	ASSEMBLY_NAME=$DRAFT \
	AGP_FILE=assembly.agp \
	TAIL=misjoinsb

    set sub_agp = $fullsub/Draft_v1.agp.chromosome.misjoins.gz
    set draft_agp = $fulldata/$DRAFT/Draft_v1.agp.chromosome.misjoins.gz
    mv $sub_agp $draft_agp
endif

##
# Create agp.chromosome.indels
if ( ! -e $fulldata/$DRAFT/Draft_v1.agp.chromosome.indels.gz ) then
    CreateChromosomeQual $pdr \
	SUBDIR=$SUB \
	OUTDIR=$SUB \
	ASSEMBLY_NAME=$DRAFT \
	AGP_FILE=assembly.agp \
	TAIL=indelsb

    set sub_agp = $fullsub/Draft_v1.agp.chromosome.indels.gz
    set draft_agp = $fulldata/$DRAFT/Draft_v1.agp.chromosome.indels.gz
    mv $sub_agp $draft_agp
endif

##
# Create agp.chromosome.errors
if ( ! -e $fulldata/$DRAFT/Draft_v1.agp.chromosome.errors.gz ) then
    CreateChromosomeQual $pdr \
	SUBDIR=$SUB \
	OUTDIR=$SUB \
	ASSEMBLY_NAME=$DRAFT \
	AGP_FILE=assembly.agp \
	TAIL=errorsb

    set sub_agp = $fullsub/Draft_v1.agp.chromosome.errors.gz
    set draft_agp = $fulldata/$DRAFT/Draft_v1.agp.chromosome.errors.gz
    mv $sub_agp $draft_agp
endif

