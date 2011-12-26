#!/bin/sh

# this is a demo for a nice and simple genome MAPPING assembly with data which
# was downloaded from the NCBI trace archive, we have ancillary information in
# a XML file and TIGR made the reads, so we use that naming scheme

# We also turn on html output

# using the "accurate" assembly qualifier turns on MIRA in high quality mode,
#  ready to tackle a fair number of problems one can encounter in genome
#  assembly.  
# It's not really needed for this small set, but neither does it harm.

# The data itself is pretty good, but has a few reads that show distinct signs
#  "extended too long", i.e., they contain really bad quality. This is why we
#  turn on -CL:bsqc with standard parameters

# the difference to bbdemo1: we map reads from C.jejuni RM1221 against 
#  a backbone (the first 40kb of C.jejuni NCTC1168)

ln -f -s ../data/bbdataset1/cjejuni_demo* .

echo "Running mira"
mira -parameters=parameters.par | tee run.log
echo "Done."
echo "Load the project into the GAP4 editor to have a look at the first 40kb of"
echo " Campylobacter jejuni RM1221 mapped against Campylobacter jejuni NCTC1168"
echo "HTML output was also switched on so that you can load 'cjejuni_demo_out.html'"
echo " into a CSS compliant browser."
#echo ""
#echo "Look for SNPs between NCTC1168 (represented as 'read' named NC_002163) and"
#echo " reads from RM1221: search for the SROc tags which MIRA conveniently set for you."