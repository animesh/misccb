#!/bin/sh

echo "Running the assembly."
echo "Main characteristics:"
echo "- Sanger & 454 hybrid assembly"
echo "- loading everything from CAFs (previously made in this pipeline)"
echo "- 7 passes with a maximum of 3 subpasses per pass (recommended in genome"
echo "  sized assemblies with 454 data and repeats)"
echo "- alignments need 80% similarity to be considered for inclusion"
echo "- after each pass, the current assembly is written as CAF file into"
echo "  the log directory"


echo "You can follow the progress in another terminal doing a"
echo     "'tail -f log_assembly.txt'"
time mira -project=$1 -caf  -AS:nop=7:rbl=3  -SK:pr=80 -AL:mrs=80  -CL:msvs=no:qc=no:bsqc=no:pvlc=no:mbc=no:emlc=no  -CO:mgqrt=40:mrpg=2   -DP:ure=no -OUT:otc=yes  -OUT:otc=yes  -GE:rns=TIGR  >& log_assembly.txt

if [ $? -gt 0 ]; then
  echo
  echo "MIRA did not run correctly. Please consult log_assembly.txt."
  exit
fi


#echo "Deleting unecessary temporary directory."
#rm -rf ${1}_d_log
echo "Done"
echo
echo "Results are in '${1}_d_results'"
echo "Statistics and other information are in '${1}_d_info'"

