#!/bin/sh
RCSID='$Id: mpm-run.sh,v 1.34 2005/01/03 22:30:38 rico Exp $'

PIPLIB="${PIPHOME:?}/lib"
STYLE="${PIPHOME:?}/lib/tag.style100"
TXT_MAX=`expr 50 \* 1024 \* 1024`  # XXX - config

t=t.$$

#set -x
set -ue # stop on any error

# trap 'cleanup' 0
cleanup()
{
	echo cleanup
	rm -f *:* *-
	rm -f *.align
}

warn()
{
    printf "%s\n" "$*" 1>&2
}

fatal()
{
    warn "$@"
    exit 1
}

checkint() # name default input
{
    case "$3" in
      '') printf "%s" "$2";;
      *) printf "%s" "$3";;
    esac
}

fgetint() # filename default 
{
    a=`cat "$1"`
    printf "%s" `checkint "$1" "$2" "$a"`
}

# data files: 
[ -s seq01data ]; [ -s seq02data ] # seq*data
[ -s seq01name ]; [ -s seq02name ] # seq*name
[ -r seq01mask ]
[ -r underlay ]
[ -r dfltulay ]
[ -r exons    ] 
[ -r annotation ] 

GENPDF=1


# first sequence
pip-aux-repeats seq01mask >$t.rpts
seqmask seq01data $t.rpts >$t.seq1

# nth sequence
for f in seq*data
do if [ ! -e "$f" ] # "seq*data" could mean that no files matched 
   then break
   fi

   if [ "$f" != seq01data ]
   then
	n=$(echo "$f" | tr -cd 0-9)
	sn="seq$n"
	# timed_blastz $t.seq1 "$f"  B=$STRAND C=$CHAIN > "$f.blastz"
	pip-aux-blastz $t.seq1 "$f" \
	    "${sn}chain" "${sn}strand" "${sn}sensitivity" >"$f.blastz"

        single_cov "$f.blastz" >"$f.align"

	if grep ' l ' "$f.blastz" >/dev/null 2>&1
	then :
	else echo "$f did not align" >align.txt; exit 0;
	fi

	#underlay_noseq $f.align | mkunderlays >$t.$n.noseq
	mkunderlays "${sn}underlay" >$t.$n.noseq
   fi
done

# overview
overview -u underlay -e exons -- seq*blastz -- seq*name >out.ov.ps

# refine 
#   only run refine if we're generating a nucleotide level view or very
#   verbose text -rico
if f.bool genmaps || f.bool gentext
then
  if time refine seq*data >$t.refine 2>align.txt
  then
    rm align.txt # discard random verbosity
  else
    # ... unfortunately draws errors in cases it shouldn't
    if grep 'refine:.*\.align contains no alignments' align.txt >/dev/null 2>&1
    then exit 0
    else
      : '                                                   '
      : '                                                   '
      : ' pipmaker error --                                 '
      : '                                                   '
      : ' refine unexpectedly exited, probably because it   '
      : ' exceeded the 30min CPU time limit that we impose. '
      : '                                                   '
      : '                                                   '
      : '                                                   '
      head align.txt 1>&2 
      exit 1
    fi
  fi
fi

# annotations
if true
then
  annot-legend annotation >out.a.legend
  underlays-legend underlay >out.u.legend

  pip-annot -t annotation >out.roff
  echo ".PSPIC out.a.legend 8.5i" >>out.roff
  echo ".PSPIC out.u.legend 8.5i" >>out.roff

  groff -S out.roff >out.annot.ps
fi

# lat
if f.bool genmaps
then
  maps2 $t.refine seq*name -- \
    U=underlay E=exons R=$t.rpts A=annotation >out.align.ps
  cat out.ov.ps out.align.ps out.annot.ps >align.ps
  [ $GENPDF -eq 0 ] || pip-distill align.ps align.pdf 1>&2
fi

# pip
if true
then
  gene_tag exons >$t.tags
  cat $t.rpts  >>$t.tags
  CpG seq01data 60 CpG60 >>$t.tags
  CpG seq01data 75 CpG75 >>$t.tags
  pip-annot annotation   >>$t.tags
  group_tags  $t.tags    >>$t.tags2
  if f.bool dfltulay; then mkunderlays underlay; fi >$t.utags

  mpip -h $STYLE -t $t.tags2 -u $t.utags -w *.noseq -- seq*data >$t.b.ps
  fix-maps <$t.b.ps >out.pip.ps

  exon-bookmarks exons >out.bmrk.ps
  pip-annot -b annotation >>out.bmrk.ps
  cat out.ov.ps out.pip.ps out.bmrk.ps out.annot.ps >pip.ps
  [ $GENPDF -eq 0 ] || pip-distill pip.ps pip.pdf 1>&2
fi

if f.bool gentext
then
  mlav2ascii $t.refine seq*name > align.txt
  # gzip -9 align.txt
fi

