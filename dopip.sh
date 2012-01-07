#!/bin/sh
RCSID='$Id: dopip.sh,v 1.71 2005/03/02 23:18:39 rico Exp $'

PIPLIB="${PIPHOME:?}/lib"
TXT_MAX=1000000  # XXX - config

set -ue

# files
SEQ1=./seq1data
RPTS=./seq1mask
SEQ2=./seq2data
EXON=./exons
COLORS=./colors
ID=./id
ARGS=./dump

# values
TO=`cat ./email | tr '\n\r\f' '  '` # XXX
FMT=`cat ./format`
STRAND=`cat ./strand`
CHAIN=`cat ./chain`
GENPIP=`cat ./genpip`
GENDOT=`cat ./gendot`
GENPDF=`cat ./genpdf`
GENTXT=`cat ./gentxt`
GENLAT=`cat ./genlat`
GENXAN=`cat ./genxan`
GENLAV=`cat ./genlav`
FOUR=""
TITLE=`cat ./piptitle`
NOVICE=`cat ./novice`

warn()
{
    printf "%s\n" "$*" 1>&2
}

fatal()
{
    warn "$@"
    exit 1
}

force_www()
{
    if test -f $PIPHOME/etc/forcewww
    then fgrep -f $PIPHOME/etc/forcewww email >/dev/null
    else false
    fi
}

output_too_big()
{
    if files-too-big \
	"$TXT" "$PP" "$EXAN" "$LAVV" "$PIPPS" "$PIPPDF" "$DOTPS" "$DOTPDF"
    then
        echo note: output file size is too large for email, forcing www >>dump
        return 0
    fi
    return 1
}

cons_result()
{
    if f.bool genwww || force_www || output_too_big
    then cons-html "$@"
    else cons-mime "$@"
    fi
}

test -n "$TO" || { warn "Empty email address"; }
cat <<! 1>&2
To: $TO
From: PipMaker <pipmaster@bio.cse.psu.edu>
Subject: Unrecoverable errors from PipMaker (submission failed)

!

checkint() # name default input
{
    case "$3" in
      '') printf "%d" "$2";;
      *)  printf "%d" "$3";;
    esac
}

NOVICE=`checkint novice 0 "$NOVICE"`
STRAND=`checkint strand 0 "$STRAND"`
CHAIN=`checkint chain 0 "$CHAIN"`
GENPIP=`checkint genpip 0 "$GENPIP"`
GENDOT=`checkint gendot 0 "$GENDOT"`
GENPDF=`checkint genpdf 0 "$GENPDF"`
GENTXT=`checkint gentxt 0 "$GENTXT"`
GENLAT=`checkint genlat 0 "$GENLAT"`
GENXAN=`checkint genexon 0 "$GENXAN"`
GENLAV=`checkint genlav 0 "$GENLAV"`

STYLE=$PIPLIB/tag.style100
[ -r "$STYLE" ] || { fatal "Cannot read STYLE '$STYLE'"; }

OUT=out.$$
LAV=$OUT.lav
TXT=$OUT.txt
TXTE=$OUT.txt.err
PIPPS=$OUT.pip.ps
DOTPS=$OUT.dot.ps
PIPPDF=$OUT.pip.pdf
DOTPDF=$OUT.dot.pdf
PP=$OUT.pp
CMD=$OUT.cmd
LEGEND=$OUT.legend
HEAD=$OUT.head     # might not need this now that we have LEGEND
TAGS=$OUT.tag
UNDR=$OUT.undr
EXAN=$OUT.xan
TMP1=$OUT.tmp1
TMP2=$OUT.tmp2
TMP3=$OUT.tmp3
TMP4=$OUT.tmp4
TMP5=$OUT.tmp5
TMP6=$OUT.tmp6
STATUS=status
LAVV=/dev/null

make_blastz_txt() # blastz.lav
{
    warn +make_blastz_txt "$@"
    if grep '^a[ \t]*{' "$1" >/dev/null
    then
	# olat -out 1- "$1"
	lat 1- "$1"
    else echo Empty alignment.
    fi
}

make_blastz() # seq1 seq1.rpts seq2
{
    warn +make_blastz "$@" 

    m=`expr 80 \* 1024 \* 1024`
    pip-aux-repeats "$2" >$TMP3 &&
    seqmask "$1" $TMP3 >$TMP4 &&
    case "$CHAIN" in
    3) timed_blastz "$TMP4" "$3" m="$m" B="$STRAND" | single_cov - ;;
    2) timed_blastz "$TMP4" "$3" m="$m" B="$STRAND" C="$CHAIN" K=2000 ;;
    *) timed_blastz "$TMP4" "$3" m="$m" B="$STRAND" C="$CHAIN" ;;
    esac
}

make_tags() # exons seq1 seq1.rpts
{
    warn +make_tags "$@"
    gene_tag "$1" &&
    pip-aux-repeats "$3" &&
    CpG "$2" 60 CpG60 &&
    CpG "$2" 75 CpG75 
    # echo 'gsave plotdata 0.75 GridLine grestore'
}

make_blastz_pp() # seq1 seq1.rpts seq2 exons
{
    warn +make_blastz_pp "$@"
    concise-align "$1"
}

make_blastz_ps() # seq1 seq1.rpts exons lav colors
{
    warn +make_blastz_ps "$@"
    mklegend "$TITLE" >$LEGEND &&
    gene_tag -h "$3" >$HEAD &&
    make_tags "$3" "$1" "$2" >>$TMP5 &&
    group_tags "$TMP5" >>$TAGS &&
    make_pip "$4"
}

make_pip() # lav
{
    if [ -z "$FOUR" ]
    then n=5;
    else n=4;
    fi

    warn +make_pip "$@" &&
    mkpipcmd "$1" $HEAD $TAGS $UNDR $FOUR >$CMD &&
    /bin/sh -xe $CMD &&
    warn +pippage "-l $STYLE -n $n -c $LEGEND eps.*" &&
    pippage -l $STYLE -n $n -c $LEGEND eps.*
}

make_analyze_exons()
{
    warn +analyze_exons "$@"
    analyze_exons "$1" "$2" || true
}

make_dotplot()
{   
    warn +make_dotplot "$@"
    mkdotplot "$@"
} 


make_title()
{
    case "$TITLE" in
    ''|[\ \t\n\r\f]*)
        if [ -r "$EXON" ]
        then TITLE=`sed -e 1q "$EXON"`
        else TITLE="Percent Identity Plot"
        fi
	;;
    esac
    TITLE=`printf "%s\n" "$TITLE" | tr '\r\n' '  '`
    TITLE=`printf "%s\n" "$TITLE" | sed -e 's/\([\\()]\)/\\\1/g' -e 1q`
}

blastit() # seq1 seq1.rpts seq2 exons colors
{
    warn +blastit "$@"
    { make_blastz "$1" "$2" "$3"; } >$LAV &&
    { [ "$GENPDF" -lt 2 ] || mkunderlays2 "$LAV"; } >>$UNDR &&
    { mkunderlays "$5"; } >>$UNDR &&
    { [ "$GENTXT" -le 0 ] || make_blastz_pp $LAV; } >$PP &&
    { if [ "$GENLAT" -gt 0 ]
      then
        if ! make_blastz_txt $LAV >$TXT 2>$TXTE
        then
          echo "pipmaker: lat failed; its incomplete output has been deleted." >$TXT;
          cat $TXTE >>$TXT
	else
	  if [ $NOVICE -gt 0 ] && [ "`wc -c < $TXT`" -gt $TXT_MAX ]
	  then
            echo "pipmaker: verbose text is bigger than $TXT_MAX bytes; discarding it." >$TXT
	  fi;
        fi;
      fi;
    } &&
    { if [ "$GENPIP" -gt 0 ]
      then make_blastz_ps "$1" "$2" "$4" "$LAV" "$5" >"$PIPPS" &&
         ([ "$GENPDF" -lt 1 ] || pip-distill "$PIPPS" "$PIPPDF" 1>&2 )
      fi;
    } &&
    { if [ "$GENDOT" -gt 0 ]
      then make_dotplot "$LAV" "$4" "$UNDR" "$STYLE" "$SEQ2" >"$DOTPS";
         ([ "$GENPDF" -lt 1 ] || pip-distill "$DOTPS" "$DOTPDF" 1>&2 )
      fi;
    } &&
    { [ "$GENXAN" -lt 1 ] || make_analyze_exons $EXON $LAV; } >$EXAN &&
    { if [ "$GENLAV" -gt 0 ]; then LAVV=$LAV; fi; }
}

# --- main

warn "$RCSID"
make_title &&
blastit $SEQ1 $RPTS $SEQ2 $EXON $COLORS &&
cons_result  \
 --to "$TO" \
 --txt "$TXT" \
 --pp "$PP" \
 --xan "$EXAN" \
 --lav "$LAVV" \
 --pipps "$PIPPS" \
 --pippdf "$PIPPDF" \
 --dotps "$DOTPS" \
 --dotpdf "$DOTPDF" \
 --args "$ARGS" \
 --title "$TITLE"

status=$?

echo $status >"$STATUS"
exit $status

