#!/bin/sh
RCSID='$Id: dompip.sh,v 1.5 2003/06/11 21:08:49 schwartz Exp schwartz $'

PIPLIB="${PIPHOME:?}/lib"

set -ue

# files
# seq*data seq*name
# seq1mask exons colors
# id dump

# values
TO=`cat ./email | tr '\n\r\f' '  '` # XXX

output_too_big()
{
    if files-too-big \
	pip.ps pip.pdf align.ps align.pdf align.txt
    then
        echo note: output file size is too large for email, forcing www >>dump
        return 0
    fi
    return 1
}

force_www()
{
    if test -f $PIPHOME/etc/forcewww
    then fgrep -f $PIPHOME/etc/forcewww email >/dev/null
    else false
    fi
}

cons_result()
{
    if f.bool genwww || force_www || output_too_big
    then mpm-html "$@"
    else mpm-mime "$@"
    fi
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

test -n "$TO" || { fatal "Empty email address"; }
cat <<! 1>&2
To: $TO
From: MultiPipMaker <pipmaster@bio.cse.psu.edu>
Subject: Unrecoverable errors from MultiPipMaker (submission failed)

!

# --- main

warn "$RCSID"
mpm-run &&
cons_result \
 --to "$TO" \
 --pipps pip.ps \
 --pippdf pip.pdf \
 --acgtps align.ps \
 --acgtpdf align.pdf \
 --acgttxt align.txt \
 --args dump

status=$?

echo $status >status
exit $status

