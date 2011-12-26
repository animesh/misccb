#/bin/sh
# RCSID='$Id: dopip3.sh,v 1.9 2003/06/16 20:38:07 schwartz Exp schwartz $'
MKFILE=${PIPHOME:?}/lib/make-pip.mk

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
	out.lat out.pp out.exons out.lav out.ono out.ono.seq \
	out.pip.ps out.pip.pdf \
	out.dot.ps out.dot.pdf \
	out.ono.ps out.ono.pdf
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

# stop on non-zero status
set -e

# XXX - should filter more carefully
TO="`tr '\n\r\f' '  ' <./email`"
TITLE="`tr '\n\r\f' '  ' <./piptitle`"

# prepend header to stderr, so we can mail it back
test -n "$TO" || { echo "Empty email address" 1>&2; exit 1; }
cat <<! 1>&2
To: $TO
From: PipMaker <pipmaster@bio.cse.psu.edu>
Subject: Unrecoverable errors from PipMaker (submission failed)

!

# be noisy; need {} instead of () to keep -e in effect.
#set -x
{
! f.bool genono || make -f $MKFILE out.ono out.ono.ps out.ono.seq
! f.bool genpip || make -f $MKFILE out.pip.ps
! f.bool gendot || make -f $MKFILE out.dot.ps
! f.bool genlat || make -f $MKFILE out.lat
! f.bool gentxt || make -f $MKFILE out.pp
! f.bool genxan || make -f $MKFILE out.exons
! f.bool genlav || make -f $MKFILE out.lav
if f.bool genpdf; then
    pip-distill out.pip.ps
    pip-distill out.dot.ps
    pip-distill out.ono.ps
fi
} 1>&2 # XXX - need redirection to keep make chatter away from stdout

cons_result  \
 --to "$TO" \
 --txt "out.lat" \
 --pp "out.pp" \
 --xan "out.exons" \
 --lav "out.lav" \
 --ono "out.ono" --onoseq out.ono.seq \
 --pipps "out.pip.ps" --pippdf "out.pip.pdf" \
 --dotps "out.dot.ps" --dotpdf "out.dot.pdf" \
 --onops "out.ono.ps" --onopdf "out.ono.pdf" \
 --args "dump" \
 --title "$TITLE"

status=$?
echo $status >out.status
exit $status
