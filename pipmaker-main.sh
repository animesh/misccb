#!/bin/sh
# $Id: pipmaker-main.sh,v 1.13 2001/05/11 01:22:12 schwartz Exp $

#
# This goes in the cgi-bin, to provide a more convenient entry point.
#
echo 'Content-type: text/html'
echo ''

# XXX - config
prefix=/usr/local/align
WWW=$prefix/pipmaker/www

case "$1" in
 1|basic)         cat $WWW/pipmaker.html;;
 2|superseded)    cat $WWW/pipmaker2.html;;
 3|advanced)      cat $WWW/pipmaker3.html;;
 basic-instr*)    cat $WWW/pip-instr.html;;
 advanced-instr*) cat $WWW/pip-instr2.html;;
 *)               cat $WWW/index.html;;
esac

