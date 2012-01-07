#!/bin/sh
# $Id: start-pipserver.sh,v 1.14 2003/04/29 19:00:07 schwartz Exp $

prefix=/usr/local/align;   # XXX - config
piphome=$prefix/pipmaker;  # XXX - config
exec $piphome/bin/pipenv init-pipserver pipserver
