#!/bin/sh
echo "library(\"utils\"); Sweave(\"$1\")" | R --no-save --no-restore
