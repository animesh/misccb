#!/usr/local/bin/perl -w

print "static char *valid_colors[] = {\n";
while (<>)
{
    next if /^\s*$/;
    next if /^\s*#/;
    /^\s*(\w+)\s+(\w+)\s+([.\d]+)\s+([.\d]+)\s+([.\d]+)\s*$/ or die;
    printf "\"%s\",\n", $1;
}
print "0\n";
print "};\n";
