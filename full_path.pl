#
#  Procedure that builds the full path of a filename (UNIX file paths)
#

require "getcwd.pl";
package FullPathPack;

sub main'FullPath {
    local($FILENAME) = @_;
    local($PWD);
    if ($FILENAME !~ m#^/#) {
        $PWD=&main'getcwd;
        $PWD .= "/" if ($PWD !~ m#/$#);
        $FILENAME = $PWD . $FILENAME;
        }
    $FILENAME=&CompactDotDots($FILENAME);
    return ($FILENAME,&main'BreakFileName($FILENAME));
    }

sub main'BreakFileName {
    local($FILE) = @_;
    local($DIR,$BASE);
    $FILE =~ m#([^/]*)$#;
    ($DIR,$BASE) = ($`,$1);
    if ($DIR =~ m#(.+)/$#) {
        $DIR=$1;
        }
    return($DIR,$BASE);
    }

sub CompactDotDots {
    local($FILENAME) = @_;
    local($TOKEN,@OUT);
    $FILENAME =~ s#//#/#             while ($FILENAME =~ m#//#);
    $FILENAME =~ s#/$##              while ($FILENAME =~ m#./$#);
    $FILENAME =~ s#(.)/\./(.)#$1/$2# while ($FILENAME =~ m#./\./.#);
    $FILENAME =~ s#^\./##            while ($FILENAME =~ m#^\./.#);
    $FILENAME =~ s#/\.$##            while ($FILENAME =~ m#./\.$#);
    $FILENAME =~ s#/$##              while ($FILENAME =~ m#./$#);
    return "/" if ($FILENAME eq "/." || $FILENAME eq "/");
    return "." if ($FILENAME eq ".");
    while ($FILENAME) {
        $FILENAME =~ m#^([/]+)|^([^/]+)#;
        $TOKEN=$+;
        $FILENAME=$';
        if ($TOKEN =~ m#/#) {
            push(@OUT,"/") if ($OUT[$#OUT] !~ m#/#);
            }
        else {
            if ($TOKEN ne "..") {
                push(@OUT,$TOKEN);
                next;
                }
            next if (@OUT < 2);
            pop(@OUT);pop(@OUT);
            }
        }
    pop(@OUT) if (@OUT > 1 && $OUT[$#OUT] =~ m#/#);
    return(join("",@OUT));
    }
1;
