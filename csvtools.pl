#!/usr/bin/perl -w

# File        : csvtools.pl
# Author      : Dr Nicola Talbot
# Date        : 26 June 2007
# Description : Perl script to accompany csvtools.sty
#             : Allows you to substitute \CSVtotabular, \CSVtolongtable and \applyCSVfile commands with the appropriate LaTeX code
# Version     : 0.6b (1 September 2006).

# usage : csvtools <in-file> <out-file>

if ($#ARGV != 1)
{
   die "Syntax : $0 <in-file> <out-file>\n";
}

($FILENAME,$OUTPUT) = @ARGV;

open FILENAME or die "Can't open '$FILENAME'\n";

$ext = substr($FILENAME, -4);

if (($ext eq ".tex") or ($ext eq ".dtx") or ($ext eq ".ltx"))
{
   $LOGFILE =  substr($FILENAME,0,length($FILENAME)-4) . ".log";
}
else
{
   $LOGFILE = $FILENAME . ".log";
}

open LOGFILE or die "Can't open log file '$LOGFILE'.  Make sure you run LaTeX before using $0\n";

while (<LOGFILE>)
{
   if (/\\c@([a-zA-Z]+)=\\count/)
   {
      $counter{$1} = 0;
   }
}

$counter{'csvrownumber'} = 0;

close LOGFILE;

open OUTPUT, ">$OUTPUT" or die;

# global csvsort options

$globalsort{'verbose'}=1;
$globalsort{'sort'}='alphabetical ascending';
$globalsort{'variable'}='\field{1}';
$globalsort{'sfirstdataline'}=1;
$globalsort{'firstdataline'}=2;

$keyvaldefaults{'verbose'}='true';

$csvpieouterlabel="\\field{1}";
$csvpieinnerlabel="\\field{2}\\%";
$separator=",";

while (<FILENAME>)
{
   $restofline = $_;
   local($comment) = "";

   if ($restofline=~m/(\\*)%/)
   {
      $n = length($1);

      if ($n%2 == 0)
      {
         $restofline = "$`$1";
         $comment = "%$'";
         $_ = $restofline;
      }
   }

   if ($restofline=~/\\setcsvseparator\s*{([^}]*)}/)
   {
      $separator=$1 if defined($1);
   }

   if ($restofline=~/\\renewcommand\*?\s*{?\\csvpieinnerlabel}?/)
   {
      print OUTPUT $`;

      $restofline=$';
      $lineno=$.;

      ($csvpieinnerlabel,$restofline,$done)=&getnextgroup($restofline);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
            $restofline .= $nextline;
            ($csvpieinnerlabel,$restofline,$done)=&getnextgroup($restofline);
         }
         else
         {
            die "Unexpected EOF while parsing \\renewcommand on line $lineno\n";
         }
      }

      $_ = $restofline;
   }

   if ($restofline=~/\\renewcommand\*?\s*{?\\csvpieouterlabel}?/)
   {
      print OUTPUT $`;

      $restofline=$';
      $lineno=$.;

      ($csvpieouterlabel,$restofline,$done)=&getnextgroup($restofline);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
            $restofline .= $nextline;
            ($csvpieouterlabel,$restofline,$done)=&getnextgroup($restofline);
         }
         else
         {
            die "Unexpected EOF while parsing \\renewcommand on line $lineno\n";
         }
      }

      $_ = $restofline;
   }

   while ($restofline=~/\\stepcounter(.*)/)
   {
      $line = $_;

      ($ctr,$restofline,$done) = &getnextgroup($1);

      $startline=$.;

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $line . $nextline;

             $restofline = $restofline . $nextline;

             ($ctr,$restofline,$done) = &getnextgroup($restofline);
         }
         else
         {
            die "EOF found whilst scanning first argument to \\stepcounter on line $startline\n";
         }
      }

      $counter{$ctr}++;

      $_ = $line;
   }

   $restofline = $_;

   while ($restofline=~/\\refstepcounter(.*)/)
   {
      $line = $_;

      ($ctr,$restofline,$done) = &getnextgroup($1);

      $startline=$.;

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $line . $nextline;

             $restofline = $restofline . $nextline;

             ($ctr,$restofline,$done) = &getnextgroup($restofline);
         }
         else
         {
            die "EOF found whilst scanning first argument to \\stepcounter on line $startline\n";
         }
      }

      $counter{$ctr}++;

      $_ = $line;
   }

   $restofline = $_;

   while ($restofline=~/\\setcounter(.*)/)
   {
      $line = $_;

      ($ctr,$restofline,$done) = &getnextgroup($1);

      $startline=$.;

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $line . $nextline;

             $restofline = $restofline . $nextline;

             ($ctr,$restofline,$done) = &getnextgroup($restofline);
         }
         else
         {
            die "EOF found whilst scanning first argument to \\stepcounter on line $startline\n";
         }
      }

      ($num,$restofline,$done) = &getnextgroup($restofline);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $line . $nextline;

             $restofline = $restofline . $nextline;

             ($num,$restofline,$done) = &getnextgroup($restofline);
         }
         else
         {
            die "EOF found whilst scanning second argument to \\stepcounter on line $startline\n";
         }
      }

      $num=~s/\\value{(.+)}/$counter{$1}/;

      $counter{$ctr} = $num;

      $_ = $line;
   }

   $restofline = $_;

   while ($restofline=~/\\addtocounter(.*)/)
   {
      $line = $_;

      ($ctr,$restofline,$done) = &getnextgroup($1);

      $startline=$.;

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $line . $nextline;

             $restofline = $restofline . $nextline;

             ($ctr,$restofline,$done) = &getnextgroup($restofline);
         }
         else
         {
            die "EOF found whilst scanning first argument to \\stepcounter on line $startline\n";
         }
      }

      ($num,$restofline,$done) = &getnextgroup($restofline);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $line . $nextline;

             $restofline = $restofline . $nextline;

             ($num,$restofline,$done) = &getnextgroup($restofline);
         }
         else
         {
            die "EOF found whilst scanning second argument to \\stepcounter on line $startline\n";
         }
      }

      $num=~s/\\value{(.+)}/$counter{$1}/;

      $counter{$ctr} = $counter{$ctr} + $num;

      $_ = $line;
   }

   $restofline = $_;

   while ($restofline=~/^(.*)\\csvGetEntry(.*)$/)
   {
      $start = $1;
      $restofline = $2;

      ($ctr,$restofline,$done) = &getnextgroup($restofline);

      $startline=$.;

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $restofline = $restofline . $nextline;

             ($ctr,$restofline,$done) = &getnextgroup($restofline);
         }
         else
         {
            die "EOF found whilst scanning first argument to \\csvGetEntry on line $startline\n";
         }
      }

      ($entry,$restofline,$done) = &getnextgroup($restofline);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $restofline = $restofline . $nextline;

             ($entry,$restofline,$done) = &getnextgroup($restofline);
         }
         else
         {
            die "EOF found whilst scanning first argument to \\csvGetEntry on line $startline\n";
         }
      }
      $_ = $start . "\\csname $entry\\roman{$ctr}\\endcsname" . $restofline;
      #$_ = $start. $data{$entry}[$counter{$ctr}] . $restofline;
   }

   $restofline = $_;

   while ($restofline=~/\\usepackage(.*)/)
   {
      $line = $_;

      $startline=$.;

      $restofline = $1;

      $options = '';

      if ($restofline=~/^\s*\[/)
      {
         ($options,$restofline,$done) = &getnextoptionalgroup($restofline);

         while (!$done)
         {
            if ($nextline = <FILENAME>)
            {
                $line = $line . $nextline;

                $restofline = $restofline . $nextline;

                ($options,$restofline,$done) = &getnextoptionalgroup($restofline);
            }
            else
            {
               die "EOF found whilst scanning optional argument to \\usepackage on line $startline\n";
            }
         }
      }


      ($package,$restofline,$done) = &getnextgroup($restofline);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $line . $nextline;

             $restofline = $restofline . $nextline;

             ($package,$restofline,$done) = &getnextgroup($restofline);
         }
         else
         {
            die "EOF found whilst scanning argument to \\usepackage on line $startline\n";
         }
      }

      if ($package eq 'csvsort' and $options)
      {
         %hash = &getKeyValues($options,%keyvaldefaults);

         foreach $key (keys(%hash))
         {
            $globalsort{$key}=$hash{$key};
         }
      }

      $_ = $line;
   }

   $restofline = $_;

   if (/^(.*)\\CSVtotabular(.*)$/)
   {
      print OUTPUT "$1$comment";
      $comment="";

      $line = $2;
      $lineno = $.;

      ($csvname,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $line . $nextline;

            ($csvname,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for CVS filename on line $lineno\n";
         }
      }

      $csvname=~s/\\csname (.*)\\roman{(.*)}\\endcsname/$data{$1}[$counter{$2}]/;

      $line = $restofline;

      ($alignment,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $restofline . $nextline;

            ($alignment,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for CVS column alignment on line $lineno\n";
         }
      }

      $line = $restofline;

      ($FIRST,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $restofline . $nextline;

            ($FIRST,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\CSVtotabular third argument on line $lineno\n";
         }
      }

      $line = $restofline;

      ($MIDDLE,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $restofline . $nextline;

            ($MIDDLE,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\CSVtotabular fourth argument on line $lineno\n";
         }
      }

      $line = $restofline;

      ($LAST,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $restofline . $nextline;

            ($LAST,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\CSVtotabular fifth argument on line $lineno\n";
         }
      }

      &csvtotabular($csvname, $alignment, $FIRST, $MIDDLE, $LAST,
                    "tabular");

      print OUTPUT "$restofline\n";
   }
   elsif (/^(.*)\\CSVtolongtable(.*)$/)
   {
      print OUTPUT "$1$comment";
      $comment="";

      $line = $2;
      $lineno = $.;

      ($csvname,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $line . $nextline;

            ($csvname,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for CVS filename on line $lineno\n";
         }
      }

      $csvname=~s/\\csname (.*)\\roman{(.*)}\\endcsname/$data{$1}[$counter{$2}]/;

      $line = $restofline;

      ($alignment,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $restofline . $nextline;

            ($alignment,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for CVS column alignment on line $.\n";
         }
      }

      $line = $restofline;

      ($FIRST,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $restofline . $nextline;

            ($FIRST,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\CSVtolongtable third argument on line $.\n";
         }
      }

      $line = $restofline;

      ($MIDDLE,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $restofline . $nextline;

            ($MIDDLE,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\CSVtolongtable fourth argument on line $.\n";
         }
      }

      $line = $restofline;

      ($LAST,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline = <FILENAME>)
         {
             $line = $restofline . $nextline;

            ($LAST,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\CSVtolongtable fifth argument on line $.\n";
         }
      }

      &csvtotabular($csvname, $alignment, $FIRST, $MIDDLE, $LAST,
                    "longtable");

      print OUTPUT "$restofline\n";
   }
   elsif (/^(.*)\\applyCSVfile(\*)?(.*)$/)
   {
      print OUTPUT "$1$comment";
      $comment="";

      $starred = defined($2);

      $restofline = $3;
      $lineno = $.;

      $restofline = &eatinitialspaces($restofline);
      $restofline = &eatcomments($restofline);

      $startrow = $starred ? 1 : 2;

      if (/^\[([0-9]+)\](.*)/)
      {
         $startrow = $1;

         $restofline = $2;
      }

      $line=$restofline;

      ($csvname,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline= <FILENAME>)
         {
            $line = $restofline . $nextline;

            ($csvname,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\applyCSVfile*[$startrow] first argument on line $lineno\n";
         }
      }

      $csvname=~s/\\csname (.*)\\roman{(.*)}\\endcsname/$data{$1}[$counter{$2}]/;

      $line=$restofline;

      ($body,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline= <FILENAME>)
         {
            $line = $restofline . $nextline;

            ($body,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\applyCSVfile*[$startrow] second argument on line $lineno\n";
         }
      }

      if ($starred)
      {
         &applyCSVfilestar($csvname, $body, $startrow);
      }
      else
      {
         &applyCSVfile($csvname, $body, $startrow);
      }

      print OUTPUT "$restofline\n";
   }
   elsif (/^(.*)\\sortapplyCSVfile(\*)?(.*)$/)
   {
      print OUTPUT "$1$comment";
      $comment="";

      $starred = defined($2);

      $restofline = $3;
      $lineno = $.;

      $restofline = &eatinitialspaces($restofline);
      $restofline = &eatcomments($restofline);

      $line=$restofline;

      $options = '';

      if ($restofline=~/^\s*\[/)
      {
         ($options,$restofline,$done) = &getnextoptionalgroup($line);

         while (!$done)
         {
            if ($nextline= <FILENAME>)
            {
               $line = $restofline . $nextline;

               ($options,$restofline,$done) = &getnextoptionalgroup($line);
            }
            else
            {
               die "EOF found whilst scanning for \\sortapplyCSVfile optional argument on line $lineno\n";
            }
         }
      }

      $line=$restofline;

      ($csvname,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline= <FILENAME>)
         {
            $line = $restofline . $nextline;

            ($csvname,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\sortapplyCSVfile argument on line $lineno\n";
         }
      }

      $csvname=~s/\\csname (.*)\\roman{(.*)}\\endcsname/$data{$1}[$counter{$2}]/;

      $line=$restofline;

      ($body,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline= <FILENAME>)
         {
            $line = $restofline . $nextline;

            ($body,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\sortapplyCSVfile argument on line $lineno\n";
         }
      }

      if ($starred)
      {
         &sortapplyCSVfilestar($csvname, $body, $options);
      }
      else
      {
         &sortapplyCSVfile($csvname, $body, $options);
      }

      print OUTPUT "$restofline\n";
   }
   elsif (/^(.*)\\sortCSVto(tabular|longtable)(.*)$/)
   {
      print OUTPUT "$1$comment";
      $comment="";

      $envname = $2;
      $restofline = $3;
      $lineno = $.;

      $restofline = &eatinitialspaces($restofline);
      $restofline = &eatcomments($restofline);

      $line=$restofline;

      $options = '';

      if ($restofline=~/^\s*\[/)
      {
         ($options,$restofline,$done) = &getnextoptionalgroup($line);

         while (!$done)
         {
            if ($nextline= <FILENAME>)
            {
               $line = $restofline . $nextline;

               ($options,$restofline,$done) = &getnextoptionalgroup($line);
            }
            else
            {
               die "EOF found whilst scanning for \\sortCSVto$envname first argument on line $lineno\n";
            }
         }
      }

      $line=$restofline;

      ($csvname,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline= <FILENAME>)
         {
            $line = $restofline . $nextline;

            ($csvname,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\sortCSVto$envname argument on line $lineno\n";
         }
      }

      $csvname=~s/\\csname (.*)\\roman{(.*)}\\endcsname/$data{$1}[$counter{$2}]/;

      $line=$restofline;

      ($align,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline= <FILENAME>)
         {
            $line = $restofline . $nextline;

            ($align,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\sortCSVto$envname argument on line $lineno\n";
         }
      }

      $line=$restofline;

      ($headrow,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline= <FILENAME>)
         {
            $line = $restofline . $nextline;

            ($headrow,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\sortCSVto$envname argument on line $lineno\n";
         }
      }

      $line=$restofline;

      ($midrows,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline= <FILENAME>)
         {
            $line = $restofline . $nextline;

            ($midrows,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\sortCSVto$envname argument on line $lineno\n";
         }
      }

      $line=$restofline;

      ($lastrow,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline= <FILENAME>)
         {
            $line = $restofline . $nextline;

            ($lastrow,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\sortCSVto$envname argument on line $lineno\n";
         }
      }

      &sortCSVtotabular($csvname,$align,$headrow,$midrows,$lastrow,
         $envname,$options);

      print OUTPUT "$restofline\n";
   }
   elsif (/^(.*)\\csvpiechart(\*)?(.*)$/)
   {
      print OUTPUT "$1$comment";
      $comment="";

      $lineno = $.;

      $starred = defined($2);

      $restofline = $3;

      $restofline = &eatinitialspaces($restofline);
      $restofline = &eatcomments($restofline);

      $startrow= $starred ? 1 : 2;

      $line=$restofline;

      ($options,$restofline,$done) = &getnextoptionalgroup($line);

      while (!$done)
      {
         if ($nextline= <FILENAME>)
         {
            $line = $restofline . $nextline;

            ($options,$restofline,$done) = &getnextoptionalgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\csvpiechart optional argument on line $lineno\n";
         }
      }

      $line=$restofline;

      ($label,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline= <FILENAME>)
         {
            $line = $restofline . $nextline;

            ($label,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\csvpiechart second argument on line $lineno\n";
         }
      }

      $line=$restofline;

      ($csvname,$restofline,$done) = &getnextgroup($line);

      while (!$done)
      {
         if ($nextline= <FILENAME>)
         {
            $line = $restofline . $nextline;

            ($csvname,$restofline,$done) = &getnextgroup($line);
         }
         else
         {
            die "EOF found whilst scanning for \\csvpiechart second argument on line $lineno\n";
         }
      }

      print OUTPUT "\\csvpiesetkeys{$options}%\n";

      if ($options=~m/firstrow\s*=\s*(\d+)/)
      {
         $startrow=$1;
      }

      if ($starred)
      {
         &csvpiechartstar($label, $csvname, $startrow);
      }
      else
      {
         &csvpiechart($label, $csvname, $startrow);
      }

      print OUTPUT "$restofline\n";
   }
   else
   {
      print OUTPUT "$_$comment";
   }
}

sub doapplyCSV{
   local($csvrow, $body, @idx, %HoH) = @_;
   local($row, $entry);

   $counter{'csvrownumber'} = 0;
   print OUTPUT "\\setcounter{csvrownumber}{0}\\relax\n";

   foreach ($row=1; $row <= $csvrow; $row++)
   {
      print ".";

      local($THISROW) = $body;

      $THISROW = &csvSaveEntry($THISROW,$row,%HoH);

      foreach $entry ( keys %{$HoH[$row]})
      {
          $replacementval=$HoH[$row]{$entry};

          $THISROW =~ s/\\insertbyname{$entry}/$replacementval/g;

          $THISROW =~ s/\\insert$entry/$replacementval/g;

          local($column) = $idx{$entry}+1;

          $THISROW =~ s/\\field{(\d+)}/$1eq$column?$replacementval:$&/emg;

          $THISROW = &ifnextrowlast($THISROW, ($row==$csvrow-1));
      }

      print OUTPUT "\\refstepcounter{csvrownumber}\\relax\n$THISROW";
      $counter{'csvrownumber'}++;
   }
}

sub doapplyCSVstar{
   local($body, $csvrow, $numcols, @entry) = @_;
   local($row,$column);

   print OUTPUT "\\setcounter{csvrownumber}{0}\\relax\n";
   $counter{'csvrownumber'} = 0;

   for ($row=0; $row < $csvrow; $row++)
   {
      print ".";

      $THISROW = $body;

      for ($column=0; $column < $numcols; $column++)
      {
          $replacementval=$entry[$row][$column];

          $THISROW =~ s/\\field{(\d+)}/$1eq$column+1?$replacementval:$&/emg;

          $THISROW = &ifnextrowlast($THISROW, ($row==$csvrow-1));
      }

      print OUTPUT "\\refstepcounter{csvrownumber}$THISROW";
      $counter{'csvrownumber'}++;
   }

}

sub getCSVvalues{
   local($CSVFILE, $startrow) = @_;
   local(@idx, %HoH, @fields);

   open FH, $CSVFILE or die "Can't open file '$CSVFILE'\n";

   local ($csvrow)=0;

   while (<FH>)
   {
      if ($. == 1)
      {
         # get header row

         chop;

         @fields = &getEntries($_);

         local($numcols) = 0;

         foreach $field (@fields)
         {
            $idx{$field} = $numcols;

            $numcols++;
         }
      }
      elsif ($. >= $startrow)
      {
         next unless /$separator/;

         chop;

         $csvrow++;
         $rec = {};

         $HoH[$csvrow] = $rec;

         local(@entries) = &getEntries($_);

         local ($i) = 0;

         foreach $e (@entries)
         {
            $rec->{$fields[$i]} = $e;
            $i++;
         }
      }
   }

   close FH;

   return ($csvrow, @idx, %HoH);
}

sub getCSVvaluesstar
{
   local($CSVFILE, $startrow) = @_;
   open FH, $CSVFILE or die "Can't open file '$CSVFILE'\n";

   $csvrow=0;

   while (<FH>)
   {
      if ($. >= $startrow)
      {
         chop;

         @fields = &getEntries($_);

         $numcols = 0;

         foreach $field (@fields)
         {
            $entry[$csvrow][$numcols] = $field;

            $numcols++;
         }

         $csvrow++;
      }
   }

   close FH;

   return ($csvrow, $numcols, @entry);
}

sub csvpiechart
{
   local($value, $CSVFILE, $startrow) = @_;

   print "Converting \\csvpiechart[$options]{$label}{$csvname}\n";
   print OUTPUT "% \\csvpiechart[$options]{$label}{$csvname} converted using csvtools.pl\n";
   print OUTPUT "%>> START INSERT\n";

   # read in data from CSV file
   local($csvrow, @idx, %HoH) = &getCSVvalues($CSVFILE,$startrow);

   # first compute parameters

   print OUTPUT "\\edef\\csvstartang{\\thecsvstartangle}\\relax\n";
   $body = "\\csvsetsegmentparams{\\thecsvrownumber}{$value}\\relax\n";

   &doapplyCSV($csvrow, $body, @idx, %HoH);

   print OUTPUT "\\setcounter{csvstartangle}{\\csvstartang}\\relax\n";

   # compute offsets for cutaway segments

   print OUTPUT "\\ifthenelse{\\equal{\\csvpiecutaways}{}}{}{\\csvcomputeoffsets}\\relax\n";

   # now do the pie chart

   print OUTPUT "\\begin{tikzpicture}\n";

   $body = "\\renewcommand*{\\csvpieinnerlabel}{$csvpieinnerlabel}\n";
   $body .= "\\renewcommand*{\\csvpieouterlabel}{$csvpieouterlabel}\n";
   $body .= "\\csvpiesegment{$value}\n";

   &doapplyCSV($csvrow, $body, %HoH);

   print OUTPUT "\\end{tikzpicture}";
   print OUTPUT "%<< END INSERT\n";
   print "\n";
}

sub csvpiechartstar
{
   local($value, $CSVFILE, $startrow) = @_;

   print "Converting \\csvpiechart*[$options]{$label}{$csvname}\n";
   print OUTPUT "% \\csvpiechart*[$options]{$label}{$csvname} converted using csvtools.pl\n";
   print OUTPUT "%>> START INSERT\n";

   local($csvrow, $numcols, @entry) = &getCSVvaluesstar($CSVFILE, $startrow);

   # first compute parameters

   print OUTPUT "\\edef\\csvstartang{\\thecsvstartangle}\\relax\n";
   $body = "\\csvsetsegmentparams{\\thecsvrownumber}{$value}\\relax\n";

   &doapplyCSVstar($body, $csvrow, $numcols, @entry);

   print OUTPUT "\\setcounter{csvstartangle}{\\csvstartang}\\relax\n";

   # compute offsets for cutaway segments

   print OUTPUT "\\ifthenelse{\\equal{\\csvpiecutaways}{}}{}{\\csvcomputeoffsets}\\relax\n";

   $body = "\\renewcommand*{\\csvpieinnerlabel}{$csvpieinnerlabel}\\relax\n";
   $body .= "\\renewcommand*{\\csvpieouterlabel}{$csvpieouterlabel}\\relax\n";
   $body .= "\\csvpiesegment{$value}\\relax\n";

   print OUTPUT "\\begin{tikzpicture}\n";

   &doapplyCSVstar($body, $csvrow, $numcols, @entry);

   print OUTPUT "\\end{tikzpicture}";
   print OUTPUT "%<< END INSERT\n";
   print "\n";
}

sub applyCSVfile
{
   my ($CSVFILE,$body,$startrow) = @_;

   print "Converting \\applyCSVfile{$CSVFILE}";
   print OUTPUT "\% \\applyCSVfile{$CSVFILE}... converted using csvtools.pl\n";
   print OUTPUT "\%>> START INSERT\n";

   # read in data from CSV file
   local($csvrow, @idx, %HoH) = &getCSVvalues($CSVFILE,$startrow);

   # apply data

   &doapplyCSV($csvrow, $body, @idx, %HoH);

   print "\n";

   print OUTPUT "\%<< END INSERT\n";
}

sub sortapplyCSVfile
{
   my ($CSVFILE,$body,$options) = @_;

   print "Converting \\sortapplyCSVfile{$CSVFILE}";
   print OUTPUT "\% \\sortapplyCSVfile{$CSVFILE}... converted using csvtools.pl\n";
   print OUTPUT "\%>> START INSERT\n";

   # get options
   %hash = &getKeyValues($options) if $options;

   local($startrow) = defined($hash{'firstdataline'})
                    ? $hash{'firstdataline'}
                    : $globalsort{'firstdataline'};

   local ($sortorder) = $hash{'sort'};

   # read in data from CSV file
   local($csvrow, @idx, %HoH) = &getCSVvalues($CSVFILE,$startrow);

   # get sort variable

   local($variable) = defined($hash{'variable'})
                    ? $hash{'variable'}
                    : $globalsort{'variable'};

   local(@rowdata)=();

   print OUTPUT "\\bgroup\n";

   # set field labels

   foreach $entry (keys %{$HoH[1]})
   {
      print OUTPUT "\\setcsvfieldlabel{";
      print OUTPUT ($idx{$entry}+1).'}{';
      print OUTPUT "$entry}%\n";
   }

   print OUTPUT "\\def\\csvsortlist{%\n";

   # construct list

   foreach ($row=1; $row <= $csvrow; $row++)
   {
      print ".";

      local($sortkey) = $variable;

      $sortkey = &csvSaveEntry($sortkey,$row,%HoH);

      foreach $entry ( keys %{$HoH[$row]})
      {
          $replacementval=$HoH[$row]{$entry};

          $sortkey =~ s/\\insertbyname{$entry}/$replacementval/g;

          $sortkey =~ s/\\insert$entry/$replacementval/g;

          local($column) = $idx{$entry}+1;

          $rowdata[$idx{$entry}] = $replacementval;

          $sortkey =~ s/\\field{(\d+)}/$1eq$column?$replacementval:$&/emg;
      }

      print OUTPUT "{$sortkey}{".join($separator,@rowdata)."}";

      print OUTPUT ($row == $csvrow ? "%\n" : ",%\n");
   }

   print OUTPUT "}%\n";

   print OUTPUT "\\sortapplyCSVdata";

   if (defined($sortorder))
   {
      print OUTPUT "[$sortorder]";
   }

   print OUTPUT "{\\csvsortlist}{%\n";

   print OUTPUT "$body}%\n";

   print OUTPUT "\\egroup\n";
   print OUTPUT "\%<< END INSERT\n";

   print "\n";
}

sub sortapplyCSVfilestar
{
   my ($CSVFILE,$body,$options) = @_;

   print "Converting \\sortapplyCSVfile*{$CSVFILE}";
   print OUTPUT "\% \\sortapplyCSVfile*{$CSVFILE}... converted using csvtools.pl\n";
   print OUTPUT "\%>> START INSERT\n";

   # get options
   %hash = &getKeyValues($options) if $options;

   local($startrow) = defined($hash{'sfirstdataline'})
                    ? $hash{'sfirstdataline'}
                    : $globalsort{'sfirstdataline'};

   local ($sortorder) = $hash{'sort'};

   local($csvrow, $numcols, @entry) = &getCSVvaluesstar($CSVFILE, $startrow);

   # get sort variable

   local($variable) = defined($hash{'variable'})
                    ? $hash{'variable'}
                    : $globalsort{'variable'};

   local(@rowdata)=();

   print OUTPUT "\\bgroup\n";

   print OUTPUT "\\def\\csvsortlist{%\n";

   # construct list

   for ($row=0; $row < $csvrow; $row++)
   {
      print ".";

      local($sortkey) = $variable;

      for ($column=0; $column < $numcols; $column++)
      {
          $replacementval=$entry[$row][$column];

          $sortkey =~ s/\\field{(\d+)}/$1eq$column+1?$replacementval:$&/emg;
          $rowdata[$column] = $replacementval;
      }

      print OUTPUT "{$sortkey}{".join($separator,@rowdata)."}";

      print OUTPUT ($row == $csvrow-1 ? "%\n" : ",%\n");
   }

   print OUTPUT "}%\n";

   print OUTPUT "\\sortapplyCSVdata*";

   if (defined($sortorder))
   {
      print OUTPUT "[$sortorder]";
   }

   print OUTPUT "{\\csvsortlist}{%\n";

   print OUTPUT "$body}%\n";

   print OUTPUT "\\egroup\n";

   print "\n";

   print OUTPUT "\%<< END INSERT\n";
}

sub sortCSVtotabular
{
   my ($CSVFILE,$align,$headrow,$midrows,$lastrow,$envname,$options) = @_;

   print "Converting \\sortCSVto${envname}{$CSVFILE}";
   print OUTPUT "\% \\sortCSVto${envname}{$CSVFILE}... converted using csvtools.pl\n";
   print OUTPUT "\%>> START INSERT\n";

   # get options
   %hash = &getKeyValues($options) if $options;

   local($startrow) = defined($hash{'firstdataline'})
                    ? $hash{'firstdataline'}
                    : $globalsort{'firstdataline'};

   local ($sortorder) = $hash{'sort'};

   # read in data from CSV file
   local($csvrow, @idx, %HoH) = &getCSVvalues($CSVFILE,$startrow);

   # get sort variable

   local($variable) = defined($hash{'variable'})
                    ? $hash{'variable'}
                    : $globalsort{'variable'};

   local(@rowdata)=();

   print OUTPUT "\\bgroup\n";

   # set field labels

   foreach $entry (keys %{$HoH[1]})
   {
      print OUTPUT "\\setcsvfieldlabel{";
      print OUTPUT ($idx{$entry}+1).'}{';
      print OUTPUT "$entry}%\n";
   }

   print OUTPUT "\\def\\csvsortlist{%\n";

   # construct list

   foreach ($row=1; $row <= $csvrow; $row++)
   {
      print ".";

      local($sortkey) = $variable;

      $sortkey = &csvSaveEntry($sortkey,$row,%HoH);

      foreach $entry ( keys %{$HoH[$row]})
      {
          $replacementval=$HoH[$row]{$entry};

          $sortkey =~ s/\\insertbyname{$entry}/$replacementval/g;

          $sortkey =~ s/\\insert$entry/$replacementval/g;

          local($column) = $idx{$entry}+1;

          $rowdata[$idx{$entry}] = $replacementval;

          $sortkey =~ s/\\field{(\d+)}/$1eq$column?$replacementval:$&/emg;
      }

      print OUTPUT "{$sortkey}{".join($separator,@rowdata)."}";

      print OUTPUT ($row == $csvrow ? "%\n" : ",%\n");
   }

   print OUTPUT "}%\n";

   if (defined($sortorder))
   {
      print OUTPUT "\\setkeys{csvsort.sty}{sort=$sortorder}";
   }

   print OUTPUT "\\sortCSVdatatotabular";

   print OUTPUT "{$csvrow}{\\csvsortlist}{$align}%\n";
   print OUTPUT "{$headrow}%\n";
   print OUTPUT "{$midrows}%\n";
   print OUTPUT "{$lastrow}%\n";
   print OUTPUT "{$envname}%\n";

   print OUTPUT "\\egroup\n";
   print OUTPUT "\%<< END INSERT\n";

   print "\n";
}

sub csvSaveEntry
{
   my ($STR,$row,%HoH) = @_;

   $rowctr = "csvrownumber";

   while (($pos = index($STR, "\\csvSaveEntry")) > -1)
   {
      $start      = substr($STR,0,$pos);
      $restofline = substr($STR,$pos+13);

      $restofline = &eatcomments($restofline);
      $restofline = &eatinitialspaces($restofline);

      if (substr($restofline,0,1) eq "[")
      {
         if (($i = index($restofline,"]")) > -1)
         {
            $rowctr = substr($restofline, 1, $i-1);

            $restofline = substr($restofline, $i+1);
         }
         else
         {
            die "unmatched [ in \\csvSaveEntry\n";
         }

         $row = $counter{$rowctr};
      }

      ($group,$restofline,$done) = &getnextgroup($restofline);

      if (!$done)
      {
         die "argument to \\csvSaveEntry[$rowctr] not found in >>$restofline<<\n";
      }

      $val = $HoH[$row]{$group};

      $STR = $start . "\\expandafter\\gdef\\csname $group\\roman{$rowctr}\\endcsname{$val}" . $restofline;

      $data{$group}[$row] = $HoH[$row]{$group};
   }

   return $STR;
}

sub applyCSVfilestar
{
   my ($CSVFILE,$body,$startrow) = @_;

   print "Converting \\applyCSVfile*{$CSVFILE}";

   $counter{'csvrownumber'} = 0;
   local($csvrow, $numcols, @entry) = &getCSVvaluesstar($CSVFILE, $startrow);

   &doapplyCSVstar($body, $csvrow, $numcols, @entry);

   print "\n";

   print OUTPUT "\%<< END INSERT\n";
}

sub csvtotabular
{
   my ($CSVFILE,$ALIGN,$START,$MID,$END,$environment) = @_;

   print "Converting \\CSVtotabular{$CSVFILE}";

   print OUTPUT "\% \\CSVtotabular{$CSVFILE}... converted using $0\n";
   print OUTPUT "\%>> START INSERT\n";

   # read in data from CSV file
   local($csvrow, @idx, %HoH) = &getCSVvalues($CSVFILE,2);

   print OUTPUT "\\setcounter{csvrownumber}{0}%\n";
   $counter{csvrownumber} = 0;
   print OUTPUT "\\begin{$environment}{$ALIGN}\n";

   print OUTPUT "$START";

   foreach ($row=1; $row <= $csvrow; $row++ )
   {
      print ".";

      if ($row == $csvrow)
      {
         $THISROW=$END;
      }
      else
      {
         $THISROW=$MID;
      }

      foreach $entry ( keys %{$HoH[$row]})
      {
          $replacementval=$HoH[$row]{$entry};

          $THISROW =~ s/\\insertbyname{$entry}/$replacementval/g;

          $THISROW =~ s/\\insert$entry/$replacementval/g;

          $column = $idx{$entry};

          $THISROW =~ s/\\field{$column}/$replacementval/g;

          $THISROW = &ifnextrowlast($THISROW, ($row==$csvrow-1));
      }

      print OUTPUT "\\refstepcounter{csvrownumber}$THISROW";
      $counter{csvrownumber}++;
   }

   print OUTPUT "\\end{$environment}";
   print OUTPUT "\%<< END INSERT\n";

   print "\n";
}

sub ifnextrowlast
{
   my ($STR, $nextislast) = @_;

   if (($pos = index($STR, "\\ifnextrowlast")) > -1)
   {
      $strbegin = substr($STR,0,$pos);
      $strend   = substr($STR,$pos+14);

      ($firstarg,$strend,$done) = &getnextgroup($strend);

      if (!$done)
      {
         die "Can't find first argument to \\ifnextrowlast\n";
      }

      ($secondarg,$strend,$done) = &getnextgroup($strend);

      if (!$done)
      {
         die "Can't find second argument to \\ifnextrowlast\n";
      }

      if ($nextislast)
      {
         $STR = $strbegin . $firstarg . $strend;
      }
      else
      {
         $STR = $strbegin . $secondarg . $strend;
      }
   }

   return $STR;
}

sub eatcomments
{
   local ($_) = @_;

   if (m/(\\{1}%[^%]*)*(\\*)%/)
   {
      $pre = $`;
      $pre .= $1 if defined($1);
      $pre .= $2;

      local($n) = length($2);

      if ($n%2 == 0)
      {
         $_ = $pre;
      }
   }

   return $_;
}

sub eatinitialspaces
{
   local ($_) = @_;

#   while (substr($STR,0,1) eq " " or substr($STR,0,1) eq "\n"
#         or substr($STR,0,1) eq "\r" or substr($STR,0,1) eq "\t")
#   {
#      $STR = substr($STR,1);
#   }

   s/^(\s)*//sg;

   return $_;
}

sub getnextgroup
{
   my($curline) = @_;

   $curline = &eatcomments($curline);

   $curline = &eatinitialspaces($curline);

   # check to see if current string is blank

   if ($curline!~/[^\s]+/m)
   {
      return ("",$curline,0);
   }

   if (($group = substr($curline,0,1)) ne "{")
   {
       # next group hasn't been delimited with braces
       # return first non-whitespace character

       $curline = substr($curline,1);

       # unless it's a backslash, in which case get command name

       if ($group eq "\\")
       {
          if ($curline=~/([a-zA-Z]+)(^[a-zA-Z].*)/m)
          {
             $group = $1;

             $curline = $2;
          }
          else
          {
             # command is made up of backslash followed by symbol

             $curline=~/([\W_0-9\s\\])(.*)/m;

             $group = $1;

             $curline = $2;
          }
       }

       return ($group,$curline,1);
   }

   my $pos=index($curline, "{");
   my $startpos=$pos;
   my $posopen=0;
   my $posclose=0;

   my $bracelevel = 1;

   my $done=0;

   while (!$done)
   {
      $pos++;

      $posopen = index($curline, "{", $pos);

      # check to make sure it's not a \{

      while ((substr($curline, $posopen-1,1) eq "\\") and ($posopen > 0))
      {
         # count how many backlashes come before it.

         $i = $posopen-1;

         $numbs = 1;

         while ((substr($curline, $i-1,1) eq "\\") and ($i > 0))
         {
            $numbs++;
            $i--;
         }

         # is $numbs is odd, we have a \{, otherwise we have \\{

         if ($numbs%2 == 0)
         {
            last;
         }
         else
         {
            $posopen = index($curline, "{", $posopen+1);
         }
      }

      $posclose= index($curline, "}", $pos);

      # check to make sure it's not a \}

      while ((substr($curline, $posclose-1,1) eq "\\") and ($posclose > 0))
      {
         # count how many backlashes come before it.

         $i = $posclose-1;

         $numbs = 1;

         while ((substr($curline, $i-1,1) eq "\\") and ($i > 0))
         {
            $numbs++;
            $i--;
         }

         # is $numbs is odd, we have a \}, otherwise we have \\}

         if ($numbs%2 == 0)
         {
            last;
         }
         else
         {
            $posclose = index($curline, "}", $posclose+1);
         }
      }

      if (($posopen==-1) and ($posclose==-1))
      {
         $done=1;
      }
      elsif ($posopen==-1)
      {
         $pos=$posclose;

         $bracelevel--;

         if ($bracelevel==0)
         {
            $group = substr($curline, $startpos+1, $pos-$startpos-1);

            $curline = substr($curline, $pos+1);

            return ($group,$curline,1);
         }
      }
      elsif ($posclose==-1)
      {
         $pos=$posopen;

         $bracelevel++;
      }
      elsif ($posopen<$posclose)
      {
         $pos=$posopen;

         $bracelevel++;
      }
      elsif ($posclose<$posopen)
      {
         $pos=$posclose;

         $bracelevel--;

         if ($bracelevel==0)
         {
            $group = substr($curline, $startpos+1, $pos-$startpos-1);

            $curline = substr($curline, $pos+1);

            return ($group,$curline,1);
         }
      }
   }

   # closing brace must be on another line

   return ("", $curline, 0);
}

sub getnextoptionalgroup
{
   my($curline) = @_;

   $curline = &eatcomments($curline);

   $curline = &eatinitialspaces($curline);

   # check to see if current string is blank

   if ($curline!~/[^\s]+/m)
   {
      return ("","",0);
   }

   if (($group = substr($curline,0,1)) ne "[")
   {
      # no optional argument present
      return ("", $curline, 1);
   }

   $posopen = index($curline, "[");

   # search for first occurance of ]

   $posclose = index($curline, "]");

   if ($posclose < 0)
   {
      # closing bracket not on this line
      return ("", $curline, 0);
   }

   # is there a { in the substring?

   $group = substr($curline, $posopen, $posclose+1);
   $contents = $group;
   $restofline = substr($curline, $posclose+$posopen+1);

   while (($idx=index($group, "{")) > 0)
   {
      $post = substr($group, $idx);

      $subgroup="";
      ($subgroup,$remainder,$done) = &getnextgroup($post);

      if ($done)
      {
         $group = $remainder;
      }
      else
      {
         # ] is in a group, so look for next one
         $posclose = index($restofline, "]");

         if ($posclose < 0)
         {
            # closing bracket not on this line
            return ("", $curline, 0);
         }
         else
         {
            $contents .= substr($restofline,0,$posclose);
            $group = $contents;
         }
      }
   }

   $contents=~s/^\[(.*)\]$/$1/;

   return ($contents, $restofline, 1);
}

# get all entries in a row
# modified version of code given in Perl FAQ

sub getEntries{
   local($row)=@_;

   @entries = ();
   push(@entries, $+) while $row =~ m{
       "([^\"\\]*(?:\\.[^\"\\]*)*)"$separator?  # groups the phrase inside the quotes
     | ([^$separator]+)$separator?
     | $separator
   }gx;
   push(@entries, undef) if substr($row,-1,1) eq $separator;

   @entries;
}

# extract key=value pairs from list

sub getKeyValues{
   local($list,%defaults)=@_;
   local(%hash)=();

   # remove leading commas and spaces

   $list=~s/^[\s,]*//;

   while ($list=~/^\s*([^,=]+)\s*/)
   {
      $key=$1;

      $value = '';

      if ($list=~s/^\s*$key\s*=\s*{/{/)
      {
         ($value,$list,$done) = &getnextgroup($list);
      }
      elsif ($list=~s/^\s*$key\s*=\s*([^,]*)\s*//)
      {
         $value = $1;
      }
      elsif ($list=~s/^\s*$key\s*(,|$)//)
      {
         if (defined($defaults{$key}))
         {
            $value = $defaults{$key};
         }
         else
         {
            print "don't know default value for $key key\n";
         }
      }

      $hash{$key} = $value;

      # remove trailing commas

      $list=~s/^[\s,]*//;
   }

   %hash;
}

1;
