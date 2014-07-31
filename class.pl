#!/perl/user/bin/perl



print "Enter the list of file names\n[ALL FILE NAMES SHOULD BE IN A SINGLE COLUMN\n";

$list = <>;
chomp($list);

open (IN, "$list")|| die "can`t open $!";

while ($line = <IN>)
	{
	chomp($line);
	print "$line\n";
	push (@files, $line);
	}

open (CL, ">$list-cl.csv");
open (FD, ">$list-fd.csv");
open (FM, ">$list-fm.csv");
open (SU, ">$list-su.csv");
#print  "##CLASS##\t  ##FOLD##\t  ##SUPERFAMILY##\t  ##FAMILY##\n";
for ($i=0; $i<= $#files; $i++)
	{
	$name = $files[$i];
	print "name= $name\n";


	mainprog($name);
	}


sub mainprog {

		$class = 'unknown';
		$fold = 'unknown';
		$supfam='unknown';
		$fam ='unknown';
		$in = $_[0];
		print "in=$in\n";
		open (F, "$in");
		#open (O, ">$in.csv");
		$in =~ s/\.txt//;
		print CL "\n$in\,";
		print FD "\n$in\,";
		print FM "\n$in\,";
		print SU "\n$in\,";

		while ($ll = <F>)
			{
			#print "ll= $ll\n";
			if ($ll =~ /Class:/)
				{
				print "ll=$ll\n";
				$ll =~ s/Class://;
				$ll =~ s/\s+//g;
				$class = $ll;
				print CL "$class\,";
				}
			elsif ($ll =~ /Fold:/)
				{
				print "ll=$ll\n";
				$ll =~ s/Fold://;
				$ll =~ s/\s+//g;
				$fold = $ll;
				print FD "$fold";
				}
			elsif ($ll =~ /Superfamily:/)
				{
				print "ll=$ll\n";
				$ll =~ s/Superfamily://;
				$ll =~ s/\s+//g;
				$supfam = $ll;
				print SU "$supfam";
				}
			elsif ($ll =~ /Family:/)
				{
				print "ll=$ll\n";
				$ll =~ s/Family://;
				$ll =~ s/\s+//g;
				$fam = $ll;
				print FM "$fam";
				}

			}
			print OP "\n";
	}#ends subroutine


