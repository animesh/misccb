#!/usr/bin/perl
# fssearchtl.pl-  Feasibility Search (Thank You Letter)
# file path /var/www/cgi-bin


use CGI qw(:standard escapeHTML);
use warnings;

use Qui::QuiDB;
###use Qui::QuiMenu;


#Assigning global variables


my ($dbh) = Qui::QuiDB::connect();   				#Assigning database handler for MySQL
my ($query) = new CGI;
my ($JSCRIPT) = Qui::QuiDB::jscript();				#calling for javascript fuctionality
my ($filepath) = Qui::QuiDB::get_file_path();
my ($jspath) = Qui::QuiDB::get_js_path ();			#returns file path with IP where js files are kept
my $styl ={-style => 'font-family: arial;font-size: 12'};

my ($cookieuid) = cookie('uid'); #added for handling sessions/cookies
#my ($rflag, $wflag, $mflag, $eflag);

my $deletebutton=param("deletebutton") if param("deletebutton");
my $dconfirm=param("dconfirm") if param("dconfirm");
print $query->header ();

	##Added for menu###########################

	print "<style>\n";
	print "a:hover{color:ff0033}\n";
	print "a{text-decoration:none;}\n";
	print "</style>\n";

	###########################################

    	print $query->start_html(-bgcolor => "#FFF8DF", -title=>'Feasibility Study - Search', -onLoad=>"if (document.frm.fid){document.frm.fid.focus();}");
	##Added for menu###########################
	print "<script type='text/javascript'>\n";
	print "function deletefs(id,a)\n";
	print "{                         \n";
	print "if(a == 'Y'){	\n";
		print "if (confirm('Are you sure you want to delete this feasiblity study('+ id+')' ))	\n";
		print "{	\n";
			print "document.frm.dconfirm.value='Yes'\n";
			print "document.frm.deletebutton.value=id\n";
			print "document.frm.submit();	\n";
		print "} \n";
		print "else{	\n";
	
		print "} \n";		
	print "} \n";
	
	print "else{alert('You are not authorised to carry out this operation')} \n";	
	
	print "} \n";
	print "</script>\n";	

	print "<script type='text/javascript'>\n";
	print "function closesearch()\n";
	print "{                         \n";
	print "	var val=getValues();     \n";
	print "	if (document.frm.fss.value != '') val= document.frm.fss.value  + \"|\" + val;";
	print "		if ( val=='-1')         \n";
	print "		{                          \n";
	print "			alert(\"Please select at least one item before proceeding!\"); \n";
	print "			return;     \n";
	print "		}     \n";
	print "	window.opener.document.frm.fss.value=val;\n";
	print "	window.opener.document.frm.submit();     \n";
	print "	window.close();     \n";
	print "}   \n";
	print "</script>\n";

	print "<script type='text/javascript'>\n";
	print "function Go(){return}</script>\n";
	print "<script type='text/javascript' src='".$jspath."exmplmenu_var.js'></script>\n";
	print "<script type='text/javascript' src='".$jspath."menu_com.js'></script>\n";
	print "<script type='text/javascript' src='".$jspath."fssearch.js'></script>\n";
	print "<TABLE cellSpacing=0 cellPadding=0 width=\"100%\" border=0>\n";
	print "<TR>\n";
    	print "<TD><img src='".$jspath."logo.jpg'>\n";
	print "</TD>\n";
	print "</TR>\n";
	print "</TABLE>\n";

	###########MAIN PROGRAM

	my ($fid,$sname,$tid,$inid,$product,$ocontact,$duedate,$fss,$bstudy,$ng,$contactfname,$contactlname);


	$fss=Qui::QuiDB::collapse_whitespace(Qui::QuiDB::trim(param("fss")));
	$ng=Qui::QuiDB::collapse_whitespace(Qui::QuiDB::trim(param("ng")));

	$fid=Qui::QuiDB::collapse_whitespace(Qui::QuiDB::trim(param("fid")));
	$sname=Qui::QuiDB::collapse_whitespace(Qui::QuiDB::trim(param("sname")));
	$tname=Qui::QuiDB::collapse_whitespace(Qui::QuiDB::trim(param("tid")));
	$lsttname=$tname; $lsttname=~s/'/\\'/g;
	$tid=return_a_field($dbh,"select tid from tmaster where tname='$lsttname'");
	$iname=Qui::QuiDB::collapse_whitespace(Qui::QuiDB::trim(param("inid")));
	$lstiname=$iname; $lstiname=~s/'/\\'/g;
	$inid=return_a_field($dbh,"select inid from imaster where iname='$lstiname' and tid='$tid'");
	$product=Qui::QuiDB::collapse_whitespace(Qui::QuiDB::trim(param("product")));
	$ocontact=Qui::QuiDB::collapse_whitespace(Qui::QuiDB::trim(param("ocontact")));
	$contactfname=Qui::QuiDB::collapse_whitespace(Qui::QuiDB::trim(param("contactfname")));
	$contactlname=Qui::QuiDB::collapse_whitespace(Qui::QuiDB::trim(param("contactlname")));
	$duedate=Qui::QuiDB::collapse_whitespace(Qui::QuiDB::trim(param("duedate")));
	$bstudy=Qui::QuiDB::collapse_whitespace(Qui::QuiDB::trim(param("bstudy")));
#	print p("UID is $cookieuid");
	if ($duedate)
	{
	$duedate = get_db_date($duedate);
	}
	if (($deletebutton) && ($dconfirm eq "Yes"))
	{
		my $deletefs1="delete from fcstudy where fid='$deletebutton'";
		my $deletefs2="delete from fsform where fid='$deletebutton'";
		my $deletefs3="delete from fstudy where fid='$deletebutton'";
		my $deletefs1r = $dbh->prepare($deletefs);
		$deletefs1r->execute ();
		my $deletefs2r = $dbh->prepare($deletefs2);
		$deletefs2r->execute ();
		my $deletefs3r = $dbh->prepare($deletefs3);
		$deletefs3r->execute ();
		show_dr_search();
		exit(0);
	}
	if($bstudy eq "Yes")
	{
		$bstudy="Y";
	}
	elsif ($bstudy eq "No")
	{
		$bstudy="N";
	}

	if (param("choice") eq "")
	{
   		show_dr_form();
	}
	elsif (param("choice") eq "Search")
	{

		show_dr_search();
	}

 	print $query->end_html ();

sub show_dr_search
{

	my(@placeholder, @condition);
	my($where,$flag);
	my($sth, $sthnxt);

	my @fstudy=split /\|/, $fss;

	my $temp="";
	my $cnt=0;
	foreach $fx (@fstudy)
	{
		if($cnt > 0){ $temp=$temp.",";}
		$temp=$temp."'".$fx."'";
		$cnt++;
        }

	if (defined ($fid) && $fid ne "")
	{
		push (@condition, "fid like ?");
		push (@placeholder, '%'.$fid.'%');
	}

	if (defined ($sname) && $sname ne "")
	{
		push (@condition, "sname like ?");
		push (@placeholder, '%'.$sname.'%');
	}

	if (defined ($tid) && $tid ne "Any")
	{
		push (@condition, "tid like ?");
		push (@placeholder, '%'.$tid.'%');
	}
       if (defined ($inid) && $inid ne "Any")
	{
		push (@condition, "inid like ?");
		push (@placeholder, '%'.$inid.'%');
	}
       if (defined ($product) && $product ne "")
	{
		push (@condition, "product like ?");
		push (@placeholder, '%'.$product.'%');
	}
	if (defined ($ocontact) && $ocontact ne "")
	{
		push (@condition, "ocontact like ?");
		push (@placeholder, '%'.$ocontact.'%');
	}

       if (defined ($duedate) && $duedate ne "")
	{
		push (@condition, "duedate = ?");
		push (@placeholder, $duedate);
	}

	if (defined ($bstudy) && $bstudy ne "Any")
	{
		push (@condition, "bstudy like ?");
		push (@placeholder, '%'.$bstudy.'%');
	}
	
	if ((defined (param("contactfname")) && param("contactfname") ne "" ) or (defined (param("contactlname")) && param("contactlname") ne "" ))
	{
		
		$pm=param("contactfname");
		$lstpm=$lm; $lstpm=~s/'/\\'/g;
		$lm=param("contactlname");
		$lstlm=$lm; $lstlm=~s/'/\\'/g;
		$sth = $dbh->prepare("select sid from staffmast where fname like '%$lstpm%' and lname like '%$lstlm%'");
		
		$sth->execute();
		if ($sth->rows() != 0) {
			undef (@condition1);
			
			while (@xx=$sth->fetchrow_array())
			{
				push (@condition1, "sid like ?");
				#push (@condition1, "prjmgrid like '%".$xx[0]."%'");
				push (@placeholder, @xx);

			}
		}
		else
		{
			if ($pm)
			{
			push (@condition, "sid like ?");
			push (@placeholder,$pm)
			}
			
			if ($lm)
			{
			push (@condition, "sid like ?");
			push (@placeholder,$lm)
			}
		}

		$sth->finish ();

	}
	
	
	$cnt=0;
	$str_ph;
	foreach $xx (@placeholder)
	{
      		if ($cnt > 0) { $str_ph .= ","; }
       		$str_ph .= $xx;
      		$cnt++;
	}

	$where = " WHERE " . join (" AND ", @condition) if @condition;
	
	if ($where eq "")
	{
	$where .=" WHERE " .  join(" OR ",@condition1) if @condition1;
	}
	else
	{
	$where1 .=" AND ".join(" OR ",@condition1) if @condition1;
	$where .= $where1;
	
	}
	
	$where1 = "WHERE " . join (" AND ", @condition1) if @condition1;
	

	if ($fid eq "")
	{
		$fid='Any';
	}
	if ($sname eq "")
	{
		$sname='Any';
	}
	if ($product eq "")
	{
		$product='Any';
	}
	if ($ocontact eq "")
	{
		$ocontact='Any';
	}
	
	if ($pm eq "")
	{
		$pm='Any';
	}
	if ($lm eq "")
	{
		$lm='Any';
	}

	if ($duedate eq "")
	{
		$duedate='Any';
	}
	else
	{
		$duedate=get_disp_date($duedate);
	}	
	if ($bstudy eq "")
	{
		$bstudy='Any';
	}


	
	
#	$where = "WHERE " . join(" AND ", @condition) if @condition;
	my $tnameq="SELECT tname FROM tmaster WHERE tid='$tid'";
	 $stht=$dbh->prepare($tnameq);$stht->execute();
	 $dtname=$stht->fetchrow_array();
	 my $dnameq="SELECT iname FROM imaster WHERE inid='$inid' and tid='$tid'";
	 $sthd=$dbh->prepare($dnameq);$sthd->execute();
	 $diname=$sthd->fetchrow_array();
	if ($tid eq "")
	{
		$dtname = "Any";
	}

	if ($inid eq "")
	{
		$diname = "Any";
	} 
	push (@searchcriteria, Tr (td({-bgcolor => "#0000FF", -colspan => "12"}, 
				span({-style => 'font-family: arial;color : #FFFFFF;font-size: 12'},
				#strong("Feasibility Search Criteria")))));
				strong("Predefined Feasibility Study Search (Thank You Letters) Criteria")))));
	push(@searchcriteria,Tr  (
				td({-colspan => "12"},
					span({-style => 'font-family: arial;font-size: 12'}, "&nbsp" , "Reference Id = " , $fid),
					br(),
					span({-style => 'font-family: arial;font-size: 12'}, "&nbsp" , "Sponsor = " , $sname),
					br(),
					span({-style => 'font-family: arial;font-size: 12'}, "&nbsp" , "Therapeutic Area = " , $dtname),
					br(),
					span({-style => 'font-family: arial;font-size: 12'}, "&nbsp" , "Indication= " , $diname),
					br(),
					span({-style => 'font-family: arial;font-size: 12'}, "&nbsp" , "Product = " , $product),
					br(),
					span({-style => 'font-family: arial;font-size: 12'}, "&nbsp" , "Quintiles Contact / Q Office = " , $ocontact),					
					br(),
					span({-style => 'font-family: arial;font-size: 12'}, "&nbsp" , "Local BD Contact First Name   = " , $pm),
					br(),
					span({-style => 'font-family: arial;font-size: 12'}, "&nbsp" , "Local BD Contact Last Name   = " , $lm),
					br(),
					span({-style => 'font-family: arial;font-size: 12'}, "&nbsp" , "Due Date= " , $duedate),
					br(),
					span({-style => 'font-family: arial;font-size: 12'}, "&nbsp" , "Blind Study = " , $bstudy),
					br()
				)
			));





	if(@fstudy )
	{
		if($where ne ""){ $where = $where . " and ";} else {$where = " where ";}
		$where= $where . " fid not in (" . $temp . ")";
	}

	

	my ($stmt);

         $stmt = "SELECT fid,sname,tid,sbid,phase,inid,dtrecd,duedate,bstudy,rsummary,sid FROM fstudy $where";

	 
	
	my $sth = $dbh->prepare($stmt);
#	 print p($stmt,@placeholder);
	my ($count) = 0;
	my (@row);
	$sth->execute (@placeholder);
	
#	$flag = 1;

#       push(@searchcriteria,Tr (td({-colspan => "12"},span({-style => 'font-family: arial;font-size: 12'}, "&nbsp"  ,$sth->rows(), " Feasiblity search record(s) found"))));

	if ($ng ne "")
	{
		print $query->br();
		print $query->br();

		push (@row,@searchcriteria);
		push (@row, Tr (td({-bgcolor => "#02BEA2", -colspan => "9"}, "&nbsp",
				span({-style => 'font-family: arial;font-size: 12'},
				strong("Feasibility Study Search Results  ")))));

        	$str = td ({-bgcolor => "#02BEA2" ,align=>"left"},"&nbsp", "<input type=checkbox name=chkall value='' onclick='checkAll()'>",span({-style => 'font-family: arial;align: center;font-size: 12'}, b("Mark All")));
		$str .= td ({-bgcolor => "#02BEA2" ,align=>"left"},"&nbsp", span({-style => 'font-family: arial;align: center;font-size: 12'}, b("Reference ID")));
		$str .= td ({-bgcolor => "#02BEA2",align=>"center"},"&nbsp", span({-style => 'font-family: arial;align: center;font-size: 12'}, b("Sponsor")));
		$str .= td ({-bgcolor => "#02BEA2",align=>"center"},"&nbsp", span({-style => 'font-family: arial;align: center;font-size: 12'}, b("Therapeutic Area")));
		$str .= td ({-bgcolor => "#02BEA2",align=>"center"},"&nbsp", span({-style => 'font-family: arial;align: center;font-size: 12'}, b("Indication")));
        	$str .= td ({-bgcolor => "#02BEA2",align=>"center"},"&nbsp", span({-style => 'font-family: arial;align: center;font-size: 12'}, b("Product")));
        	$str .= td ({-bgcolor => "#02BEA2",align=>"center"},"&nbsp", span({-style => 'font-family: arial;align: center;font-size: 12'}, b("Quintiles Contact / Q Office")));
        	$str .= td ({-bgcolor => "#87CEEB",-align=>"center",-valign=>"center"},span({-style => 'font-family: arial;align: center;font-size: 12'}, b("Local BD Contact First Name ")));
        	$str .= td ({-bgcolor => "#87CEEB",-align=>"center",-valign=>"center"},span({-style => 'font-family: arial;align: center;font-size: 12'}, b("Local BD Contact Last Name ")));
		$str .= td ({-bgcolor => "#02BEA2",align=>"center"},"&nbsp", span({-style => 'font-family: arial;align: center;font-size: 12'}, b("Due Date")));
		$str .= td ({-bgcolor => "#02BEA2",align=>"center"},"&nbsp", span({-style => 'font-family: arial;align: center;font-size: 12'}, b("Blind Study")));

		push (@row, $str);
		

		while (my $myref = $sth->fetchrow_hashref ())
		{

			push (@row, Tr ());

			$str  = td ("&nbsp", "<input type=checkbox name=fs value='$myref->{fid}' onclick='verifyCheckAll()'>");
			$str .= td ("&nbsp", span({-style => 'font-family: arial;font-size: 12'}, $myref->{fid}));
			$str .= td ("&nbsp", span({-style => 'font-family: arial;font-size: 12'}, $myref->{sname}));
#			$str .= td ("&nbsp", span({-style => 'font-family: arial;font-size: 12'}, $myref->{phase}));
			$str .= td ("&nbsp", span({-style => 'font-family: arial;font-size: 12'}, return_a_field($dbh, "select tname from tmaster where tid='" . $myref->{tid} . "'" )));
			$str .= td ("&nbsp", span({-style => 'font-family: arial;font-size: 12'}, return_a_field($dbh,"select iname from imaster where inid='" . $myref->{inid} . "' and tid='" . $myref->{tid} . "'")));
                        $str .= td ("&nbsp", span({-style => 'font-family: arial;font-size: 12'}, $myref->{product}));
                        $str .= td ("&nbsp", span({-style => 'font-family: arial;font-size: 12'}, $myref->{ocontact}));
			$str .= td ("&nbsp", span({-style => 'font-family: arial;font-size: 12'}, get_disp_date($myref->{duedate})));
			$str .= td ("&nbsp", span({-style => 'font-family: arial;font-size: 12'}, $myref->{bstudy}));

			push (@row, $str);
			++$count;
		}
		if ($count == 0)
		{
			
			print "<span><font style='font-family: arial;font-size: 12'><strong>No feasibility study is found based on the above search criteria</strong></span>";
		} else
		{
			
			push (@row, Tr (td({-colspan => "7"}, "&nbsp","<input type=button value='Submit' name=choice onclick=\"closesearch();\">" )));
			print $query->start_form(-action => url (), -name => "frm");
			print table ({-width => "100%", -align => "left", -cellpadding => "1", -border => "1", -rules=>"all"},

			@row);
			print "<input type=hidden name=fss value='$fss'>";
			print $query->end_form();
		}
	}
	else
	{
	
		push(@str,Tr  (
			td({-bgcolor => "#0000FF",-colspan=>"12"},
				#span({-style => 'font-family: arial;color : "#FFFFFF";font-size: 12;'}, b("Feasibility Search Results:")))
				span({-style => 'font-family: arial;color : "#FFFFFF";font-size: 12;'}, b("Predefined Feasibility Study Search Results (Thank You Letters)"))),
			Tr(td({-colspan=>"12"},span({-style => 'font-family: arial;font-size: 12;'}, "The search results are refined by listing only the studies of which not all doctors (who have returned feasibility study forms) have received 'Thank You Letter'")))
		));
##marking
		my $fidcount=0;
		while (my $myref = $sth->fetchrow_hashref ())
		{
												
			my $checkquery="select fid from fsform where fid='$myref->{fid}' and dfrecd !='0000-00-00' and lsent not like '%1%'";
	 		
			
			my $checkprepare = $dbh->prepare($checkquery);
		
			$checkprepare ->execute ();
	
			while ( $fsref = $checkprepare->fetchrow_hashref ())
			{	
				my $sb="";
				if($myref->{sbid} eq 'Y')
				{
					$sb="Yes";
				}
				elsif($myref->{sbid} eq 'P')
				{
					$sb="Pending";
				}
				elsif($myref->{sbid} eq 'N')
				{
					$sb="No";
				}

				$rsu=$myref->{rsummary};
				my (@rsu1);
				my $len=length($myref->{rsummary});
				my (@newarray);
				
				if ($len>0)
				{
				
					for(my $chumma=0;$chumma<$len;$chumma++)
					{
						if (($chumma%100) eq 0 )
						{
							push(@newarray,"<br>");
						}
						$new=substr($rsu,$chumma,1);
						push(@newarray,$new);
			
					}
				}

				my $a=adminfetch();
				$delbutton="<input type=button name=\"Button1\"  Label=\"X\"  value=\"x\"  style=\"background-color: red;font-family: arial;font-size: 9; font-style: bold;color :black;\" onclick=\"deletefs('$myref->{fid}','$a');\">";
				$fidcount++;
		$sponsortemp=$myref->{sname};
		if ($sponsortemp eq "")
		{
			$sponsortemp = "&nbsp";
		}
		$phasetemp=$myref->{phase};
		if ($phasetemp eq "")
		{
			$phasetemp="&nbsp";
		}
		$drugtemp=return_a_field($dbh,"select product from fstudy where fid='$myref->{fid}'");
		if ($drugtemp eq "")
		{
			$drugtemp="&nbsp";
		}
		$datereceivedtemp=get_disp_date($myref->{dtrecd});
		if ($datereceivedtemp eq "")
		{
			$datereceivedtemp="&nbsp";
		}
		$sbtemp=$sb;
		if ($sbtemp)
		{
			$sbtemp="&nbsp";
		}
		$indicationtemp=return_a_field($dbh, " select iname from imaster where inid='$myref->{inid}' and tid='$myref->{tid}'");
		if ($indicationtemp eq "")
		{
			$indicationtemp = "&nbsp";
		}
		push(@str,Tr(
			td ({-align=>"right",-bgcolor => "#87CEEB" ,-width=>"12%"},$align, span({-style => 'font-family: arial;font-size: 12'}, "Ref ID :")),
			td ({-width=>"15%"},$align, span({-style => 'font-family: arial;font-size: 12'}, $delbutton,a({-href => "FeasibilityEntry.pl?fid=$myref->{fid}&$myref->{sbid}&$myref->{bstudy}"},u($myref->{fid})))),
			td ({-align=>"right",-bgcolor => "#87CEEB",-width=>"10%"},$align, span({-style => 'font-family: arial;font-size: 12'}, "Sponsor :")),
			td ({-width=>"20%"},$align, span({-style => 'font-family: arial;font-size: 12'},$sponsortemp )),
			td ({-align=>"right",-bgcolor => "#87CEEB",-width=>"8%"},$align,  span({-style => 'font-family: arial;font-size: 12'}, "Phase :")),
			td ({-width=>"5%"},$align, span({-style => 'font-family: arial;font-size: 12'}, $phasetemp)),
			td ({-align=>"right",-width=>"5%",-bgcolor => "#87CEEB"},$align,  span({-style => 'font-family: arial;font-size: 12'}, "Drug :")),
			td ({-width=>"20%"},$align, span({-style => 'font-family: arial;font-size: 12'},$drugtemp))
		   ));

		push(@str,Tr (
			td ({-align=>"right",-bgcolor => "#87CEEB",-width=>"12%"},$align, span({-style => 'font-family: arial;font-size: 12'}, "Date Received :")),
			td ($align,  span({-style => 'font-family: arial;font-size: 12'},$datereceivedtemp )),
			#td ($align, "&nbsp", span({-style => 'font-family: arial;font-size: 12'}, ($myref->{dtrecd}))),
			td ({-align=>"right",-bgcolor => "#87CEEB"},$align, "&nbsp", span({-style => 'font-family: arial;font-size: 12'}, "Successful Bid :")),
			td ($align, span({-style => 'font-family: arial;font-size: 12'}, $sbtemp)),
			td ({-align=>"right",-bgcolor => "#87CEEB",-colspan => "1"},$align, "&nbsp", span({-style => 'font-family: arial;font-size: 12'}, "Indication :")),
			td ({-colspan => "3"},$align, span({-style => 'font-family: arial;font-size: 12'}, $indicationtemp))
		   ));
		   
#		  push(@str,tr (
#			td ({-align=>"right",-width=>"12%",-bgcolor => "#87CEEB"},$align, span({-style => 'font-family: arial;font-size: 12'}, "Summary of Results")),
#			td ({-width=>"50%"},$align, "&nbsp", span({-style => 'font-family: arial;font-size: 12'},$myref->{rsummary}))
#		  ));

		  push(@str,Tr (
			td ({-colspan=>1,-align=>"right",-width=>"12%",-bgcolor => "#87CEEB"},$align, span({-style => 'font-family: arial;font-size: 12'}, "Summary of Results:")),
#			td ({-colspan=>11,-width=>"88%"},$align, "&nbsp", span({-style => 'font-family: arial;font-size: 12'},@newarray))
			td ({-colspan=>11,-width=>"88%"},$align, span({-style => 'font-family: arial;font-size: 12'},"<textarea rows=5 cols=150 style = 'font-family: arial;font-size: 12' name=aa readonly>$myref->{rsummary}</textarea>"))		  
		  ));


				 #push(@str,Tr(
				#	td ({-colspan => "8"},$align, "&nbsp", span({-style => 'font-family: arial;font-size: 12'},$myref->{rsummary}))
				 #  ));
				#	push(@str,
				#	td ({-colspan => "8"},$align, "&nbsp", span({-style => 'font-family: arial;font-size: 12'},$myref->{rsummary}))
				 # );
			push(@str,"<tr height=2% bgcolor=white><td></td></tr>");
			last;	
			}
		}
		print "<br/>";
            	push(@searchcriteria,Tr (td({-colspan => "12"},span({-style => 'font-family: arial;font-size: 12'}, "&nbsp"  ,$fidcount, " Feasiblity record(s) found"))));

		print $query->start_form(-action => url (), -name => "frm");
		if ($fidcount > 0)
			{
			print "<table width=\"100%\"><tr><td>";
			print table ({-width => "100%", -align => "left", -cellpadding => "1", -border => "1", -rules=>"all"},
			@searchcriteria,
			@str,
			);
			print "</td></tr>";

			print "<tr><td>";
			print "<a class='txt' href=\"fssearchtl.pl\" style = 'font-family: arial;font-size: 12'><u>Predefined Feasibility Study Search (Thank You Letters)</u>";
			print "</td></tr></table>";
		}
		else
		{
			print "<table width=100%><tr><td>";
			print table ({-width => "100%", -align => "left", -cellpadding => "1", -border => "1", -rules=>"all"},
			@searchcriteria,
			@str
			);
			print "</td></tr>";
			print "<tr><td>";
			print "<span><font style='font-family: arial;font-size: 12'><strong>No feasibility study is found based on the above search criteria</strong></span>";
			print "</td></tr>";
			print "<tr><td>";
			print "<a class='txt' href=\"fssearchtl.pl\" style = 'font-family: arial;font-size: 12'><u>Predefined Feasibility Study Search (Thank You Letters)</u>";
			print "</td></tr></table>";

		}
		print "<input type=hidden name=deletebutton value=\"\">";
		print "<input type=hidden name=dconfirm value=\"\">";
		print $query->end_form();


	}
}
#@ SHOW_DR_SEARCH





sub show_dr_form
{


                $temp="";



		if ($tid ne ""){ $temp = "where tid='" . $tid . "'"; } else { $temp=""; }

		$sq1 = "SELECT DISTINCT tname from tmaster ORDER BY tname";
		$sq2 = "SELECT DISTINCT iname from imaster " . $temp . " ORDER BY iname";

		@tname_val_ref = return_field($dbh, $sq1); #Therapeutic name
		if($#tname_val_ref > -1 and  $tname_val_ref[$#tname_val_ref] ne ""){unshift(@tname_val_ref, "Any");}
		@iname_val_ref = return_field($dbh, $sq2); #Indication name
		if($#iname_val_ref > -1 and  $iname_val_ref[$#iname_val_ref] ne ""){unshift(@iname_val_ref, "Any");}

                push(@$tname_val_ref,"");
                push(@$iname_val_ref,"");
		 if ($tid eq "")
                {
                @iname_val_ref="Any";
                }

		print hr (); print br ();

		print start_form (-action => url (), -name => "frm"),

		table   ({-width => "100%", -cellspacing => "2", -cellpadding => "1", -border => "1", -rules=>"all"},

			Tr (td({-bgcolor => "#0000FF", -colspan => "7"}, 
				span({-style => 'font-family: arial;color="#FFFFFF";font-size: 12'},
				#strong("Feasibility Study Search")))
				strong("Predefined Feasibility Study Search (Thank You Letters)")))
			),
			
			Tr (
			
				td({-colspan => "7"},span({-style => 'font-family: arial;font-size: 14'},"Indicate the criteria below to search for feasibility studies. The search results is further refined by listing only the studies of which not all doctors (who have returned feasibility study forms) have received 'Thank You Letter'"))
			
			),

			Tr (
				td ({-width=>"20%",-align=>"right",-bgcolor=>"#87CEEB",-valign => "top"}, "&nbsp",
					span({-style => 'font-family: arial;font-size: 12'},"Reference ID:")),
				 td ({-width=>"80%"},textfield(-name => "fid", -size => "40", -maxlength => "20", -value => "$fid")),
			),
			Tr (
				td ({-align=>"right",-bgcolor=>"#87CEEB",-valign => "top"}, "&nbsp",
					span({-style => 'font-family: arial;font-size: 12'},"Sponsor:")),
				td(textfield (-name => "sname", -size => "60", -maxlength => "50", -value => "$sname"))
			),
			Tr (

				td ({-align=>"right",-bgcolor=>"#87CEEB",-valign => "top"}, "&nbsp",
					span({-style => 'font-family: arial;font-size: 12'},"Therapeutic Area:")),
				td(popup_menu(-name => "tid", -values => \@tname_val_ref, -default => "$tid", -onchange =>"document.frm.submit()"))

			),
			Tr (

				td ({-align=>"right",-bgcolor=>"#87CEEB",-valign => "top"}, "&nbsp",
					span({-style => 'font-family: arial;font-size: 12'},"Indication:")),
				td(popup_menu(-name => "inid", -values => \@iname_val_ref, -default => ""))

			),
			Tr (

				td ({-align=>"right",-bgcolor=>"#87CEEB",-valign => "top"}, "&nbsp",
					span({-style => 'font-family: arial;font-size: 12'},"Product:")),
				td(textfield(-name => "product", -size => "60", -maxlength => "50", -value => "$product"))

			),
			Tr (

				td ({-align=>"right",-bgcolor=>"#87CEEB",-valign => "top"}, "&nbsp",
					span({-style => 'font-family: arial;font-size: 12'},"Quintiles Contact / Q Office:")),
				td(textfield(-name => "ocontact", -size => "60", -maxlength => "50", -value => "$ocontact"))

			),
			
			
			Tr (
			
				td ({-align=>"right",-bgcolor=>"#87CEEB",-valign => "top"},"&nbsp",
				        span({-style => 'font-family: arial;font-size: 12'},"Local BD Contact First Name:")),
				td(textfield (-name => "contactfname",	-value => "$contactfname", -size => 30))
			),
			
			Tr (
			
				td ({-align=>"right",-bgcolor=>"#87CEEB",-valign => "top"}, "&nbsp",
				        span({-style => 'font-family: arial;font-size: 12'},"Local BD Contact Last Name:")),
				td(textfield (-name => "contactlname",	-value => "$contactlname", -size => 30))
			),	
			
			Tr (

				td ({-align=>"right",-bgcolor=>"#87CEEB",-valign => "top"}, "&nbsp",
					span({-style => 'font-family: arial;font-size: 12'},"Due Date:")),
				td(textfield(-name => "duedate", -size => "60", -maxlength => "50", -value => "$duedate"))

			),
			Tr (

				td ({-align=>"right",-bgcolor=>"#87CEEB",-valign => "top"}, "&nbsp",
					span({-style => 'font-family: arial;font-size: 12'},"Blind Study:")),
				td(popup_menu(-name => "bstudy", -values => ['Any','Yes','No'], -default => "$bstudy" ))

			),

		),
	br (),
	submit (-name => "choice",-style=>'background-color:"#87CEEB";font-family: arial;font-size: 14;', -value => "Search");
	print "<input type=hidden name=fss value='$fss'>";
	print "<input type=hidden name=ng value='$ng'>";
	print end_form ();


}
#@ SHOW_STAFF_NAME


sub return_a_field
{

	my($dbh, $query) = @_;
	my($sth);
	my($val);
	$sth = $dbh->prepare ($query) or die "Error";
	$sth->execute ();
	if($sth->rows() == 0)
	{
		$sth->finish();
		return;
	}
	else
	{
		my @row = $sth->fetchrow_array ();
		return ($row[0]);
	}
	$sth->finish ();
}

#sub get_disp_date
#{
#	my $paramdate = shift;
#	my $calcdate1;
#	if(defined $paramdate and $paramdate1 ne "")
#	{
#		my @date=split(/-/,$paramdate);
#		$calcdate=$date[2]."/".$date[1]."/".$date[0];
#	}
#	else
#	{
#		$calcdate="00/00/0000";
#	}
#	return ($calcdate);
#}

sub get_disp_date
{
	my $paramdate = shift;
	my $calcdate;
	if(defined $paramdate and ($paramdate ne "") and ($paramdate ne "0000-00-00"))
	{
		my @date=split(/-/,$paramdate);
		if ($#date ==2)
		{
			$calcdate=$date[2]."/".$date[1]."/".$date[0];
		}
		else
		{
			$calcdate="&nbsp";
		}
	}
	else
	{
		$calcdate="&nbsp";
	}
	return ($calcdate);
}










sub get_db_date
{
	my $calcdate1;
	my $paramdate1 = shift;
	if (defined $paramdate1 and $paramdate1 ne "")
	{
		my @date1=split(/\//,$paramdate1);
		#my $calcdate=$date[2]."/".$date[1]."/".$date[0];
		$calcdate1=$date1[2]."-".$date1[1]."-".$date1[0];
	}
	else
	{
		$calcdate="0000-00-00";
	}

	return ($calcdate1);
}

sub return_field
{

	my($dbh, $query) = @_;
	my($sth);
	my(@val);
	$sth = $dbh->prepare ($query) or die "Error";
	$sth->execute ();
	if($sth->rows() == 0)
	{
		$sth->finish();
		return ("");
		exit;
	} else
	{
		while (my @row = $sth->fetchrow_array ())
		{
			push (@val, $row[0]);
		}
	}
	$sth->finish ();
     if( @val)
     {
	return (@val);
	}
      else
      {
	return "";
	}
}

sub adminfetch
{
		my $adminselectq="select distinct(sysadmin) from modulerights where uid='$cookieuid'";
		my $adminselectr = $dbh->prepare($adminselectq);
		$adminselectr->execute ();
		my @afetch=$adminselectr->fetchrow_array();
		return @afetch[0];
}
