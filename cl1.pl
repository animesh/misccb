use Checkme;
$me = Checkme->new;
$me->name("ANISH");
$me->age(23);
$who = $me->name; 
$age = $me->age; 
print "$who $age";
