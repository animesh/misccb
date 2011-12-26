# LaTeX2HTML 99.1 release (March 30, 1999)
# Associate index original text with physical files.


$key = q/sectiontableau/;
$index{$key} .= q|<A HREF="|."$dir".q|ex32.html#23">Exemple d'un tableau</A>
 \| |; 
$noresave{$key} = "$nosave";

$key = q/tableau/;
$index{$key} .= q|<A HREF="|."$dir".q|ex32.html#49">Exemple d'un tableau</A>
 \| |; 
$noresave{$key} = "$nosave";

# LaTeX2HTML 99.1 release (March 30, 1999)
# Printable index-keys from sub_index array.


$key = q/section/;
$sub_index{$key} .= q|sectiontableau|; 
$noresave{$key} = "$nosave";

# LaTeX2HTML 99.1 release (March 30, 1999)
# Printable index-keys from printable_key array.


$key = q/sectiontableau/;
$printable_key{$key} = q|tableau| unless ($printable_key{$key}); 
$noresave{$key} = "$nosave";

$key = q/section/;
$printable_key{$key} = q|section| unless ($printable_key{$key}); 
$noresave{$key} = "$nosave";

$key = q/tableau/;
$printable_key{$key} = q|tableau| unless ($printable_key{$key}); 
$noresave{$key} = "$nosave";

1;

