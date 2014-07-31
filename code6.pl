#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    Code base of Animesh Sharma [ sharma.animesh@gmail.com ]

#!/usr/bin/perl
foreach $a (a..z)
{foreach $b (a..z)
{$c=$a.$b;push(@ab,$c);}
}
foreach $c (a..z)
{$d=$c.X;push(@ab,$d);$d=X.$c;push(@ab,$d);$c="";}
push(@ab,XX);
$tt=@ab;
open (FILENAME,"chapter-01.txt")||die "can't open $filename: $!";
while ($line = <FILENAME>) {
$whole=$whole.$line;
lc($whole);
$whole=~tr/^\t\n,;"':![]{}()-_'.?\/`~ /X/;
}
foreach $al (@ab)
{
$cont=$whole=~s/$al/$al/g;
#print "$al\t$cont\n";
push(@ccnt,$cont);
}
#foreach $vv (@ccnt) {print "$vv\t";}
for($cc=0;$cc<$tt;$cc++)
{
$tree{$ab[$cc]}=$ccnt[$cc];
}
$XX=$tree{XX};
foreach $alls (a..z){
$c=$alls.a;
$cc=$tree{$c};
print "$c\t$cc\n";
$t1=$t1+$cc;
push(@a,$cc);
$c=$alls.b;
$cc=$tree{$c};
print "$c\t$cc\n";
$t2=$t2+$cc;
push(@b,$cc);
$c=$alls.c;
$cc=$tree{$c};
print "$c\t$cc\n";
$t3=$t3+$cc;
push(@c,$cc);
$c=$alls.d;
$cc=$tree{$c};
print "$c\t$cc\n";
$t4=$t4+$cc;
push(@d,$cc);
$c=$alls.e;
$cc=$tree{$c};
print "$c\t$cc\n";
$t5=$t5+$cc;
push(@e,$cc);
$c=$alls.f;
$cc=$tree{$c};
print "$c\t$cc\n";
$t6=$t6+$cc;
push(@f,$cc);
$c=$alls.g;
$cc=$tree{$c};
print "$c\t$cc\n";
$t7=$t7+$cc;
push(@g,$cc);
$c=$alls.h;
$cc=$tree{$c};
print "$c\t$cc\n";
$t8=$t8+$cc;
push(@h,$cc);
$c=$alls.i;
$cc=$tree{$c};
print "$c\t$cc\n";
$t9=$t9+$cc;
push(@i,$cc);
$c=$alls.j;
$cc=$tree{$c};
print "$c\t$cc\n";
$t10=$t10+$cc;
push(@j,$cc);
$c=$alls.k;
$cc=$tree{$c};
print "$c\t$cc\n";
$t11=$t11+$cc;
push(@k,$cc);
$c=$alls.l;
$cc=$tree{$c};
print "$c\t$cc\n";
$t12=$t12+$cc;
push(@l,$cc);
$c=$alls."m";
$cc=$tree{$c};
print "$c\t$cc\n";
$t13=$t13+$cc;
push(@m,$cc);
$c=$alls.n;
$cc=$tree{$c};
print "$c\t$cc\n";
$t14=$t14+$cc;
push(@n,$cc);
$c=$alls.o;
$cc=$tree{$c};
print "$c\t$cc\n";
$t15=$t15+$cc;
push(@o,$cc);
$c=$alls.p;
$cc=$tree{$c};
print "$c\t$cc\n";
$t16=$t16+$cc;
push(@p,$cc);
$c=$alls."q";
$cc=$tree{$c};
print "$c\t$cc\n";
$t17=$t17+$cc;
push(@q,$cc);
$c=$alls.r;
$cc=$tree{$c};
print "$c\t$cc\n";
$t18=$t18+$cc;
push(@r,$cc);
$c=$alls."s";
$cc=$tree{$c};
print "$c\t$cc\n";
$t19=$t19+$cc;
push(@s,$cc);
$c=$alls.t;
$cc=$tree{$c};
print "$c\t$cc\n";
$t20=$t20+$cc;
push(@t,$cc);
$c=$alls.u;
$cc=$tree{$c};
print "$c\t$cc\n";
$t21=$t21+$cc;
push(@u,$cc);
$c=$alls.v;
$cc=$tree{$c};
print "$c\t$cc\n";
$t22=$t22+$cc;
push(@v,$cc);
$c=$alls.w;
$cc=$tree{$c};
print "$c\t$cc\n";
$t23=$t23+$cc;
push(@w,$cc);
$c=$alls.x;
$cc=$tree{$c};
print "$c\t$cc\n";
$t24=$t24+$cc;
push(@x,$cc);
$c=$alls."y";
$cc=$tree{$c};
print "$c\t$cc\n";
$t25=$t25+$cc;
push(@y,$cc);
$c=$alls.z;
$cc=$tree{$c};
print "$c\t$cc\n";
$t26=$t26+$cc;
push(@z,$cc);
}
foreach $xs (a..z)
{$c=X.$xs;
$cc=$tree{$c};
print "$c\t$cc\n";
push(@Xf,$cc);
$t0=$t0+$cc;
$c=$xs.X;
$cc=$tree{$c};
print "$c\t$cc\n";
$t27=$t27+$cc;
push(@sX,$cc);
}
$c=X.X;
$cc=$tree{$c};
print "$c\t$cc\n";
$t27=$t27+$cc;
push(@sX,$cc);
$t1=$t1+@Xf[0];$t2=$t2+@Xf[1];$t3=$t3+@Xf[2];$t4=$t4+@Xf[3];$t5=$t5+@Xf[4]; $t6=$t6+@Xf[5];$t7=$t7+@Xf[6];
$t8=$t8+@Xf[7];$t9=$t9+@Xf[8];$t10=$t10+@Xf[9];$t11=$t11+@Xf[10];$t12=$t12+@Xf[11]; $t13=$t13+@Xf[12];
$t14=$t14+@Xf[13];$t15=$t15+@Xf[14];$t16=$t16+@Xf[15]; $t17=$t17+@Xf[16]; $t18=$t18+@Xf[17];
$t19=$t19+@Xf[18]; $t20=$t20+@Xf[19]; $t21=$t21+@Xf[20];$t22=$t22+@Xf[21]; $t23=$t23+@Xf[22];
$t24=$t24+@Xf[23]; $t25=$t25+@Xf[24];$t26=$t26+@Xf[25];
@a1= map ($_/$t1+1, @a); @b1= map ($_/$t2+1 , @b);  @c1= map ($_/$t3+1 , @c);
@d1= map ($_/$t4+1 , @d);@e1= map ($_/$t5+1 , @e);@f1= map ($_/$t6+1 , @f);
@g1= map ($_/$t7+1 , @g);   @h1= map ($_/$t8+1 , @h);@i1= map ($_/$t9+1 , @i);
 @j1= map ($_/$t10+1 , @j); @k1= map ($_/$t11+1 , @k);     @l1= map ($_/$t12+1 , @l);
@m1= map ($_/$t13+1 , @m);  @n1= map ($_/$t14+1 , @a);   @o1= map ($_/$t15+1 , @o);
@p1= map ($_/$t16+1 , @p); @q1= map ($_/$t17+1 , @q);  @r1= map ($_/$t18+1 , @r);
@s1= map ($_/$t19+1 , @s);@t1= map ($_/$t20+1 , @t);      @u1= map ($_/$t21+1, @u);
 @v1= map ($_/$t22+1 , @v);@w1= map ($_/$t23+1, @w); @x1= map ($_/$t24+1, @x);
 @y1= map ($_/$t25+1, @y);  @z1= map ($_/$t26+1, @z );@sX1= map ($_/$t27+1, @sX );