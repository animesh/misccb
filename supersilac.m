%% read

prot=xlsread('C:\Users\animeshs\Desktop\SS1RPGsort.xlsx')

%% save at tab txt and transpose

awk '
{
    for (i=1; i<=NF; i++)  {
        a[NR,i] = $i
    }
}
NF>p { p = NF }
END {
    for(j=1; j<=p; j++) {
        str=a[1,j]
        for(i=2; i<=NR; i++){
            str=str" "a[i,j];
        }
        print str
    }
}' /cygdrive/c/Users/animeshs/Desktop/SS1RPGsort.txt > /cygdrive/c/Users/animeshs/Desktop/SS1RPGsorttrp.txt

sed 's/ /,/g' /cygdrive/c/Users/animeshs/Desktop/SS1RPGsorttrp.txt > /cygdrive/c/Users/animeshs/Desktop/SS1RPGsorttrp.csv

source: http://stackoverflow.com/questions/1729824/transpose-a-file-in-bash


%% DT

C9K057 <= 1.455 : LM1 (15/0%)
C9K057 >  1.455 : LM2 (9/0%)

LM num: 1
Class = 
	0.1213 * C9K057 
	- 0.0292

LM num: 2
Class = 
	0.1517 * C9K057 
	+ 0.3385

