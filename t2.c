#include <stdio.h>
#include <string.h>

void fun(char *c,int len,int l)
{
	if(l>len)
	{
			return;
	}
	if(l>=0)
	{
		fun(c,len,l++);
	}
		printf("%c",c[len]);
	

}

void main(){
char *c,f;
int index=0;
c=(char*)malloc((index+1)*sizeof(char));
while (f=getchar()!='\n')
{
	c[index]=f;
	index++;
	c=(char*)realloc(c,(index+1)*sizeof(char));
}
//scanf("%s",&c);
c[index]='\0';
printf ("%s",c);
int len;
len = index-1;
fun(c,len,0);
}


