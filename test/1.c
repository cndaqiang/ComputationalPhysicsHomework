#include"stdio.h"
void main()
{
	float i,j,ii;
	int n;
	j=0.1;
	i=0;
	ii=0;
	n=0;
	i=0.1+0.1+0.1+0.1+0.1+0.1+0.1+0.1+0.1;
	for(n=0;n<10;n++) ii=ii+j;	
	printf("float字节%d\n",sizeof(i));
	printf("++:\t i=%1.10f\n",i);
	printf("while:\tii=%1.10f\n",ii);
}
