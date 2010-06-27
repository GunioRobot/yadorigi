
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "yadorigi.h"

void *copy_with_alloc(const void *p,size_t size)
{
	void *temp = malloc(size);
	if(temp){
		memcpy(temp,p,size);
	}
	return temp;
}

int *fgetli(FILE *input)
{
	int getf,*result;
	result = parse_int(input);
	if(!result){
		return NULL;
	}
	getf = fgetc(input);
	if(!feof(input) && getf != '\n'){
		free_int(result);
		return NULL;
	}
	return result;
}

int **fgetlp(FILE *input)
{
	int getf,**retval;
	retval = malloc(sizeof(int *)*2);
	if(!retval){
		return NULL;
	}
	retval[0] = parse_int(input);
	getf = fgetc(input);
	retval[1] = fgetli(input);
	if(!retval[0] || getf != ',' || !retval[1]){
		free(retval[0]);
		free(retval[1]);
		return NULL;
	}
	return retval;
}

void stream_redirect(FILE *in,FILE *out)
{
	int getf;
	while(1){
		getf = fgetc(in);
		if(feof(in)){
			break;
		}
		fputc(getf,out);
	}
}

int main(void)
{
	int *size,***points,***result,getf;
	size_t iter = 0;
	FILE *pin,*pout;
	size = fgetli(stdin);
	if(!size){
		return -1;
	}
	while(1){
		if(iter%8 == 0){
			int ***temp = realloc(points,sizeof(unsigned int **)*(iter+8));
			if(!temp){
				return -1;
			}
			points = temp;
		}
		getf = fgetc(stdin);
		if(getf == '\n' || feof(stdin)){
			points[iter] = NULL;
			break;
		}
		else{
			ungetc(getf,stdin);
		}
		points[iter] = fgetlp(stdin);
		if(!points[iter]){
			return -1;
		}
		iter++;
	}
	popen2("runghc ./nqueens.hs",&pout,&pin);
	output_int(pin,size);
	fputc('\n',pin);
	output_iituplelist(pin,points);
	fputc('\n',pin);
	fclose(pin);
	result = parse_intlistlist(pout);
	output_intlistlist(stdout,result);
	fclose(pout);
	return 0;
}

