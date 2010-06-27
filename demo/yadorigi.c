
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include "yadorigi.h"

#define R (0)
#define W (1)

int popen2(char *command,FILE **fr,FILE **fw)
{
	int pipe_c2p[2],pipe_p2c[2];
	int pid;
	if(pipe(pipe_c2p)<0){
		perror("popen2");
		return -1;
	}
	if(pipe(pipe_p2c)<0){
		perror("popen2");
		close(pipe_c2p[R]);
		close(pipe_c2p[W]);
		return -1;
	}
	if((pid=fork())<0){
		perror("popen2");
		close(pipe_c2p[R]);
		close(pipe_c2p[W]);
		close(pipe_p2c[R]);
		close(pipe_p2c[W]);
		return -1;
	}
	if(pid==0){
		close(pipe_p2c[W]);
		close(pipe_c2p[R]);
		dup2(pipe_p2c[R],0);
		dup2(pipe_c2p[W],1);
		close(pipe_p2c[R]);
		close(pipe_c2p[W]);
		if(execlp("sh","sh","-c",command,NULL)<0){
			perror("popen2");
			close(pipe_p2c[R]);
			close(pipe_c2p[W]);
			exit(1);
		}
	}
	close(pipe_p2c[R]);
	close(pipe_c2p[W]);
	*fw=fdopen(pipe_p2c[W],"w");
	*fr=fdopen(pipe_c2p[R],"r");
	return pid;
}

/*
	Parser
*/

size_t parray_length(void **array)
{
	size_t iter = 0;
	while(array[iter])iter++;
	return iter;
}

void read_spaces(FILE *input)
{
	while(1){
		int getf = fgetc(input);
		if(getf != ' '){
			ungetc(getf,input);
			return;
		}
	}
}

int read_character(char c,FILE *input)
{
	int getf = fgetc(input);
	if(getf == c){
		read_spaces(input);
		return 1;
	}
	else{
		ungetc(getf,input);
		return 0;
	}
}

int *parse_int(FILE *input)
{
	int getf,result,*retval;
	getf = fgetc(input);
	if('0' <= getf && getf <= '9'){
		result = getf-'0';
	}
	else{
		return NULL;
	}
	retval = malloc(sizeof(int));
	if(!retval){
		return NULL;
	}
	while(1){
		getf = fgetc(input);
		if('0' <= getf && getf <= '9'){
			result = result*10+getf-'0';
		}
		else{
			ungetc(getf,input);
			*retval = result;
			read_spaces(input);
			return retval;
		}
	}
}

void **parse_list(parser_t parse_child,free_t free_child,FILE *input)
{
	size_t iter = 0;
	void **result = NULL;
	if(!read_character('[',input)){
		goto ERROR;
	}
	if(read_character(']',input)){
		result = malloc(sizeof(void *));
		if(!result){
			goto ERROR;
		}
		result[0] = NULL;
		return result;
	}
	while(1){
		if(iter%8 == 0){
			void **temp = realloc(result,sizeof(void *)*(iter+9));
			if(!temp){
				goto ERROR;
			}
			result = temp;
		}
		result[iter] = parse_child(input);
		if(!result[iter]){
			goto ERROR;
		}
		iter++;
		if(!read_character(',',input)){
			result[iter] = NULL;
			if(!read_character(']',input)){
				goto ERROR;
			}
			return result;
		}
	}
ERROR:
	while(iter){
		iter--;
		free_child(result[iter]);
	}
	free(result);
	return NULL;
}

void **parse_tuple(parser_t *parsers,free_t *frees,FILE *input)
{
	size_t iter = 0;
	void **result = malloc(sizeof(void *)*(parray_length((void **)parsers)+1));
	if(!read_character('(',input)){
		goto ERROR;
	}
	if(!parsers[0]){
		result[0] = NULL;
		if(!read_character(')',input)){
			goto ERROR;
		}
		return result;
	}
	while(1){
		result[iter] = parsers[iter](input);
		if(!result[iter]){
			goto ERROR;
		}
		iter++;
		if(!parsers[iter]){
			result[iter] = NULL;
			if(!read_character(')',input)){
				goto ERROR;
			}
			return result;
		}
		if(!read_character(',',input)){
			goto ERROR;
		}
	}
ERROR:
	while(iter){
		iter--;
		frees[iter](result[iter]);
	}
	return NULL;
}

int **parse_intlist(FILE *input)
{
	return (int **)parse_list((parser_t)parse_int,(free_t)free_int,input);
}

int ***parse_intlistlist(FILE *input)
{
	return (int ***)parse_list((parser_t)parse_intlist,(free_t)free_intlist,input);
}

int **parse_iituple(FILE *input)
{
	parser_t parsers[3] = {(parser_t)parse_int,(parser_t)parse_int,NULL};
	free_t frees[3] = {(free_t)free_int,(free_t)free_int,NULL};
	return (int **)parse_tuple(parsers,frees,input);
}

int ***parse_iituplelist(FILE *input)
{
	return (int ***)parse_list((parser_t)parse_iituple,(free_t)free_iituple,input);
}

/*
	Output
*/

void output_int(FILE *output,int *n)
{
	char buf[64],*p = &buf[62];
	int m = *n;
	buf[63] = '\0';
	while(1){
		*p = '0'+m%10;
		m = m/10;
		if(!m){
			break;
		}
		p--;
	}
	fputs(p,output);
}

void output_list(output_t output_child,FILE *output,void **list)
{
	size_t iter = 0;
	fputc('[',output);
	if(!list[0]){
		fputc(']',output);
		return;
	}
	while(1){
		output_child(output,list[iter]);
		iter++;
		if(!list[iter]){
			fputc(']',output);
			return;
		}
		fputc(',',output);
	}
}

void output_tuple(output_t *outputs,FILE *output,void **tuple)
{
	size_t iter = 0;
	fputc('(',output);
	if(!outputs[0]){
		fputc(')',output);
		return;
	}
	while(1){
		outputs[iter](output,tuple[iter]);
		iter++;
		if(!outputs[iter]){
			fputc(')',output);
			return;
		}
		fputc(',',output);
	}
}

void output_intlist(FILE *output,int **list)
{
	output_list((output_t)output_int,output,(void **)list);
}

void output_intlistlist(FILE *output,int ***list)
{
	output_list((output_t)output_intlist,output,(void **)list);
}

void output_iituple(FILE *output,int **tuple)
{
	output_t outputs[3] = {(output_t)output_int,(output_t)output_int,NULL};
	output_tuple(outputs,output,(void **)tuple);
}

void output_iituplelist(FILE *output,int ***list)
{
	output_list((output_t)output_iituple,output,(void **)list);
}

/*
	Free
*/

void free_int(int *p)
{
	return free(p);
}

void free_list(free_t free_child,void **p)
{
	size_t iter = 0;
	if(!p){
		return;
	}
	while(p[iter]){
		free_child(p[iter]);
		iter++;
	}
	free(p);
}

void free_tuple(free_t *frees,void **p)
{
	size_t iter = 0;
	if(!p){
		return;
	}
	while(frees[iter]){
		frees[iter](p[iter]);
		iter++;
	}
	free(p);
}

void free_intlist(int **p)
{
	free_list((free_t)free_int,(void **)p);
}

void free_intlistlist(int ***p)
{
	free_list((free_t)free_intlistlist,(void **)p);
}

void free_iituple(int **p)
{
	free_t frees[3] = {(free_t)free_int,(free_t)free_int,NULL};
	free_tuple(frees,(void **)p);
}

void free_iituplelist(int ***p)
{
	free_list((free_t)free_iituple,(void **)p);
}

