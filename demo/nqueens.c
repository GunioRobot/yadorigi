
#include <stdio.h>
#include "yadorigi.h"

typedef struct pos{
	int x,y;
} pos_t;

int main(void)
{
	yadenv_t env;
	unsigned int boardsize = 10;
	unsigned int ***result;
	size_t iter = 0;
	pos_t poslist[] = {{0,0},{1,2}};
	env = yadload("nqueens.hs");
	if(!env){
		fputs("Load Error!\n",stderr);
	}
	result = yadcall(env,"nqueensInterface",boardsize,poslist);
	while(result[iter]){
		size_t iter_ = 0;
		printf("%d : ",(unsigned int)iter);
		while(result[iter][iter_]){
			printf("(%d)%d ",result[iter][iter_],*(result[iter][iter_]));
			iter_++;
		}
		fputc('\n',stdout);
		iter++;
	}
	return 0;
}
