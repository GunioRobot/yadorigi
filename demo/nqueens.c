
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SWAP(a,b,size) \
	do{ \
		char *a_ = (char *)(a),*b_ = (char *)(b); \
		size_t size_ = (size); \
		while(size_--){ \
			char temp = *a_; \
			*a_++ = *b_; \
			*b_++ = temp; \
		} \
	}while(0)

int abs(int n)
{
	if(0 <= n){
		return n;
	}
	else{
		return -n;
	}
}

unsigned int fact(unsigned int n)
{
	unsigned int result = 1;
	while(n){
		result = result*n;
		n--;
	}
	return result;
}

void *permutations(const void *array,size_t elem,size_t length,unsigned int index)
{
	void *result = malloc(elem*length);
	size_t iter = 0;
	if(!result){
		return NULL;
	}
	memcpy(result,array,elem*length);
	while(iter != length){
		SWAP(result+elem*iter,result+elem*(iter+index%(length-iter)),elem);
		index = index/(length-iter);
		iter++;
	}
	return result;
}

int is_nqueens(size_t size,unsigned int *array)
{
	size_t iter1 = 0;
	while(iter1 != size-1){
		size_t iter2 = iter1+1;
		while(iter2 != size){
			if(iter2-iter1 == abs(array[iter2]-array[iter1])){
				return 0;
			}
			iter2++;
		}
		iter1++;
	}
	return 1;
}

unsigned int **nqueens(size_t size)
{
	unsigned int
		**result = NULL,
		*array = malloc(sizeof(unsigned int)*size),
		iter = 0,
		iter_stop = fact(size),
		result_iter = 0;
	if(!array){
		return NULL;
	}
	while(iter != size){
		array[iter] = iter;
		iter++;
	}
	iter = 0;
	while(iter != iter_stop){
		int *next_nqueens = permutations(array,sizeof(unsigned int),size,iter);
		if(!next_nqueens){
			goto ERROR;
		}
		if(is_nqueens(size,next_nqueens)){
			if(result_iter%8 == 0){
				unsigned int **temp = realloc(result,sizeof(unsigned int *)*(result_iter+8));
				if(!temp){
					free(next_nqueens);
					goto ERROR;
				}
				result = temp;
			}
			result[result_iter] = next_nqueens;
			result_iter++;
		}
		else{
			free(next_nqueens);
		}
		iter++;
	}
	if(result_iter%8 == 0){
		unsigned int **temp = realloc(result,sizeof(unsigned int *)*(result_iter+8));
		if(!temp){
			goto ERROR;
		}
		result = temp;
	}
	result[result_iter] = NULL;
	return result;
ERROR:
	while(result_iter){
		result_iter--;
		free(result[result_iter]);
	}
	free(result);
	return NULL;
}

int main(void)
{
	unsigned int size = 10,**result = nqueens(size);
	size_t iter1 = 0;
	while(result[iter1]){
		size_t iter2 = 0;
		while(iter2 != size){
			printf("%d ",result[iter1][iter2]);
			iter2++;
		}
		fputc('\n',stdout);
		iter1++;
	}
}

