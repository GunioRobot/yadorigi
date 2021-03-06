
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ncurses.h>
#include <unistd.h>
#include "yadorigi.h"

#define DEFAULT_BOARD_SIZE 5
#define MAX_BOARD_SIZE 32

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

typedef struct
{
	int x,y;
} point_t;

int abs(int n)
{
	return 0 < n?n:-n;
}

size_t parray_length(void **array)
{
	size_t iter = 0;
	while(array[iter])iter++;
	return iter;
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

unsigned int calc_pages(unsigned int all_items,unsigned page_items)
{
	return !all_items?0:(all_items-1)/page_items+1;
}

int input_nqueens_data(WINDOW *mainwin,size_t *size_,size_t *points_size,point_t **points_)
{
	size_t size = DEFAULT_BOARD_SIZE,iter = 0,iter_,x = 0,y = 0;
	int *points = malloc(sizeof(int)*MAX_BOARD_SIZE);
	WINDOW *subwindow;
	if(!points)return 0;
	while(iter != DEFAULT_BOARD_SIZE)points[iter++] = -1;
	wclear(mainwin);
	wrefresh(mainwin);
	subwindow = subwin(mainwin,size+2,size+2,0,0);
	goto REWRITE_BOARD;
	while(1){
		int getf = wgetch(subwindow);
		switch(getf){
		case '\n':
			delwin(subwindow);
			*size_ = size;
			iter = 0,iter_ = 0;
			*points_ = malloc(sizeof(point_t)*(size+1));
			if(!*points_){
				free(points);
				return 0;
			}
			while(iter != size){
				if(0 <= points[iter]){
					(*points_)[iter_].x = iter;
					(*points_)[iter_].y = points[iter];
					iter_++;
				}
				iter++;
			}
			*points_size = iter_;
			free(points);
			return 1;
		case 'q':
			delwin(subwindow);
			free(points);
			return 2;
		case 'h':
			if(0 < x)x--;
			goto MOVE_CURSOR;
		case 'j':
			if(y < size-1)y++;
			goto MOVE_CURSOR;
		case 'k':
			if(0 < y)y--;
			goto MOVE_CURSOR;
		case 'l':
			if(x < size-1)x++;
			goto MOVE_CURSOR;
		case 'a':
			if(1 < size){
				size--;
				if(x == size)x--;
				if(y == size)y--;
				iter = 0;
				while(iter != size){
					if(points[iter] == size)points[iter] = -1;
					iter++;
				}
				goto REWRITE_BOARD;
			}
			break;
		case 's':
			if(size < MAX_BOARD_SIZE){
				points[size] = -1;
				size++;
				goto REWRITE_BOARD;
			}
			break;
		case ' ':
			if(points[x] == y)points[x] = -1;
			else{
				iter = 0;
				while(iter != size){
					if(points[iter] != -1 && (points[iter] == y ||
						abs(x-iter) == abs(y-points[iter])))points[iter] = -1;
					iter++;
				}
				points[x] = y;
			}
		REWRITE_BOARD:
			wclear(subwindow);
			wrefresh(subwindow);
			wresize(subwindow,size+2,size+2);
			wborder(subwindow,'|','|','-','-','+','+','+','+');
			iter = 0;
			while(iter != size){
				if(0 <= points[iter])mvwaddch(subwindow,points[iter]+1,iter+1,'*');
				iter++;
			}
		MOVE_CURSOR:
			wmove(subwindow,y+1,x+1);
			wrefresh(subwindow);
		}
	}
}

int result_filter(size_t size,size_t points_size,point_t *points,unsigned int *result)
{
	size_t iter = 0;
	while(iter != points_size){
		if(size <= points[iter].x || result[points[iter].x] != points[iter].y){
			return 0;
		}
		iter++;
	}
	return 1;
}

int get_result(size_t size,size_t points_size,point_t *points,unsigned int ***result)
{
	size_t iter = 0,iter_ = 0;
	*result = nqueens(size);
	if(!*result){
		return 0;
	}
	while((*result)[iter]){
		if(result_filter(size,points_size,points,(*result)[iter])){
			(*result)[iter_] = (*result)[iter];
			iter_++;
		}
		else{
			free((*result)[iter]);
		}
		iter++;
	}
	(*result)[iter_] = NULL;
	return 1;
}

int output_result(WINDOW *mainwin,size_t size,unsigned int **result)
{
	size_t iter,iter_x,iter_y,page = 0,pages,page_items;
	unsigned int mainwin_size_x,mainwin_size_y;
	WINDOW **subwins;
	getmaxyx(mainwin,mainwin_size_y,mainwin_size_x);
	page_items = (mainwin_size_x/(size+3))*(mainwin_size_y/(size+3));
	pages = calc_pages(parray_length((void **)result),page_items);
	if(!pages){
		wclear(mainwin);
		mvwinsstr(mainwin,1,1,"empty result");
		wrefresh(mainwin);
		return 1;
	}
	subwins = malloc(sizeof(WINDOW *)*page_items);
	if(!subwins)return 0;
	iter = 0;
	iter_y = 0;
	while(iter_y != mainwin_size_y/(size+3)){
		iter_x = 0;
		while(iter_x != mainwin_size_x/(size+3)){
			subwins[iter] = subwin(mainwin,size+2,size+2,iter_y*(size+3),iter_x*(size+3));
			if(!subwins[iter]){
				while(iter--)delwin(subwins[iter]);
				free(subwins);
				return 0;
			}
			iter_x++;
			iter++;
		}
		iter_y++;
	}
	goto REWRITE_BOARD;
	while(1){
		int getf = wgetch(mainwin);
		switch(getf){
		case '\n':
		case 'q':
			iter = 0;
			while(iter != page_items)delwin(subwins[iter++]);
			free(subwins);
			wclear(mainwin);
			wrefresh(mainwin);
			return (getf == '\n'?1:2);
		case 'j':
			if(page < pages-1){
				page++;
				goto REWRITE_BOARD;
			}
			break;
		case 'k':
			if(page){
				page--;
				goto REWRITE_BOARD;
			}
			break;
		REWRITE_BOARD:
			wclear(mainwin);
			iter = 0;
			while(iter != page_items && result[page_items*page+iter]){
				iter_x = 0;
				while(iter_x != size){
					mvwaddch(subwins[iter],result[page_items*page+iter][iter_x]+1,iter_x+1,'*');
					iter_x++;
				}
				wborder(subwins[iter],'|','|','-','-','+','+','+','+');
				iter++;
			}
			wrefresh(mainwin);
			break;
		}
	}
}

void free_result(unsigned int **result)
{
	size_t iter = 0;
	while(result[iter])free(result[iter++]);
	free(result);
}

int main(void)
{
	int errcode;
	initscr();
	cbreak();
	noecho();
	while(1){
		unsigned int **result;
		size_t size,points_size;
		point_t *points;
		errcode = input_nqueens_data(stdscr,&size,&points_size,&points);
		if(errcode == 0)return -1;
		if(errcode == 2)break;
		errcode = get_result(size,points_size,points,&result);
		free(points);
		if(errcode == 0)return -1;
		errcode = output_result(stdscr,size,result);
		free_result(result);
		if(errcode == 0)return -1;
		if(errcode == 2)break;
	}
	endwin();
	return 0;
}

