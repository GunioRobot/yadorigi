
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ncurses.h>
#include <unistd.h>
#include "yadorigi.h"

#define DEFAULT_BOARD_SIZE 5
#define MAX_BOARD_SIZE 32

typedef struct
{
	int x,y;
} point_t;

int abs(int n)
{
	return 0 < n?n:-n;
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

int get_result(size_t size,size_t points_size,point_t *points,int ***result)
{
	size_t iter = 0,iter_,result_size;
	int hs_size = size,***hs_points = malloc(sizeof(int **)*(points_size+1))
		,***hs_result,errcode;
	FILE *pin,*pout;
	if(!hs_points)return 0;
	while(iter != points_size){
		hs_points[iter] = malloc(sizeof(int *)*2);
		if(!hs_points[iter]){
			free_iituplelist(hs_points);
			return 0;
		}
		hs_points[iter][0] = malloc(sizeof(int));
		hs_points[iter][1] = malloc(sizeof(int));
		if(!hs_points[iter][0] || !hs_points[iter][1]){
			free(hs_points[iter][0]);
			free(hs_points[iter][1]);
			free(hs_points[iter]);
			hs_points[iter] = NULL;
			free_iituplelist(hs_points);
			return 0;
		}
		*hs_points[iter][0] = points[iter].x;
		*hs_points[iter][1] = points[iter].y;
		iter++;
	}
	hs_points[points_size] = NULL;
	errcode = popen2("runyadorigi ./nqueens.hs",&pout,&pin);
	if(errcode < 0){
		free_iituplelist(hs_points);
		return 0;
	}
	output_int(pin,&hs_size);
	fputc('\n',pin);
	output_iituplelist(pin,hs_points);
	fputc('\n',pin);
	fclose(pin);
	hs_result = parse_intlistlist(pout);
	fclose(pout);
	if(!hs_result)return 0;
	result_size = parray_length((void **)hs_result);
	*result = malloc(sizeof(int *)*(result_size+1));
	if(!*result){
		free_intlistlist(hs_result);
		return 0;
	}
	iter = 0;
	while(iter != result_size){
		(*result)[iter] = malloc(sizeof(int)*size);
		if(!(*result)[iter]){
			while(iter--)free((*result)[iter]);
			free(*result);
			return 0;
		}
		iter_ = 0;
		while(iter_ != size){
			(*result)[iter][iter_] = *hs_result[iter][iter_];
			iter_++;
		}
		iter++;
	}
	(*result)[result_size] = NULL;
	return 1;
}

int output_result(WINDOW *mainwin,size_t size,int **result)
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

void free_result(int **result)
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
		int **result;
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

