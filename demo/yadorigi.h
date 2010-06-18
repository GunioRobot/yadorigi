
#include <stddef.h>

typedef void *(*parser_t)(FILE *);
typedef void (*output_t)(FILE *,void *);
typedef void (*free_t)(void *);

int popen2(char *,FILE **,FILE **);

unsigned int parray_length(void **);

/* parser */

void read_spaces(FILE *);
int read_character(char,FILE *);

int *parse_int(FILE *);
void **parse_list(parser_t,free_t,FILE *);
void **parse_tuple(parser_t *,free_t *,FILE *);

int **parse_intlist(FILE *);
int ***parse_intlistlist(FILE *);
int **parse_iituple(FILE *);
int ***parse_iituplelist(FILE *);

/* output */

void output_int(FILE *,int *);
void output_list(output_t,FILE *,void **);
void output_tuple(output_t *,FILE *,void **);

void output_intlist(FILE *,int **);
void output_intlistlist(FILE *,int ***);
void output_iituple(FILE *,int **);
void output_iituplelist(FILE *,int ***);

/* free */

void free_int(int *);
void free_list(free_t,void **);
void free_tuple(free_t *,void **);

void free_intlist(int **);
void free_intlistlist(int ***);
void free_iituple(int **);
void free_iituplelist(int ***);

