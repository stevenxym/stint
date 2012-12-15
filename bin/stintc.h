#ifndef _STINTC_H
#define _STINTC_H

/*
 * CONSTANT
 */

/* argument */
#define MIN_ARG_NUM		2
#define FLAG_POS		1

/* buffer */
#define MAX_BUFF		1000
#define BUFF_SIZE		MAX_BUFF + 1
#define FILTER_SIZE		20

#define LOW_START		97
#define LOW_END			122
#define CASE_OFFSET		32
#define IS_LOW_CASE(c)		(c >= LOW_START && c <= LOW_END)
#define TO_UP_CASE(c)		c -= CASE_OFFSET

#define JAVA_CODE		0
#define JAVA_CLASS		2
#define OUTPUT_FILE		4
#define EXP_FILE		8
#define EXE_TARGET		16
#define STINT_CODE		32

#define INIT_FILTER(f, s)	(f) = strdup(s)
#define EMPTY_FILTER(f)		(f) = NULL
#define INIT_FILE_LIST(c, s)	c->f_list = (char **)malloc(sizeof(char *) * (s))

enum cmd_type {MAKEFILE, COMPILE, RUNCODE, DEBUG};
enum make_type {MAKE, CLEAN, CLEAN_ALL};


struct command{
	enum cmd_type	type;
	char 		**f_list;
	int		arg_count;
	char		*filter;
};


#endif
