/*
 * stintc.c
 *
 * tool for running code in Stint
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "stintc.h"

static int parse_cmd(int argc, char **argv, struct command *c)
{
	char flag;

	/* check number of arguments */
	if (argc < MIN_ARG_NUM)
		return -EINVAL;

	/* check flag */
	if (strlen(argv[FLAG_POS]) != 2 || argv[FLAG_POS][0] != '-')
		return -EINVAL;

	flag = argv[FLAG_POS][1];
	
	/* parse flag & get following arguments */
	if (flag == 'm') {
		c->type = MAKEFILE;
		c->arg_count = 0;
		if (argc != MIN_ARG_NUM)
			return -E2BIG;
	} else if (flag == 'c' | flag == 'r') {

		if (flag == 'c')
			c->type = COMPILE;
		else
			c->type = RUNCODE;

		if (argc <= MIN_ARG_NUM)
			return -EINVAL;

		INIT_FILE_LIST(c, argc - MIN_ARG_NUM);
		c->arg_count = argc - MIN_ARG_NUM;
		int i;
		for (i = MIN_ARG_NUM; i < argc; i++)
			c->f_list[i - MIN_ARG_NUM] = strdup(argv[i]);

	} else if (flag == 'd') {
		c->type = DEBUG;
		if (argc > 3)
			return -E2BIG;
		else if (argc == 3) {
			if (strlen(argv[2]) >= FILTER_SIZE)
				return -EINVAL;
			else
				INIT_FILTER(c->filter, argv[2]);
		} else
			EMPTY_FILTER(c->filter);
	} else
		return -EINVAL;

	return 0;
}

static void print_usage()
{
	printf("Usage:	stintc [-m]\n");
	printf("	stintc [-c | -j | -r] source ...\n");
	printf("	stintc [-d] index\n");
}

/* running shell command make*/
static void __run_makefile(enum make_type m)
{
	char *buff[3];
	int pid;
	int err = 0;
	buff[0] = strdup("make");

	if (m == MAKE)
		buff[1] = NULL;
	else if (m == CLEAN) {
		buff[1] = strdup("clean");
		buff[2] = NULL;
	} else if (m == CLEAN_ALL) {
		buff[1] = strdup("cleanall");
		buff[2] = NULL;
	}

	pid = fork();
	if (pid != 0)
		wait(NULL);
	else {
		err = execv("/usr/bin/make", buff);
		if (err < 0) {
			perror("");
			exit(EXIT_FAILURE);
		}
	}	
}

static int run_makefile()
{
	printf("run makefile for ocaml code...\n");
	__run_makefile(CLEAN_ALL);
	__run_makefile(MAKE);
	if (access("./stint", F_OK)) {
		printf("fail to makefile...\n");
		return -1;
	}
	printf("---------\nclean up unnecessary intermedia files...\n");
	__run_makefile(CLEAN);
	printf("finish makefile...\n");
	return 0;
}

static char *getln_from_file(FILE *fp)
{
	char *buff = NULL;
	int len;
	//int size;
	//int i = 1;

	buff = (char *)malloc(sizeof(char) * BUFF_SIZE);
	fgets(buff, BUFF_SIZE, fp);

	len  = strlen(buff);
	if (len == 0)
		return buff;

	if (buff[len - 1] != '\n' && !feof(fp)) {
		printf("line is too long!\n");
		exit(EXIT_FAILURE);
	} else
		buff[len - 1] = 0;
	/*do {
		if (buff)
			free(buff);

		size = MAX_BUFF * i;
		buff = (char *)malloc(sizeof(char) * BUFF_SIZE * i);
		fgets(buff, BUFF_SIZE, fp);
		i++;
	} while (strlen(buff) == size);*/

	return buff;
}

static int do_diff(const char *exp_file, const char *out_file)
{
	char *exp_buff = NULL, *out_buff = NULL;
	FILE *exp_fp, *out_fp;
	int line = 1;
	int ret = 0;

	exp_fp = fopen(exp_file, "rt");
	if (!exp_fp) {
		printf("FATAL: cannot open test case %s, terminate...\n", exp_file);
		exit(EXIT_FAILURE);
	}

	out_fp = fopen(out_file, "rt");
	if (!out_fp) {
		printf("target file %s doesn't exist, skip...\n", out_file);
		fclose(exp_fp);
		return -1;
	}

	while (!feof(exp_fp) || !feof(out_fp)) {

		/* get line from both file */
		exp_buff = getln_from_file(exp_fp);
		out_buff = getln_from_file(out_fp);

		if (strlen(exp_buff) != strlen(out_buff)
			|| memcmp(exp_buff, out_buff,
			sizeof(char) * strlen(exp_buff)) != 0) {

			printf("At line #%d:\n", line);
			printf("Expected: %s\n", exp_buff);
			printf("Result: %s\n", out_buff);

			free(exp_buff);
			free(out_buff);
			ret = -1;
			goto out;
		}

		line++;
		free(exp_buff);
		free(out_buff);
	}

	printf("Passed...\n");

out:	fclose(exp_fp);
	fclose(out_fp);
	return ret;
}

/* find match of sub inside target
 * return 1 for success */
static int find_str_match(char *target, char *sub)
{
	char *pos;

	for (pos = target; pos != NULL; pos++) {
		pos = strchr(pos, sub[0]);
		if (pos) {
			if (memcmp(pos, sub, sizeof(char) * strlen(sub)) == 0)
				return 1;
		} else {
			return 0;
		}
	}
	return 0;
}

static void load_testcase(struct command *cmd)
{
	int total;
	FILE *fp;
	char *buff;
	char *pos;
	int i, count = 0;

	fp = fopen("../testcase/testcase.list", "rt");
	if (!fp) {
		printf("FATAL: cannot find test case list under required path, terminate...\n");
		exit(EXIT_FAILURE);
	}

	/* get test case size in total */
	buff = getln_from_file(fp);
	total = atoi(buff);
	if (total <= 0) {
		printf("FATAL: empty test case list, terminate...\n");
		exit(EXIT_FAILURE);
	}
	free(buff);

	/* record test case list */
	INIT_FILE_LIST(cmd, total);
	for (i = 0; i < total && !feof(fp); i++) {

		buff = getln_from_file(fp);
		if (strlen(buff) == 0) {
			printf("skip empty line...\n");
			free(buff);
			continue;
		}

		/* filter files if necessary */
		if (cmd->filter) {
			if (!find_str_match(buff, cmd->filter)) {
				free(buff);
				continue;
			}
		}

		cmd->f_list[count] = strdup(buff);
		count++;

		free(buff);
	}

	if (count == 0) {
		printf("FATAL: no test case selected, terminate...\n");
		exit(EXIT_FAILURE);
	} else
		cmd->arg_count = count;

	fclose(fp);
}

static void __run_compiler(char *name, char *option)
{
	int pid;
	int err = 0;
	char *buff[4];

	buff[0] = strdup("stint");
	buff[1] = strdup(option);
	buff[2] = strdup(name);
	buff[3] = NULL;

	pid = fork();
	if (pid != 0)
		wait(NULL);
	else {
		err = execv("./stint", buff);
		if (err < 0) {
			perror("");
			exit(EXIT_FAILURE);
		}
	}
}

static void __run_java(char *name, char *out)
{
	int pid;
	int err = 0;
	char *buff[5];

	buff[0] = strdup("java");
	buff[1] = strdup(name);

	/*if (out) {
		buff[2] = strdup(">");
		buff[3] = strdup(name);
		buff[4] = NULL;
		printf("output is %s\n", out);
	} else*/
		buff[2] = NULL;

	pid = fork();
	if (pid != 0)
		wait(NULL);
	else {
		int fd = open(out, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
		close(1);
		dup(fd);
		err = execv("/usr/bin/java", buff);
		if (err < 0) {
			perror("");
			close(fd);
			exit(EXIT_FAILURE);
		}
		close(fd);
	}
}

static void change_suffix(char *name, char *suffix)
{
	char *pos = strrchr(name, '.');
	if (pos) {
		if (suffix)
			strcpy(pos, suffix);
		else
			pos[0] = 0;
	} else {
		if (suffix)
			strcat(name, suffix);
	}
}

static void conv_file_name(char *name, int mode)
{
	switch(mode) {
	case JAVA_CODE:
		if (IS_LOW_CASE(name[0]))
			TO_UP_CASE(name[0]);
		change_suffix(name, ".java");
		break;
	case JAVA_CLASS:
		if (IS_LOW_CASE(name[0]))
			TO_UP_CASE(name[0]);
		change_suffix(name, ".class");
		break;
	case OUTPUT_FILE:
		change_suffix(name, ".ans");
		break;
	case EXP_FILE:
		change_suffix(name, ".out");
		break;
	case EXE_TARGET:
		if (IS_LOW_CASE(name[0]))
			TO_UP_CASE(name[0]);
		change_suffix(name, NULL);
		break;
	case STINT_CODE:
		change_suffix(name, ".sti");
		break;
	}
}

static int run_compiler(struct command *cmd)
{
	int i;
	char name_buff[30];
	char path_buff[40] = "./";
	int count = 0;

	/* check command type */
	if (cmd->type != COMPILE && cmd->type != RUNCODE) {
		printf("Unexpected error\n");
		exit(EXIT_FAILURE);
	}

	/* check binary for compiler */
	printf("check compiler binary...\n");
	if (access("./stint", F_OK)) {
		printf("compiler stint doesn't exist, run makefile...\n");
		if (run_makefile() < 0)
			return -1;
	} else
		printf("binary found...\n");

	printf("\n-------COMPILING-------\n");
	printf("compile source code to java");
	if (cmd->type == COMPILE)
		printf(" source code...\n");
	else
		printf(" executable...\n");

	/* run each given file */
	for (i = 0; i < cmd->arg_count; i++) {
		printf("-----------\ncompiling %s...\n", cmd->f_list[i]);

		if (cmd->type == COMPILE)
			__run_compiler(cmd->f_list[i], "-j");
		else
			__run_compiler(cmd->f_list[i], "-c");

		/* get name and path */
		strcpy(name_buff, cmd->f_list[i]);

		if (cmd->type == COMPILE)
			conv_file_name(name_buff, JAVA_CODE);
		else
			conv_file_name(name_buff, JAVA_CLASS);

		strcat(path_buff, name_buff);
		//printf("buff is %s\n", buff);

		if (access(path_buff, F_OK)) {
			printf("fail to generate java");
			if (cmd->type == COMPILE)
				printf(" source code...\n");
			else
				printf(" executable...\n");
			free(cmd->f_list[i]);
			cmd->f_list[i] = NULL;
			continue;
		} else {
			printf("generate java");
			if (cmd->type == COMPILE)
				 printf(" source code");
			else
				printf(" executable");
			printf(" in %s\n", name_buff);
			count++;
		}
	}

	printf("-----------\ncompile finishes, %d in total\n%d succeeded, %d failed\n",
			cmd->arg_count, count, cmd->arg_count - count);
	return count;
}

static void run_java(struct command *cmd)
{
	int count;
	int i;
	char name_buff[30];

	/* compile source code to java class */
	count = run_compiler(cmd);
	if (count == 0)
		return;

	printf("\n-------RUNNING-------\n");
	printf("running *.class files...\n");

	for (i = 0; i < cmd->arg_count; i++) {

		if (!cmd->f_list[i])
			continue;
		
		printf("-----------\n");

		strcpy(name_buff, cmd->f_list[i]);
		conv_file_name(name_buff, EXE_TARGET);
		printf("executing %s:\n", name_buff);

		__run_java(name_buff, NULL);
	}

	printf("\nrunning finishes...\n");
}

/* .sti -> .class 
 * special for running test case */
static void do_stint(char *name)
{
	char name_buff[30];
	char path_buff[50] = "../testcase/";

	/* get name of .sti */
	strcpy(name_buff, name);
	conv_file_name(name_buff, STINT_CODE);

	/* get path of .sti */
	strcat(path_buff, name_buff);

	/* compile .sti -> .class */
	__run_compiler(path_buff, "-c");
}

/* execute .class
 * return -1 if no .class */
static char *do_java(char *name)
{
	char name_buff[30];
	char path_buff[50] = "./";
	char *ret;

	strcpy(name_buff, name);
	conv_file_name(name_buff, JAVA_CLASS);
	strcat(path_buff, name_buff);

	if (access(path_buff, F_OK))
		return NULL;

	strcpy(name_buff, name);
	conv_file_name(name_buff, EXE_TARGET);

	/* get output name */
	ret = (char *)malloc(sizeof(char) * 30);
	strcpy(ret, name);
	conv_file_name(ret, OUTPUT_FILE);
	__run_java(name_buff, ret);

	return ret;
}

static int run_testcase(struct command *cmd)
{
	char *out_name;
	char exp_name[50];
	char name_buff[50];

	/* check compiler binary */
	printf("check compiler binary...\n");
	if (access("./stint", F_OK)) {
		printf("compiler stint doesn't exist, run makefile...\n");
		if (run_makefile() < 0)
			return -1;
	} else
		printf("binary found...\n");

	/* get test case name list from required path */
	load_testcase(cmd);
	printf("%d test cases selected, start running...\n", cmd->arg_count);

	int i = 0;
	int count = 0;
	for (i = 0; i < cmd->arg_count; i++) {

		printf("---------\nrunning test case %s\n", cmd->f_list[i]);

		do_stint(cmd->f_list[i]);
		out_name = do_java(cmd->f_list[i]);
		if (!out_name) {
			printf("compile error\n");
			continue;
		}

		/* get name of correct output file */
		strcpy(exp_name, "../testcase/");
		strcpy(name_buff, cmd->f_list[i]);
		conv_file_name(name_buff, EXP_FILE);
		strcat(exp_name, name_buff);
		do_diff(exp_name, out_name);
		count++;
	}

	printf("---------\n");
	printf("test finished, %d tests in total, %d passed\n", cmd->arg_count, count);

	return 0;
}

static void free_cmd(struct command *cmd)
{
	int i;

	/* free file list */
	if (cmd->f_list) {
		for (i = 0; i < cmd->arg_count; i++)
			if (cmd->f_list[i])
				free(cmd->f_list[i]);
		free(cmd->f_list);
	}

	/* free filter */
	if (cmd->filter)
		free(cmd->filter);
}

int main (int argc, char **argv)
{
	struct command cmd;
	int err;

	/* get user command */
	err = parse_cmd(argc, argv, &cmd);
	if (err < 0) {
		printf("%s\n", strerror((-1)*err));
		goto fail_exit;
	}

	if (cmd.type == MAKEFILE) {
		err = run_makefile();
		if (err < 0)
			goto fail_exit;
	} else if (cmd.type == COMPILE) {
		err = run_compiler(&cmd);
		if (err < 0)
			goto fail_exit;
	} else if (cmd.type == RUNCODE) {
		run_java(&cmd);
	} else if (cmd.type == DEBUG) {
		run_testcase(&cmd);
	}

	free_cmd(&cmd);
	return EXIT_SUCCESS;

fail_exit:
	free_cmd(&cmd);
	return EXIT_FAILURE;
}
