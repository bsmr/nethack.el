/* nhlaunch.c -- Handle logging in and launching nethack
   Author: Shawn Betts
   Copyright (c) 2004,2005 Shawn Betts

To build on linux:
   gcc -D__LINUX__ nhlaunch.c -o nhlaunch -lcrypt
To build on OpenBSD
   gcc nhlaunch.c -o nhlaunch -lcrypto
*/

#ifdef __LINUX__
#include <crypt.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <errno.h>
#include <string.h>

#ifndef BUFSIZ
#  define BUFSIZ 256
#endif

#define UNKNOWN_USER 	0
#define BAD_PASSWD 	1
#define SUCCESS 	2

/* Config values. Tweak these to fit your system */
#define USER_FILE 	"/etc/nh-login"
#define CHROOT_DIR 	"/home/sabetts/src/nethack-el/gl"
#define SHED_UID 	1000
#define SHED_GID 	1000

struct game_t
{
  char *name;
  char *path;
};

/* This structure describes the games are playable on this server. The
   MUST be at least one entry. */
struct game_t games[] = {{"nethack", "/usr/games/nethack"},
			 {"slashem", "/usr/games/slashem"},
			 /* The last entry must be NULL. */
			 {NULL, NULL}};

char *
find_program(char *game)
{
  struct game_t *i;

  for (i=games; i->name; i++)
    {
      if (!strcmp(game, i->name))
	return i->path;
    }
  /* No match? pick the first one in the list. */
  return games[0].path;
}

void
list_programs()
{
  struct game_t *i;

  for (i=games; i->name; i++)
    printf("%s\n", i->name);
}


int
valid_user_p(char *username, char *passwd)
{
  FILE *fd;
  char line[BUFSIZ];

  if ((fd = fopen(USER_FILE, "r")) == NULL)
    exit(100);

  while (fgets(line, BUFSIZ, fd) != NULL)
    {
      char *n;
      char *p;
      n = (char *)strtok(line, ",");
      p = (char *)strtok(NULL, "\n");
      if (!strcmp(n, username))
	{
	  fclose(fd);
	  if (!strcmp(crypt(passwd,passwd),p)) 
	    return SUCCESS;
	  else
	    return BAD_PASSWD;
	}
      
    }

  fclose(fd);
  return UNKNOWN_USER;
}

void
add_user(char *username, char *passwd)
{
  FILE *fd, *fpl;

  /* Always lock the file before writting. */
  fpl = fopen("/var/lock/nh-lock","r");
  if (!fpl) exit(101);
  if (flock(fileno(fpl),LOCK_SH)) exit(102);

  if ((fd = fopen(USER_FILE, "a+")) == NULL)
    exit(103);

  fprintf(fd, "%s,%s\n", username, crypt(passwd,passwd));
  fclose(fd);

  /* Unlock. */
  flock(fileno(fpl),LOCK_UN);
  fclose(fpl);
}

int logged_in = 0;
char *player_name = NULL;

void
read_cmd()
{
  char line[BUFSIZ];

  /* The client disconnected. */
  if (fgets(line, BUFSIZ, stdin) == NULL)
    exit(0);

  if (!strncmp(line, "login", 5))
    {
      char *name, *pass, *cmd;
      int ret;
      cmd = (char *)strtok(line, " ");
      name = (char *)strtok(NULL, " ");
      pass = (char *)strtok(NULL, " ");
      if (!name || !pass)
	{
	  printf ("Error parsing name and password.\n");
	}
      else 
	{
	  ret = valid_user_p(name, pass);
	  if (ret == SUCCESS)
	    {
	      if (logged_in)
		{
		  printf("You are already logged in.\n");
		}
	      else
		{
		  logged_in = 1;
		  if (player_name != NULL)
		    free(player_name);
		  player_name = (char *)strdup(name);
		  printf("Welcome back %s.\n", name);
		}
	    }
	  else if (ret == BAD_PASSWD)
	    {
	      printf("Failed to login %s.\n", name);
	    }
	  else
	    {
	      printf("Unknown user %s.\n", name);
	    }
	}
    }
  else if (!strncmp(line, "list", 4))
    {
      list_programs();
    }
  else if (!strncmp(line, "play", 4))
    {
      if (logged_in)
	{
	  int pid;
	  char *cmd, *game, *program;
	  /* Find out which game to play. */
	  cmd = (char *)strtok(line, " \n");
	  game = (char *)strtok(NULL, " \n");
	  if (game)
	    program = find_program(game);
	  else
	    program = find_program("");
	  /* Fork and run the game. */
	  pid = fork();
	  if (pid == 0)
	    execlp(program, program, "-u", player_name, (char *)NULL);
	  else
	    waitpid(pid, NULL, 0);
	  exit(0);
	}
      else
	printf("You must login first.\n");
    }
  else if (!strncmp(line, "new", 3))
    {
      char *name, *pass, *cmd;
      cmd = (char *)strtok(line, " ");
      name = (char *)strtok(NULL, " ");
      pass = (char *)strtok(NULL, " ");
      if (!name || !pass || strchr(name,','))
	{
	  printf ("Error parsing name and password.\n");
	}
      else if (valid_user_p(name, pass) == UNKNOWN_USER)
	{
	  add_user(name, pass);
	  printf ("User %s added successfully.\n", name);
	}
      else
	{
	  printf ("User %s already exists.\n", name);
	}
    }
  else
    {
      printf("Unknown command\n");
    }
}

void
repl()
{
  while(1) 
    {
      read_cmd();
      fflush(stdout);
    }
}

int
main(int argc, char **argv)
{
  /* chroot */
  if (chroot (CHROOT_DIR))
    perror ("cannot change root directory");
  if (chdir ("/"))
    perror ("cannot chdir to root directory");

  /* shed privs. this is done immediately after chroot. */
  setgid(SHED_UID);
  setuid(SHED_GID);

  printf("Welcome to the nethack-el server.\n");
  fflush(stdout);
  repl();

  return 0;
}
