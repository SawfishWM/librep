/*
 * Definitions etc. for regexp(3) routines.
 *
 * Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
 * not the System V one.
 */
#define NSUBEXP  10
typedef struct regexp {
	char *startp[NSUBEXP];
	char *endp[NSUBEXP];
	char regstart;		/* Internal use only. */
	char reganch;		/* Internal use only. */
	char *regmust;		/* Internal use only. */
	int regmlen;		/* Internal use only. */
	char program[1];	/* Unwarranted chumminess with compiler. */
} regexp;

/* eflags for regexec2() */
#define REG_NOTBOL 1
#define REG_NOCASE 2

#define regexec(p,s) regexec2(p,s,0)

#ifdef __STDC__
extern regexp *regcomp(char *);
extern int regexec2(regexp *, char *, int);
extern void regsub(regexp *, char *, char *);
extern int regsublen(regexp *, char *);
extern void regerror(char *);
#else
extern regexp *regcomp();
extern int regexec2();
extern void regsub();
extern int regsub();
extern void regerror();
#endif

/* My Amiga's C library calls str[n]casecmp() str[n]icmp()  */
#ifdef AMIGA
# ifndef strcasecmp
#  define strcasecmp stricmp
# endif
# ifndef strncasecmp
#  define strncasecmp strnicmp
# endif
#endif

