/*
 * Definitions etc. for regexp(3) routines.
 *
 * Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
 * not the System V one.
 */

#ifndef REP_REGEXP_H
#define REP_REGEXP_H

#define rep_NSUBEXP  10

typedef enum rep_regtype {
    rep_reg_string = 0,
    rep_reg_obj
} rep_regtype;

typedef union rep_regsubs {
    struct {
	char *startp[rep_NSUBEXP];
	char *endp[rep_NSUBEXP];
    } string;
    struct {
	repv startp[rep_NSUBEXP];
	repv endp[rep_NSUBEXP];
    } obj;
} rep_regsubs;

typedef struct rep_regexp {
	rep_regtype lasttype;
	rep_regsubs matches;

	u_char regstart;	/* Internal use only. */
	char reganch;		/* Internal use only. */
	char *regmust;		/* Internal use only. */
	int regmlen;		/* Internal use only. */
	int regsize;		/* actual size of regexp structure */
	char program[1];	/* Unwarranted chumminess with compiler. */
} rep_regexp;

/* Data structure used to save and restore regexp data internally */
struct rep_saved_regexp_data {
    struct rep_saved_regexp_data *next;
    rep_regtype type;
    repv data;
    rep_regsubs matches;
};

/* eflags for regexec2() */
#define rep_REG_NOTBOL 1	/* start of input isn't start of line */
#define rep_REG_NOCASE 2	/* fold upper and lower case */
#define rep_REG_1LINE  4	/* for regexec_tx: only search to the
				   end of the line for the start of the
				   match. */

#define rep_regexec(p,s) rep_regexec2(p,s,0)

extern rep_regexp *rep_regcomp(char *);
extern int rep_regexec2(rep_regexp *, char *, int);
extern int rep_regmatch_string(rep_regexp *, char *, int);

extern int rep_regexp_max_depth;


/* Only include the internal stuff if it's explicitly requested, since
   it comtaminates the namespace.. */

#ifdef rep_NEED_REGEXP_INTERNALS

/*
 * Structure for regexp "program".  This is essentially a linear encoding of
 * a nondeterministic finite-state machine (aka syntax charts or "railroad
 * normal form" in parsing technology).  Each node is an opcode plus a "next"
 * pointer, possibly plus an operand.  "Next" pointers of all nodes except
 * BRANCH implement concatenation; a "next" pointer with a BRANCH on both
 * ends of it is connecting two alternatives.	(Here we have one of the
 * subtle syntax dependencies:	an individual BRANCH (as opposed to a
 * collection of them) is never concatenated with anything because of
 * operator precedence.)  The operand of some types of node is a literal
 * string; for others, it is a node leading into a sub-FSM.  In particular,
 * the operand of a BRANCH node is the first node of the branch. (NB this is
 * *not* a tree structure:  the tail of the branch connects to the thing
 * following the set of BRANCHes.)  The opcodes are:
 */

/* definition	number	opnd?	meaning */
#define END	0		/* no	End of program. */
#define BOL	1		/* no	Match "" at beginning of line. */
#define EOL	2		/* no	Match "" at end of line. */
#define ANY	3		/* no	Match any one character. */
#define ANYOF	4		/* str	Match any character in this string. */
#define ANYBUT	5		/* str	Match any character not in this
				 * string. */
#define BRANCH	6		/* node Match this alternative, or the
				 * next... */
#define BACK	7		/* no	Match "", "next" ptr points backward. */
#define EXACTLY 8		/* str	Match this string. */
#define NOTHING 9		/* no	Match empty string. */
#define STAR	10		/* node Match this (simple) thing 0 or more
				 * times. */
#define PLUS	11		/* node Match this (simple) thing 1 or more
				 * times. */
#define WORD	12		/* no   Match alphanumeric or _ char */
#define NWORD	13		/* no   Match non-(alphanumeric or _) char */
#define WSPC	14		/* no   Match whitespace char */
#define NWSPC	15		/* no   Match non-whitespace char */
#define DIGI	16		/* no   Match digit char */
#define NDIGI	17		/* no   Match non-digit char */
#define WEDGE	18		/* no	Match "" at word boundary */
#define NWEDGE	19		/* no	Match "" not at word boundary */
#define OPEN	20		/* no	Mark this point in input as start of
				 * #n. */
/* OPEN+1 is number 1, etc. */
#define CLOSE	30		/* no	Analogous to OPEN. */
#define NGSTAR	40		/* node Match this (simple) thing 0 or more
				   times (non-greedily) */
#define NGPLUS	41		/* node	Match this (simple) thing 1 or more
				   times (non-greedily) */

/*
 * Opcode notes:
 *
 * BRANCH	The set of branches constituting a single choice are hooked together
 * with their "next" pointers, since precedence prevents anything being
 * concatenated to any individual branch.  The "next" pointer of the last
 * BRANCH in a choice points to the thing following the whole choice.  This
 * is also where the final "next" pointer of each individual branch points;
 * each branch starts with the operand node of a BRANCH node.
 *
 * BACK		Normal "next" pointers all implicitly point forward; BACK exists to
 * make loop structures possible.
 *
 * STAR,PLUS	'?', and complex '*' and '+', are implemented as circular
 * BRANCH structures using BACK.  Simple cases (one character per match) are
 * implemented with STAR and PLUS for speed and to minimize recursive
 * plunges.
 *
 * OPEN,CLOSE	...are numbered at compile time.
 */

/*
 * A node is one char of opcode followed by two chars of "next" pointer.
 * "Next" pointers are stored as two 8-bit pieces, high order first.  The
 * value is a positive offset from the opcode of the node containing it. An
 * operand, if any, simply follows the node.  (Note that much of the code
 * generation knows about this implicit relationship.)
 *
 * Using two bytes for the "next" pointer is vast overkill for most things, but
 * allows patterns to get big without disasters.
 */
#define OP(p)	(*(p))
#define NEXT(p) (((*((p)+1)&0377)<<8) + (*((p)+2)&0377))
#define OPERAND(p)	((p) + 3)


/*
 * The first byte of the regexp internal "program" is actually this magic
 * number; the start node begins in the second byte.
 */
#define	MAGIC	0234

#endif /* rep_NEED_REGEXP_INTERNALS */

#endif /* REP_REGEXP_H */
