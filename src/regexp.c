/*
 * regcomp and regexec -- regsub and regerror are elsewhere @(#)regexp.c 1.3
 * of 18 April 87
 *
 * Copyright (c) 1986 by University of Toronto. Written by Henry Spencer.  Not
 * derived from licensed software.
 *
 * Permission is granted to anyone to use this software for any purpose on any
 * computer system, and to redistribute it freely, subject to the following
 * restrictions:
 *
 * 1. The author is not responsible for the consequences of use of this
 * software, no matter how awful, even if they arise from defects in it.
 *
 * 2. The origin of this software must not be misrepresented, either by explicit
 * claim or by omission.
 *
 * 3. Altered versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 *
 * Beware that some of this code is subtly aware of the way operator precedence
 * is structured in regular expressions.  Serious changes in
 * regular-expression syntax might require a total rethink.
 */

/* Lots of changes for Jade. See the file README.regexp for more details */

#define _GNU_SOURCE

#define rep_NEED_REGEXP_INTERNALS
#include "repint.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#undef DEBUG

/*
 * Utility definitions.
 */
#ifndef CHARBITS
#define UCHARAT(p)	((int)*(unsigned char *)(p))
#else
#define UCHARAT(p)	((int)*(p)&CHARBITS)
#endif

#define FAIL(m) { rep_regerror(m); return(NULL); }
#define ISMULT(c)	((c) == '*' || (c) == '+' || (c) == '?')
#define META	"^$.[()|?+*\\"

/*
 * Flags to be passed up and down.
 */
#define HASWIDTH	01	/* Known never to match null string. */
#define SIMPLE		02	/* Simple enough to be STAR/PLUS operand. */
#define SPSTART		04	/* Starts with * or +. */
#define WORST		0	/* Worst case. */

/*
 * Global work variables for regcomp().
 */
static char    *regparse;	/* Input-scan pointer. */
static int	regnpar;	/* () count. */
static char	regdummy;
static char    *regcode;	/* Code-emit pointer; &regdummy = don't. */
static long	regsize;	/* Code size. */

/*
 * Forward declarations for regcomp()'s friends. 
 */
static char    *reg(int, int *);
static char    *regbranch(int *);
static char    *regpiece(int *);
static char    *regatom(int *);
static char    *regnode(char);
static char    *regnext(char *);
static void	regc(char);
static void	reginsert(char, char *);
static void	regtail(char *, char *);
static void	regoptail(char *, char *);
extern void	rep_regerror(char *);
#ifdef DEBUG
void		regdump(rep_regexp *);
int		regenable_debug = 0;
#endif

#ifndef HAVE_STRCSPN
int		strcspn(char *, char *);
#endif

/*
 * - regcomp - compile a regular expression into internal code
 *
 * We can't allocate space until we know how big the compiled form will be, but
 * we can't compile it (and thus know how big it is) until we've got a place
 * to put the code.  So we cheat:  we compile it twice, once with code
 * generation turned off and size counting turned on, and once "for real".
 * This also means that we don't allocate space until we are sure that the
 * thing really will compile successfully, and we never have to move the code
 * and thus invalidate pointers into it.  (Note that it has to be in one
 * piece because free() must be able to free it all.)
 *
 * Beware that the optimization-preparation code in here knows about some of the
 * structure of the compiled regexp.
 */
rep_regexp *
rep_regcomp(char *exp)
{
    register rep_regexp *r;
    register char  *scan;
    register char  *longest;
    register int    len;
    int		    flags;

    if (exp == NULL)
	FAIL("NULL argument");

    /* First pass: determine size, legality. */
    regparse = exp;
    regnpar = 1;
    regsize = 0L;
    regcode = &regdummy;
    regc(MAGIC);
    if (reg(0, &flags) == NULL)
	return (NULL);

    /* Small enough for pointer-storage convention? */
    if (regsize >= 32767L)	/* Probably could be 65535L. */
	FAIL("regexp too big");

    /* Allocate space. */
    r = (rep_regexp *) malloc(sizeof(rep_regexp) + (unsigned) regsize);
    if (r == NULL)
	FAIL("out of space");

    /* Second pass: emit code. */
    regparse = exp;
    regnpar = 1;
    regcode = r->program;
    regc(MAGIC);
    if (reg(0, &flags) == NULL)
	return (NULL);

    /* Dig out information for optimizations. */
    r->regstart = '\0';         /* Worst-case defaults. */
    r->reganch = 0;
    r->regmust = NULL;
    r->regmlen = 0;
    r->regsize = sizeof(rep_regexp) + (unsigned)regsize;
    scan = r->program + 1;	/* First BRANCH. */
    if (OP(regnext(scan)) == END) {	/* Only one top-level choice. */
	scan = OPERAND(scan);

	/* Starting-point info. */
	if (OP(scan) == EXACTLY)
	    r->regstart = UCHARAT(OPERAND(scan));
	else if (OP(scan) == BOL)
	    r->reganch++;

	/*
	 * If there's something expensive in the r.e., find the longest
	 * literal string that must appear and make it the regmust.  Resolve
	 * ties in favor of later strings, since the regstart check works
	 * with the beginning of the r.e. and avoiding duplication
	 * strengthens checking.  Not a strong reason, but sufficient in the
	 * absence of others.
	 */
	if (flags & SPSTART) {
	    longest = NULL;
	    len = 0;
	    for (; scan != NULL; scan = regnext(scan))
		if (OP(scan) == EXACTLY && strlen(OPERAND(scan)) >= len) {
		    longest = OPERAND(scan);
		    len = strlen(OPERAND(scan));
		}
	    r->regmust = longest;
	    r->regmlen = len;
	}
    }
#ifdef DEBUG
    if (regenable_debug) {
	printf ("compiled `%s' to:\n", exp);
	regdump (r);
    }
#endif
    return (r);
}

/*
 * - reg - regular expression, i.e. main body or parenthesized thing
 *
 * Caller must absorb opening parenthesis.
 *
 * Combining parenthesis handling with the base level of regular expression is a
 * trifle forced, but the need to tie the tails of the branches to what
 * follows makes it hard to avoid.
 */
static char *
reg(int paren, int *flagp)
{
    register char  *ret;
    register char  *br;
    register char  *ender;
    register int    parno = 0;
    int		    flags;

    *flagp = HASWIDTH;		/* Tentatively. */

    /* Make an OPEN node, if parenthesized. */
    if (paren) {
	if (regnpar >= rep_NSUBEXP)
	    FAIL("too many ()");
	parno = regnpar;
	regnpar++;
	ret = regnode(OPEN + parno);
    } else
	ret = NULL;

    /* Pick up the branches, linking them together. */
    br = regbranch(&flags);
    if (br == NULL)
	return (NULL);
    if (ret != NULL)
	regtail(ret, br);	/* OPEN -> first. */
    else
	ret = br;
    if (!(flags & HASWIDTH))
	*flagp &= ~HASWIDTH;
    *flagp |= flags & SPSTART;
    while (*regparse == '|') {
	regparse++;
	br = regbranch(&flags);
	if (br == NULL)
	    return (NULL);
	regtail(ret, br);	/* BRANCH -> BRANCH. */
	if (!(flags & HASWIDTH))
	    *flagp &= ~HASWIDTH;
	*flagp |= flags & SPSTART;
    }

    /* Make a closing node, and hook it on the end. */
    ender = regnode((paren) ? CLOSE + parno : END);
    regtail(ret, ender);

    /* Hook the tails of the branches to the closing node. */
    for (br = ret; br != NULL; br = regnext(br))
	regoptail(br, ender);

    /* Check for proper termination. */
    if (paren && *regparse++ != ')') {
	FAIL("unmatched ()");
    } else if (!paren && *regparse != '\0') {
	if (*regparse == ')') {
	    FAIL("unmatched ()");
	} else
	    FAIL("junk on end");/* "Can't happen". */
	/* NOTREACHED */
    }
    return (ret);
}

/*
 * - regbranch - one alternative of an | operator
 *
 * Implements the concatenation operator.
 */
static char *
regbranch(int *flagp)
{
    register char  *ret;
    register char  *chain;
    register char  *latest;
    int		    flags;

    *flagp = WORST;		/* Tentatively. */

    ret = regnode(BRANCH);
    chain = NULL;
    while (*regparse != '\0' && *regparse != '|' && *regparse != ')') {
	latest = regpiece(&flags);
	if (latest == NULL)
	    return (NULL);
	*flagp |= flags & HASWIDTH;
	if (chain == NULL)	/* First piece. */
	    *flagp |= flags & SPSTART;
	else
	    regtail(chain, latest);
	chain = latest;
    }
    if (chain == NULL)		/* Loop ran zero times. */
	(void) regnode(NOTHING);

    return (ret);
}

/*
 * - regpiece - something followed by possible [*+?]
 *
 * Note that the branching code sequences used for ? and the general cases of *
 * and + are somewhat optimized:  they use the same NOTHING node as both the
 * endmarker for their branch list and the body of the last branch. It might
 * seem that this node could be dispensed with entirely, but the endmarker
 * role is not redundant.
 */
static char *
regpiece(int *flagp)
{
    register char  *ret;
    register char   op;
    register char  *next;
    int		    flags;
    int             greedy;

    ret = regatom(&flags);
    if (ret == NULL)
	return (NULL);

    op = *regparse;
    if (!ISMULT(op)) {
	*flagp = flags;
	return (ret);
    }
    if (!(flags & HASWIDTH) && op != '?')
	FAIL("*+ operand could be empty");
    *flagp = (op != '+') ? (WORST | SPSTART) : (WORST | HASWIDTH);
    greedy = (regparse[1] != '?');

    if (op == '*' && (flags & SIMPLE))
	reginsert(greedy ? STAR : NGSTAR, ret);
    else if (op == '*') {
	if (greedy) {
	    /* Emit x* as (x&|), where & means "self". */
	    reginsert(BRANCH, ret);		/* Either x */
	    regoptail(ret, regnode(BACK));	/* and loop */
	    regoptail(ret, ret);		/* back */
	    regtail(ret, regnode(BRANCH));	/* or */
	    regtail(ret, regnode(NOTHING));	/* null. */
	} else {
	    /* Emit x*? as (|x&), where & means "self". */
	    reginsert(BRANCH, ret);		/* Either */
	    reginsert(NOTHING, ret);		/* null. */
	    reginsert(BRANCH, ret);		/* or x */
	    regtail(ret+9, regnode(BACK));	/* and loop */
	    regtail(ret+9, ret);		/* back */
	    regtail(ret, ret+6);
	    regtail(ret+3, regcode);
	}	    
    } else if (op == '+' && (flags & SIMPLE))
	reginsert(greedy ? PLUS : NGPLUS, ret);
    else if (op == '+') {
	if (greedy) {
	    /* Emit x+ as x(&|), where & means "self". */
	    next = regnode(BRANCH);		/* Either */
	    regtail(ret, next);
	    regtail(regnode(BACK), ret);	/* loop back */
	    regtail(next, regnode(BRANCH)); 	/* or */
	    regtail(ret, regnode(NOTHING)); 	/* null. */
	} else {
	    char *null, *b2;
	    /* Emit x+? as x(|&), where & means "self". */
	    next = regnode(BRANCH);		/* Either */
	    regtail(ret, next);
	    null = regnode(NOTHING);		/* null */
	    b2 = regnode(BRANCH);
	    regtail(regnode(BACK), ret);	/* or loop back */
	    regtail(next, b2);
	    regtail(null, regcode);
	}
    } else if (op == '?') {
	if (greedy) {
	    /* Emit x? as (x|) */
	    reginsert(BRANCH, ret);		/* Either x */
	    regtail(ret, regnode(BRANCH));	/* or */
	    next = regnode(NOTHING);		/* null. */
	    regtail(ret, next);
	    regoptail(ret, next);
	} else {
	    /* Emit x?? as (|x) */
	    reginsert(BRANCH, ret);
	    reginsert(NOTHING, ret);		/* Either null */
	    reginsert(BRANCH, ret);		/* or x. */
	    regoptail(ret, regcode);
	    regtail(ret, ret + 6);
	    regtail(ret, regcode);
	}
    }
    if (greedy)
	regparse++;
    else
	regparse += 2;
    if (ISMULT(*regparse))
	FAIL("nested *?+");

    return (ret);
}

/*
 * - regatom - the lowest level
 *
 * Optimization:  gobbles an entire sequence of ordinary characters so that it
 * can turn them into a single node, which is smaller to store and faster to
 * run.	 Backslashed characters are exceptions, each becoming a separate
 * node; the code is simpler that way and it's not worth fixing. 
 */
static char *
regatom(int *flagp)
{
    register char  *ret;
    int		    flags;

    *flagp = WORST;		/* Tentatively. */

    switch (*regparse++) {
    case '^':
	ret = regnode(BOL);
	break;
    case '$':
	ret = regnode(EOL);
	break;
    case '.':
	ret = regnode(ANY);
	*flagp |= HASWIDTH | SIMPLE;
	break;
    case '[':{
	    register int    class;
	    register int    classend;

	    if (*regparse == '^') {     /* Complement of range. */
		ret = regnode(ANYBUT);
		regparse++;
	    } else
		ret = regnode(ANYOF);
	    if (*regparse == ']' || *regparse == '-')
		regc(*regparse++);
	    while (*regparse != '\0' && *regparse != ']') {
		if (*regparse == '-') {
		    regparse++;
		    if (*regparse == ']' || *regparse == '\0')
			regc('-');
		    else {
			class = UCHARAT(regparse - 2) + 1;
			classend = UCHARAT(regparse);
			if (class > classend + 1)
			    FAIL("invalid [] range");
			for (; class <= classend; class++)
			    regc(class);
			regparse++;
		    }
		} else
		    regc(*regparse++);
	    }
	    regc('\0');
	    if (*regparse != ']')
		FAIL("unmatched []");
	    regparse++;
	    *flagp |= HASWIDTH | SIMPLE;
	}
	break;
    case '(':
	ret = reg(1, &flags);
	if (ret == NULL)
	    return (NULL);
	*flagp |= flags & (HASWIDTH | SPSTART);
	break;
    case '\0':
    case '|':
    case ')':
	FAIL("internal urp");   /* Supposed to be caught earlier. */
	break;
    case '?':
    case '+':
    case '*':
	FAIL("?+* follows nothing");
	break;
    case '\\':
	switch (*regparse++)
	{
	case '\0':
	    FAIL("trailing \\");
	    break;
	case 'w':
	    ret = regnode (WORD);
	    *flagp |= HASWIDTH | SIMPLE;
	    break;
	case 'W':
	    ret = regnode (NWORD);
	    *flagp |= HASWIDTH | SIMPLE;
	    break;
	case 's':
	    ret = regnode (WSPC);
	    *flagp |= HASWIDTH | SIMPLE;
	    break;
	case 'S':
	    ret = regnode (NWSPC);
	    *flagp |= HASWIDTH | SIMPLE;
	    break;
	case 'd':
	    ret = regnode (DIGI);
	    *flagp |= HASWIDTH | SIMPLE;
	    break;
	case 'D':
	    ret = regnode (NDIGI);
	    *flagp |= HASWIDTH | SIMPLE;
	    break;
	case 'b':
	    ret = regnode (WEDGE);
	    break;
	case 'B':
	    ret = regnode (NWEDGE);
	    break;
	default:
	    ret = regnode(EXACTLY);
	    regc(regparse[-1]);
	    regc('\0');
	    *flagp |= HASWIDTH | SIMPLE;
	}
	break;
    default:{
	    register int    len;
	    register char   ender;

	    regparse--;
	    len = strcspn(regparse, META);
	    if (len <= 0)
		FAIL("internal disaster");
	    ender = *(regparse + len);
	    if (len > 1 && ISMULT(ender))
		len--;		/* Back off clear of ?+* operand. */
	    *flagp |= HASWIDTH;
	    if (len == 1)
		*flagp |= SIMPLE;
	    ret = regnode(EXACTLY);
	    while (len > 0) {
		regc(*regparse++);
		len--;
	    }
	    regc('\0');
	}
	break;
    }

    return (ret);
}

/*
 * - regnode - emit a node
 */
static char *		/* Location. */
regnode(char op)
{
    register char  *ret;
    register char  *ptr;

    ret = regcode;
    if (ret == &regdummy) {
	regsize += 3;
	return (ret);
    }
    ptr = ret;
    *ptr++ = op;
    *ptr++ = '\0';              /* Null "next" pointer. */
    *ptr++ = '\0';
    regcode = ptr;

    return (ret);
}

/*
 * - regc - emit (if appropriate) a byte of code
 */
static void
regc(char b)
{
    if (regcode != &regdummy)
	*regcode++ = b;
    else
	regsize++;
}

/*
 * - reginsert - insert an operator in front of already-emitted operand
 *
 * Means relocating the operand.
 */
static void
reginsert(char op, char *opnd)
{
    register char  *src;
    register char  *dst;
    register char  *place;

    if (regcode == &regdummy) {
	regsize += 3;
	return;
    }
    src = regcode;
    regcode += 3;
    dst = regcode;
    while (src > opnd)
	*--dst = *--src;

    place = opnd;		/* Op node, where operand used to be. */
    *place++ = op;
    *place++ = '\0';
    *place++ = '\0';
}

/*
 * - regtail - set the next-pointer at the end of a node chain
 */
static void
regtail(char *p, char *val)
{
    register char  *scan;
    register char  *temp;
    register int    offset;

    if (regcode == &regdummy)
	return;

    /* Find last node. */
    scan = p;
    for (;;) {
	temp = regnext(scan);
	if (temp == NULL)
	    break;
	scan = temp;
    }

    if (OP(scan) == BACK)
	offset = scan - val;
    else
	offset = val - scan;
    *(scan + 1) = (offset >> 8) & 0377;
    *(scan + 2) = offset & 0377;
}

/*
 * - regoptail - regtail on operand of first argument; nop if operandless
 */
static void
regoptail(char *p, char *val)
{
    /* "Operandless" and "op != BRANCH" are synonymous in practice. */
    if (p == NULL || regcode == &regdummy || OP(p) != BRANCH)
	return;
    regtail(OPERAND(p), val);
}

/*
 * regexec and friends
 */

/*
 * Global work variables for regexec().
 */
static char    *reginput;	/* String-input pointer. */
static char    *regbol;		/* Beginning of input, for ^ check. */
static char   **regstartp;	/* Pointer to startp array. */
static char   **regendp;	/* Ditto for endp. */
static char	regnocase;	/* Ignore case when string-matching. */
static int	regnest;	/* depth of recursion */

int rep_regexp_max_depth = 2048;

/*
 * Forwards.
 */
static int	regtry(rep_regexp *, char *);
static int	regmatch(char *);
static int	regrepeat(char *);

#ifdef DEBUG
int		regnarrate = 0;
char		*regprop(char *);
#endif /* DEBUG */


/*
 * - regexec - match a regexp against a string
 *
 * jsh -- changed regexec to regexec2 with an extra argument for flag bits,
 * flags are REG_NOTBOL and REG_NOCASE.
 */
int
rep_regexec2(rep_regexp *prog, char *string, int eflags)
{
    register char  *s;
    /* For REG_NOCASE and strpbrk()  */
    static char mat[3] = "xX";

    /* Be paranoid... */
    if (prog == NULL || string == NULL) {
	rep_regerror("NULL parameter");
	return (0);
    }
    /* Check validity of program. */
    if (UCHARAT(prog->program) != MAGIC) {
	rep_regerror("corrupted program");
	return (0);
    }

    /* jsh -- Check for REG_NOCASE, means ignore case in string matches.  */
    regnocase = ((eflags & rep_REG_NOCASE) != 0);

    /* If there is a "must appear" string, look for it. */
    if (prog->regmust != NULL)
    {
	s = string;
	if(regnocase)
	{
	    mat[0] = tolower(UCHARAT(prog->regmust));
	    mat[1] = toupper(UCHARAT(prog->regmust));
	    while ((s = strpbrk(s, mat)) != NULL)
	    {
		if(strncasecmp(s, prog->regmust, prog->regmlen) == 0)
		    break;	    /* Found it. */
		s++;
	    }
	}
	else
	{
	    while ((s = strchr(s, prog->regmust[0])) != NULL)
	    {
		if(strncmp(s, prog->regmust, prog->regmlen) == 0)
		    break;	    /* Found it. */
		s++;
	    }
	}
	if (s == NULL)		/* Not present. */
	    return (0);
    }
    /* Mark beginning of line for ^ . */
    /* jsh -- if REG_NOTBOL is set then set regbol to something absurd
       to guarantee ^ doesn't match */
    regbol = (eflags & rep_REG_NOTBOL) ? "" : string;

    /* Simplest case:  anchored match need be tried only once. */
    if (prog->reganch)
	return (regtry(prog, string));

    /* Messy cases:  unanchored match. */
    s = string;
    if (prog->regstart != '\0')
    {
	/* We know what char it must start with. */
	if(regnocase)
	{
	    mat[0] = tolower(prog->regstart);
	    mat[1] = toupper(prog->regstart);
	    while((s = strpbrk(s, mat)) != NULL)
	    {
		if(regtry(prog, s))
		    return (1);
		s++;
	    }
	}
	else
	{
	    while((s = strchr(s, prog->regstart)) != NULL)
	    {
		if(regtry(prog, s))
		    return (1);
		s++;
	    }
	}
    }
    else
	/* We don't -- general case. */
	do {
	    if (regtry(prog, s))
		return (1);
	} while (*s++ != '\0');

    /* Failure. */
    return (0);
}

/*
 * - regmatch_string - match a regexp against the string STRING.
 *   No searching
 */
int
rep_regmatch_string(rep_regexp *prog, char *string, int eflags)
{
    /* Check for REG_NOCASE, means ignore case in string matches.  */
    regnocase = ((eflags & rep_REG_NOCASE) != 0);

    /* Mark beginning of line for ^ . */
    /* jsh -- if REG_NOTBOL is set then set regbol to something absurd
       to guarantee ^ doesn't match */
    regbol = (eflags & rep_REG_NOTBOL) ? "" : string;

    return regtry(prog, string);
}

/*
 * - regtry - try match at specific point
 */
static int			/* 0 failure, 1 success */
regtry(rep_regexp *prog, char *string)
{
    register int    i;
    register char **sp;
    register char **ep;

    reginput = string;
    regstartp = prog->matches.string.startp;
    regendp = prog->matches.string.endp;
    regnest = 0;

    sp = prog->matches.string.startp;
    ep = prog->matches.string.endp;
    for (i = rep_NSUBEXP; i > 0; i--) {
	*sp++ = NULL;
	*ep++ = NULL;
    }
    if (regmatch(prog->program + 1)) {
	regstartp[0] = string;
	regendp[0] = reginput;
	prog->lasttype = rep_reg_string;
	return (1);
    } else
	return (0);
}

/* get around the insane number of return statements in regmatch () */
static inline int
nested_regmatch (char *prog)
{
    int ret;
    regnest++;
    ret = regmatch (prog);
    regnest--;
    return ret;
}

/*
 * - regmatch - main matching routine
 *
 * Conceptually the strategy is simple:	 check to see whether the current node
 * matches, call self recursively to see whether the rest matches, and then
 * act accordingly.  In practice we make some effort to avoid recursion, in
 * particular by going through "ordinary" nodes (that don't need to know
 * whether the rest of the match failed) by a loop instead of by recursion.
 */
static int			/* 0 failure, 1 success */
regmatch(char *prog)
{
    register char  *scan;	/* Current node. */
    char	   *next;	/* Next node. */

    if (regnest >= rep_regexp_max_depth)
    {
	/* recursion overload, bail out */
	rep_regerror ("stack overflow");
	return 0;
    }

    scan = prog;
#ifdef DEBUG
    if (scan != NULL && regnarrate)
	fprintf(stderr, "%s(\n", regprop(scan));
#endif
    while (scan != NULL) {
#ifdef DEBUG
	if (regnarrate)
	    fprintf(stderr, "%s...\n", regprop(scan));
#endif
	next = regnext(scan);

	switch (OP(scan)) {
	case BOL:
	    if (reginput != regbol)
		return (0);
	    break;
	case EOL:
	    if (*reginput != '\0')
		return (0);
	    break;
	case ANY:
	    if (*reginput == '\0')
		return (0);
	    reginput++;
	    break;
	case EXACTLY:{
		register int	len;
		register char  *opnd;
		opnd = OPERAND(scan);
		if(regnocase)
		{
		    /* Inline the first character, for speed. */
		    if(toupper(UCHARAT(opnd)) != toupper(UCHARAT(reginput)))
			return (0);
		    len = strlen(opnd);
		    if(len > 1 && strncasecmp(opnd, reginput, len) != 0)
			return (0);
		}
		else
		{
		    /* Inline the first character, for speed. */
		    if(*opnd != *reginput)
			return (0);
		    len = strlen(opnd);
		    if(len > 1 && strncmp(opnd, reginput, len) != 0)
			return (0);
		}
		reginput += len;
	    }
	    break;
	case ANYOF:
	    if (*reginput == '\0' || strchr(OPERAND(scan), *reginput) == NULL)
		return (0);
	    reginput++;
	    break;
	case ANYBUT:
	    if (*reginput == '\0' || strchr(OPERAND(scan), *reginput) != NULL)
		return (0);
	    reginput++;
	    break;
	case NOTHING:
	    break;
	case BACK:
	    break;
	case OPEN + 1:
	case OPEN + 2:
	case OPEN + 3:
	case OPEN + 4:
	case OPEN + 5:
	case OPEN + 6:
	case OPEN + 7:
	case OPEN + 8:
	case OPEN + 9:{
		register int	no;
		register char  *save;

		no = OP(scan) - OPEN;
		save = reginput;

		if (nested_regmatch(next)) {
		    /*
		     * Don't set startp if some later invocation of the same
		     * parentheses already has.
		     */
		    if (regstartp[no] == NULL)
			regstartp[no] = save;
		    return (1);
		} else
		    return (0);
	    }
	    break;
	case CLOSE + 1:
	case CLOSE + 2:
	case CLOSE + 3:
	case CLOSE + 4:
	case CLOSE + 5:
	case CLOSE + 6:
	case CLOSE + 7:
	case CLOSE + 8:
	case CLOSE + 9:{
		register int	no;
		register char  *save;

		no = OP(scan) - CLOSE;
		save = reginput;

		if (nested_regmatch(next)) {
		    /*
		     * Don't set endp if some later invocation of the same
		     * parentheses already has.
		     */
		    if (regendp[no] == NULL)
			regendp[no] = save;
		    return (1);
		} else
		    return (0);
	    }
	    break;
	case BRANCH:{
		register char  *save;

		if (OP(next) != BRANCH) /* No choice. */
		    next = OPERAND(scan);	/* Avoid recursion. */
		else {
		    do {
			save = reginput;
			if (nested_regmatch(OPERAND(scan)))
			    return (1);
			reginput = save;
			scan = regnext(scan);
		    } while (scan != NULL && OP(scan) == BRANCH);
		    return (0);
		    /* NOTREACHED */
		}
	    }
	    break;
	case STAR:
	case PLUS:{
		register u_char	nextch;
		register int	no;
		register char  *save;
		register int	min;

		/*
		 * Lookahead to avoid useless match attempts when we know
		 * what character comes next.
		 */
		nextch = '\0';
		if (OP(next) == EXACTLY)
		    nextch = UCHARAT(OPERAND(next));
		if(regnocase)
		    nextch = toupper(nextch);
		min = (OP(scan) == STAR) ? 0 : 1;
		save = reginput;
		no = regrepeat(OPERAND(scan));
		while (no >= min) {
		    /* If it could work, try it. */
		    if (nextch == '\0'
			|| (regnocase ? toupper(UCHARAT(reginput))
			    : *reginput) == nextch)
			if (nested_regmatch(next))
			    return (1);
		    /* Couldn't or didn't -- back up. */
		    no--;
		    reginput = save + no;
		}
		return (0);
	    }
	    break;
	case NGSTAR:
	case NGPLUS:{
		register u_char	nextch;
		register int	no;
		register char  *save;
		register int	max;

		/*
		 * Lookahead to avoid useless match attempts when we know
		 * what character comes next.
		 */
		nextch = '\0';
		if (OP(next) == EXACTLY)
		    nextch = UCHARAT(OPERAND(next));
		if(regnocase)
		    nextch = toupper(nextch);
		no = (OP(scan) == NGSTAR) ? 0 : 1;
		save = reginput;
		max = regrepeat(OPERAND(scan));
		while (no <= max) {
		    reginput = save + no;
		    /* If it could work, try it. */
		    if (nextch == '\0'
			|| (regnocase ? toupper(UCHARAT(reginput))
			    : *reginput) == nextch)
			if (nested_regmatch(next))
			    return (1);
		    /* Couldn't or didn't -- move up. */
		    no++;
		}
		return (0);
	    }
	    break;
	case WORD:
	    if (*reginput != '_' && !isalnum (UCHARAT(reginput)))
		return 0;
	    reginput++;
	    break;
	case NWORD:
	    if (*reginput == '_' || isalnum (UCHARAT(reginput)))
		return 0;
	    reginput++;
	    break;
	case WSPC:
	    if (!isspace (UCHARAT(reginput)))
		return 0;
	    reginput++;
	    break;
	case NWSPC:
	    if (isspace (UCHARAT(reginput)))
		return 0;
	    reginput++;
	    break;
	case DIGI:
	    if (!isdigit (UCHARAT(reginput)))
		return 0;
	    reginput++;
	    break;
	case NDIGI:
	    if (isdigit (UCHARAT(reginput)))
		return 0;
	    reginput++;
	    break;
	case WEDGE:
	    if (reginput == regbol || *reginput == '\0'
		|| ((reginput[-1] == '_' || isalnum (UCHARAT(reginput - 1)))
		    && (*reginput != '_' && !isalnum (UCHARAT(reginput))))
		|| ((reginput[-1] != '_' && !isalnum (UCHARAT(reginput - 1)))
		    && (*reginput == '_' || isalnum (UCHARAT(reginput)))))
		break;
	    return 0;
	case NWEDGE:
	    if (!(reginput == regbol || *reginput == '\0'
		  || ((reginput[-1] == '_' || isalnum (UCHARAT(reginput - 1)))
		      && (*reginput != '_' && !isalnum (UCHARAT(reginput))))
		  || ((reginput[-1] != '_' && !isalnum (UCHARAT(reginput - 1)))
		      && (*reginput == '_' || isalnum (UCHARAT(reginput))))))
		break;
	    return 0;
	case END:
	    return (1);		/* Success! */
	    break;
	default:
	    rep_regerror("memory corruption");
	    return (0);
	    break;
	}

	scan = next;
    }

    /*
     * We get here only if there's trouble -- normally "case END" is the
     * terminating point.
     */
    rep_regerror("corrupted pointers");
    return (0);
}

/*
 * - regrepeat - repeatedly match something simple, report how many
 */
static int
regrepeat(char *p)
{
    int count;
    register char  *scan;
    register char  *opnd;

    scan = reginput;
    opnd = OPERAND(p);
    switch (OP(p)) {
    case ANY:
	scan += strlen(scan);
	break;
    case EXACTLY:
	if(regnocase)
	{
	    while(toupper(UCHARAT(opnd)) == toupper(UCHARAT(scan))) {
		scan++;
	    }
	}
	else
	{
	    while(*opnd == *scan) {
		scan++;
	    }
	}
	break;
    case ANYOF:
	while (*scan != '\0' && strchr(opnd, *scan) != NULL) {
	    scan++;
	}
	break;
    case ANYBUT:
	while (*scan != '\0' && strchr(opnd, *scan) == NULL) {
	    scan++;
	}
	break;
    case WORD:
	while (*scan != '\0' && (*scan == '_' || isalnum (UCHARAT(scan)))) {
	    scan++;
	}
	break;
    case NWORD:
	while (*scan != '\0' && (*scan != '_' && !isalnum (UCHARAT(scan)))) {
	    scan++;
	}
	break;
    case WSPC:
	while (*scan != '\0' && isspace (UCHARAT(scan))) {
	    scan++;
	}
	break;
    case NWSPC:
	while (*scan != '\0' && !isspace (UCHARAT(scan))) {
	    scan++;
	}
	break;
    case DIGI:
	while (*scan != '\0' && isdigit (UCHARAT(scan))) {
	    scan++;
	}
	break;
    case NDIGI:
	while (*scan != '\0' && !isdigit (UCHARAT(scan))) {
	    scan++;
	}
	break;
    default:			/* Oh dear.  Called inappropriately. */
	rep_regerror("internal foulup");
	return 0;		/* Best compromise. */
	break;
    }

    count = scan - reginput;
    reginput = scan;

    return count;
}

/*
 * - regnext - dig the "next" pointer out of a node 
 */
static char    *
regnext(char *p)
{
    register int    offset;

    if (p == &regdummy)
	return (NULL);

    offset = NEXT(p);
    if (offset == 0)
	return (NULL);

    if (OP(p) == BACK)
	return (p - offset);
    else
	return (p + offset);
}

#ifdef DEBUG

char    *regprop();

/*
 * - regdump - dump a regexp onto stdout in vaguely comprehensible form
 */
void
regdump(rep_regexp *r)
{
    register char  *s;
    register char   op = EXACTLY;	/* Arbitrary non-END op. */
    register char  *next;


    s = r->program + 1;
    while (op != END) {		/* While that wasn't END last time... */
	op = OP(s);
	printf("\t%4d%s", s - r->program, regprop(s));    /* Where, what. */
	next = regnext(s);
	if (next == NULL)	/* Next ptr. */
	    printf("(0)");
	else
	    printf("(%d)", (s - r->program) + (next - s));
	s += 3;
	if (op == ANYOF || op == ANYBUT || op == EXACTLY) {
	    /* Literal string, where present. */
	    while (*s != '\0') {
		putchar(*s);
		s++;
	    }
	    s++;
	}
	putchar('\n');
    }

    /* Header fields of interest. */
    if (r->regstart != '\0')
	printf("start `%c' ", r->regstart);
    if (r->reganch)
	printf("anchored ");
    if (r->regmust != NULL)
	printf("must have \"%s\"", r->regmust);
    printf("\n");
}

/*
 * - regprop - printable representation of opcode
 */
char    *
regprop(char *op)
{
    register char  *p;
    static char	    buf[50];

    (void) strcpy(buf, ":");

    switch (OP(op)) {
    case BOL:
	p = "BOL";
	break;
    case EOL:
	p = "EOL";
	break;
    case ANY:
	p = "ANY";
	break;
    case ANYOF:
	p = "ANYOF";
	break;
    case ANYBUT:
	p = "ANYBUT";
	break;
    case BRANCH:
	p = "BRANCH";
	break;
    case EXACTLY:
	p = "EXACTLY";
	break;
    case NOTHING:
	p = "NOTHING";
	break;
    case BACK:
	p = "BACK";
	break;
    case END:
	p = "END";
	break;
    case OPEN + 1:
    case OPEN + 2:
    case OPEN + 3:
    case OPEN + 4:
    case OPEN + 5:
    case OPEN + 6:
    case OPEN + 7:
    case OPEN + 8:
    case OPEN + 9:
	sprintf(buf + strlen(buf), "OPEN%d", OP(op) - OPEN);
	p = NULL;
	break;
    case CLOSE + 1:
    case CLOSE + 2:
    case CLOSE + 3:
    case CLOSE + 4:
    case CLOSE + 5:
    case CLOSE + 6:
    case CLOSE + 7:
    case CLOSE + 8:
    case CLOSE + 9:
	sprintf(buf + strlen(buf), "CLOSE%d", OP(op) - CLOSE);
	p = NULL;
	break;
    case STAR:
	p = "STAR";
	break;
    case PLUS:
	p = "PLUS";
	break;
    case WORD:
	p = "WORD";
	break;
    case NWORD:
	p = "NWORD";
	break;
    case WSPC:
	p = "WSPC";
	break;
    case NWSPC:
	p = "NWSPC";
	break;
    case DIGI:
	p = "DIGI";
	break;
    case NDIGI:
	p = "NDIGI";
	break;
    case WEDGE:
	p = "WEDGE";
	break;
    case NWEDGE:
	p = "NWEDGE";
	break;
    case NGSTAR:
	p = "NGSTAR";
	break;
    case NGPLUS:
	p = "NGPLUS";
	break;
    default:
	rep_regerror("corrupted opcode");
	p = 0;
	break;
    }
    if (p != NULL)
	(void) strcat(buf, p);
    return (buf);
}
#endif

/*
 * The following is provided for those people who do not have strcspn() in
 * their C libraries.  They should get off their butts and do something about
 * it; at least one public-domain implementation of those (highly useful)
 * string routines has been published on Usenet.
 */
#ifndef HAVE_STRCSPN
/*
 * strcspn - find length of initial segment of s1 consisting entirely of
 * characters not from s2
 */
int
strcspn(char *s1, char *s2)
{
    register char  *scan1;
    register char  *scan2;
    register int    count;

    count = 0;
    for (scan1 = s1; *scan1 != '\0'; scan1++) {
	for (scan2 = s2; *scan2 != '\0';)       /* ++ moved down. */
	    if (*scan1 == *scan2++)
		return (count);
	count++;
    }
    return (count);
}
#endif
