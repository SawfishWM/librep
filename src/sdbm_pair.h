extern int sdbm_fitpair (char *, int);
extern void  sdbm_putpair (char *, datum, datum);
extern datum	sdbm_getpair (char *, datum);
extern int  sdbm_delpair (char *, datum);
extern int  sdbm_chkpage (char *);
extern datum sdbm_getnkey (char *, int);
extern void sdbm_splpage (char *, char *, long);
#ifdef SEEDUPS
extern int sdbm_duppair (char *, datum);
#endif
