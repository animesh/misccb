/* Concurrent read version */

/* $Name: fasta34t11d4 $ - $Id: htime.c,v 1.2 2001/01/16 20:54:25 wrp Exp $ */

#include <stdio.h>
#include <time.h>

#ifdef UNIX
#include <sys/types.h>
#include <sys/time.h>
#ifdef TIMES
#include <sys/times.h>
#else
#undef TIMES
#endif
#endif

#ifndef HZ
#define HZ 100
#endif

time_t s_time ()			/* returns time in milliseconds */
{
#ifndef TIMES
	time_t time(), tt;
	return time(&tt)*1000;
#else
  struct tms tt;
  times(&tt);
#ifdef CLK_TCK
  return tt.tms_utime*1000/CLK_TCK;
#else
  return tt.tms_utime*1000/HZ;
#endif
#endif
}

void ptime (FILE *fp, time_t time)		/* prints the time */
{
  fprintf (fp, "%6.3f",(double)(time)/1000.0);
}

