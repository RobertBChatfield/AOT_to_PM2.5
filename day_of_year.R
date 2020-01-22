day_of_year = function(year,month,monthday) {
source('~esswein/lib/R/isleap.R')
#+
# PURPOSE:
#  Return number of day in year
# INPUT:
#  year, This the actual year, Not the day of the century.
#  month, month number; Jan = 1
#  monthday, day of month; > 0
# OUTPUT:
#  Number of day in year; ie. Jan 1 = day 1 and Dec 31 = 365 if not a leap year.
# MODIFICATION HISTORY:
#	R. Esswein	960410	Original code.
#	R. Esswein	111117	Vectorize and eliminate a loop.
#	R. Esswein	150601	"unvectorize" because R array indexing
#				 doesn't work the same as IDL.
#-
# $Id: day_of_year.R,v 1.1 2015/06/01 21:31:45 esswein Exp $

  days_prev_to_month = cbind(  
    c( 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 ), 
    c( 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 )
  )

  ntime = length(year)
  doy = rep.int(NA,ntime)
  leap = as.integer(isleap(year))
  for (itime in 1:ntime) doy[itime] = 
    days_prev_to_month[month[itime],leap[itime] + 1] + monthday[itime]

  doy

}
