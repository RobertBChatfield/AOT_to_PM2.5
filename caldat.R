caldat = function(julian) {
# $Id: //depot/idl/IDL_63_RELEASE/idldir/lib/caldat.pro#1 $
#
# Copyright (c) 1992-2006, Research Systems, Inc.  All rights reserved.
#	Unauthorized reproduction prohibited.
#

#+
# NAME:
#	CALDAT
#
# PURPOSE:
#	Return the calendar date and time given julian date.
#	This is the inverse of the function JULDAY.
# CATEGORY:
#	Misc.
#
# CALLING SEQUENCE:
#	CALDAT, Julian, Month, Day, Year, Hour, Minute, Second
#	See also: julday, the inverse of this function.
#
# INPUTS:
#	JULIAN contains the Julian Day Number (which begins at noon) of the
#	specified calendar date.  It should be a long integer.
# OUTPUTS:
#	(Trailing parameters may be omitted if not required.)
#	MONTH:	Number of the desired month (1 = January, ..., 12 = December).
#
#	DAY:	Number of day of the month.
#
#	YEAR:	Number of the desired year.
#
#	HOUR:	Hour of the day
#	Minute: Minute of the day
#	Second: Second (and fractions) of the day.
#
# COMMON BLOCKS:
#	None.
#
# SIDE EFFECTS:
#	None.
#
# RESTRICTIONS:
#	Accuracy using IEEE double precision numbers is approximately
#	1/10000th of a second.
#
# MODIFICATION HISTORY:
#	Translated from "Numerical Recipies in C", by William H. Press,
#	Brian P. Flannery, Saul A. Teukolsky, and William T. Vetterling.
#	Cambridge University Press, 1988 (second printing).
#
#	DMS, July 1992.
#	DMS, April 1996, Added HOUR, MINUTE and SECOND keyword
#	AB, 7 December 1997, Generalized to handle array input.
#	AB, 3 January 2000, Make seconds output as DOUBLE in array output.
#-
#
# pro CALDAT, julian, month, day, year, hour, minute, second
# 
# COMPILE_OPT idl2
# 
# 	ON_ERROR, 2		# Return to caller if errors
# 
# 	nParam = N_PARAMS()
# 	IF (nParam LT 1) THEN MESSAGE,'Incorrect number of arguments.'
# 
 	min_julian = -1095
 	max_julian = 1827933925
# 	minn = MIN(julian, MAX=maxx)
# 	IF (minn LT min_julian) OR (maxx GT max_julian) THEN MESSAGE, $
# 		'Value of Julian date is out of allowed range.'
# 
 	igreg = 2299161    #Beginning of Gregorian calendar
 	jullong = floor(julian + 0.5)   #Better be long
 	minjul = min(jullong)
# 
  	if (minjul >= igreg) { # all are Gregorian
  		jalpha = trunc(((jullong - 1867216) - 0.25) / 36524.25)
  		ja = jullong + 1 + jalpha - trunc(0.25 * jalpha)
  	} else {
  		ja = jullong
  		gregchange = which(jullong >= igreg)
  		if (length(gregchange) > 0) {
      		jalpha = trunc(((jullong[gregchange] - 1867216) - 0.25) / 36524.25)
      		ja[gregchange] = jullong[gregchange] + 1 + jalpha - trunc(0.25 * jalpha)
 		}
 	}
# 	jalpha = -1  # clear memory
# 
 	jb = ja + 1524
 	jc = trunc(6680 + ((jb-2439870)-122.1)/365.25)
 	jd = trunc(365 * jc + (0.25 * jc))
 	je = trunc((jb - jd) / 30.6001)
 
 	day = jb - jd - trunc(30.6001 * je)
 	month = je - 1
 	month = ((month - 1) %% 12) + 1
 	year = jc - 4715
	year = year - (month > 2)
 	year = year - (year <= 0)
# 
# # see if we need to do hours, minutes, seconds
# 	IF (nParam GT 4) THEN BEGIN
 		fraction = julian + 0.5 - jullong
 		hour = floor(fraction * 24)
 		fraction = fraction - hour/24
 		minute = floor(fraction*1440)
 		second = (fraction - minute/1440) * 86400
# 	ENDIF
# 
# # if julian is an array, reform all output to correct dimensions
# 	IF (SIZE(julian,/N_DIMENSION) GT 0) THEN BEGIN
# 		dimensions = SIZE(julian,/DIMENSION)
# 		month = REFORM(month,dimensions)
# 		day = REFORM(day,dimensions)
# 		year = REFORM(year,dimensions)
# 		IF (nParam GT 4) THEN BEGIN
# 			hour = REFORM(hour,dimensions)
# 			minute = REFORM(minute,dimensions)
# 			second = REFORM(second,dimensions)
# 		ENDIF
# 	ENDIF

    cd = c()
    cd$year = year
    cd$month = month
    cd$day = day
    cd$hour = hour
    cd$minute = minute
    cd$second = second
# 
# END

  cd

}
