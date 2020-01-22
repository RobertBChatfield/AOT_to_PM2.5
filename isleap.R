# $Header: /var/tmp/portage/local-tools/acdb-idl-1.0/work/acdb-idl-1.0/userlib/isleap.pro,v 1.3 2002/04/15 17:33:41 eroc Exp $

isleap = function(year) {

#+
# NAME:
#   isleap
# PURPOSE:
#   Determines if a year is a leap year.
# CATEGORY:
#   date
# CALLING SEQUENCE:
#   x = isleap(year)
# FUNCTION RETURN VALUE:
#   byte: array, sclar
#   Returns 0 for each element of 'year' which is not a leap year. Returns 1 
#   for each element of 'year' which is a leap year. 
# INPUT PARAMETERS:
#   year = (numerical array or scalar) the year
# OPTIONAL INPUT PARAMETERS:
# INPUT/OUTPUT PARAMETERS:
# OPTIONAL INPUT/OUTPUT PARAMETERS:
# OUTPUT PARAMETERS:
# OPTIONAL OUTPUT PARAMETERS:
# INPUT KEYWORDS:
#   cal = (number) numerical calendar code.Takes precedence over the 
#         '!calendar' system variable. The codes are:
#             0: GREGORIAN
#           100: MODEL365
#         (string) string calendar code. Takes precedence over the 
#         '!calendar' system variable
# INPUT/OUTPUT KEYWORDS:
# OUTPUT KEYWORDS:
#   error = (integer array or scalar) returns 0 for those elements for which the
#           function result is meaningful, otherwise returns 1.
# COMMON BLOCKS:
# REQUIRED ROUTINES:
#   which_calendar
# @ FILES:
# RESTRICTIONS:
# SIDE EFFECTS:
# DIAGNOSTIC INFORMATION:
# PROCEDURE:
# EXAMPLES:
# REFERENCE:
# FURTHER INFORMATION:
# RELATED FUNCTIONS AND PROCEDURES:
# MODIFICATION HISTORY:
#   1990      :lrlait:idlv2
#   2002-04-15:eroc:returns results based on the calendar; added error return
#-

# *****determine the calendar

#   *****Gregorian

#     *****note: years of 4000 have not been officially adopted
  leap = (((year %% 4) == 0) & (((year %% 100) != 0) |
         ((year %% 400) == 0)) & ((year %% 4000) != 0))

#     *****years before the calendar
  l = which(year < 1583 )
  if  (length(l) > 0) {
    leap[l] = F
    warning('Some years < 1583 !')
  }

  leap

}
