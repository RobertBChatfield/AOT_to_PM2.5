
fullpath = function(fn,dir) {
#+
# PURPOSE:
#  Return the full path name of a UNIX file.
# VALUE RETURNED:
#  Full path name.
# INPUTS
#  fn_in = a file name
#  dir_in = an optional directory name.
# RESTRICTIONS:
#   Does not check that the file or directory names are valid.
#-
#	R. F. Esswein	980723	Translated from fullpath.nawk
#	R. Esswein	980731	Rewrite 2 loops to be clearer, if
#				 possibly less efficient.
#	R. Esswein	070321	Replace REGEX_ routines by RE_ counterpart.
# $Id: fullpath.pro,v 1.3 2007/03/29 17:00:45 usrlocal Exp $

  if (nargs() == 0) {
    warning('Incorrect number of arguments')
    return('')
  }

  fn = path.expand(fn)
  wd = getwd()

  if (nargs() == 1) {
    # If no directory was specified, use the current directory:
    dir = wd
  } else if (substr(dir,1,1) == '.') {
    # If directory starts with ".", prefix the current directory:
    dir = paste(wd,'/',dir,sep='')
  }

  # If file name starts with "/", it is a full pathname;
  # otherwise, prefix the directory:
  if (substr(fn,1,1) != '/') {
    fn = paste(dir,'/',fn,sep='')
  }

  # Eliminate all "dir/../" instances:
  while (length(grep( "[^/][^/]*/\\.\\./", fn )) > 0) {
    fn = sub( "[^/][^/]*/\\.\\./" , "", fn )
  }

  # At this point, if too many instances of "../" had been
  # in the file name, there would be a "/.." at the start.
  # If so, eliminate it:
  fn = sub( "/\\.\\." , "", fn )

  # Eliminate all "\./" instances:
  fn = gsub( "\\./" , "", fn )

  # Replace "//" by "/"
  while (length(grep( "//", fn )) > 0) { 
    fn = sub( "//" , "/", fn )
  }

  fn

}
