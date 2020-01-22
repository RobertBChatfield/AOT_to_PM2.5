runinfo = function() {
# Return information about a source'd script
#	150831	R. F. Esswein	Initial version
# $Id: runinfo.R,v 1.1 2015/09/16 20:31:49 esswein Exp $
  source('~esswein/lib/R/fullpath.R')

  v = R.Version()$version.string
  wrds = strsplit(v,'  *')
  v = wrds[[1]][3]
  u = Sys.getenv('LOGNAME')

  t = date()

  c = sys.calls()[[1]]
  if (grepl('^source',as.character(c[1]))) {
    p = as.character(c[2])
    p = fullpath(p)
  } else {
    p = 'MAIN'
  }


  h = Sys.getenv('HOST')
  
  list(Rver=v,user=u,time=t,prog=p,host=h)


}
