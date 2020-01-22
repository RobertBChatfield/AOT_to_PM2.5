library(stringr)
source('~esswein/lib/R/julday.R')
source('~esswein/lib/R/caldat.R')
data.atsites = function(region,jd1,jd2) {
# Get the data at the ground sites, at the maiac times
# regions = SECC SFBA SJV
# Note that all sites in the region are returned,
# even if they contain no data for the time range.

  cd1 = caldat(jd1)
  cd2 = caldat(jd2)


  years = cd1$year:cd2$year

  first = T
  dat3 = NULL
  for (year in years) {
    mergefile = str_c('~esswein/science/region/daq/CA2013/Data.extracted/','ALL.atsite.',region,'.',year,'.rds')
    if (file.access(mergefile) != 0) {
      cat(mergefile,'not available\n')
      next
    }
    d = readRDS(mergefile)
    if (first) {
      dat3 = d
      first = F
    } else {
      dat3 = abind::abind(dat3,d,along=1)
    }

  }
  if (is.null(dat3)) return(NULL)
  jd.dat3 = julday(dat3[,'month',1],dat3[,'day',1],dat3[,'year',1],dat3[,'hour',1],dat3[,'minute',1],0)
  w.keep = which(jd.dat3 >= jd1 & jd.dat3 <= jd2)
  dat3 = dat3[w.keep,,]


  dat3

  }
