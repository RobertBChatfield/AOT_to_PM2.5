
# Set up data filters:
# Since there are so many possibilities there will be no attempt to parametrize all
# filtering. It is the responsibility of the user to construct any extra filtering.
# Each filter will be passed a 3d matrix
# 1st dim is time; 2nd is variable; 3rd is site.
# A condensed example:
# dimnames(site.data) =
#   $MAIAC.time
#     NULL
#   $data.names
#    [1] "year"                         "month"                       
#    [3] "day"                          "DOY"
#   $site
#    [1] "EPA_Stockton"       "EPA_Manteca"        "EPA_Modesto"
# The value returned should have the same structure.

# Remove extreme PM2.5 values:
filter.PM2.5 = function(dat) {
  PM2.5.min = 10
  PM2.5.max = 55
  dat.2 = dat[,'PM2.5',]
  dat.2[dat.2<PM2.5.min] = NA
  dat.2[dat.2>PM2.5.max] = NA
  dat[,'PM2.5',] = dat.2
  dat
}
# Delete some sites
# What was the criterion for this?
filter.sites = function(dat) {
  Omit.sites = c(
   "EPA_Roseville", "EPA_Folsom", "EPA_Sacramento", "EPA_Sacramento-DelPasoManor", "EPA_Sacramento-TStreet",
   "EPA_Davis", "EPA_San.Andreas",
   "DRAGON_Madera",  "DRAGON_Parlier",  "DRAGON_Visalia", "EPA_Coarsegold", "EPA_Corcoran", "EPA_Santa.Rosa.Rancheria", "DRAGON_Shafter")
  keep.sites = setdiff(dimnames(dat)[[3]],Omit.sites)
  dat = dat[,,keep.sites]
  dat
}
filter.site.sample = function(dat) {
  w.samp = sample(1:(dim(dat)[3]),.1*(dim(dat)[3]))
  w.samp = sample(1:(dim(dat)[3]),2)
  dat[,,w.samp,drop=F]
}
# Restrict to afternoon hours
filter.localhour = function(dat) {
  lon.mean = mean(dat[1,'lon',])
  local.hour = dat[,'hour',1] + lon.mean / 15
  w.aft = which(local.hour >= 12 & local.hour <= 18)
  dat[w.aft,,]
}
# Remove nonpositive PBL heights
# Remove zero also since using AOT/ML
filter.RAP_PBL_hgt = function(dat) {
  dat.2 = dat[,'PBL_hgt.R',]
  dat.2[dat.2<=0] = NA
  dat[,'PBL_hgt.R',] = dat.2
  dat
}
# Smaller number of days
# Use only days points by another regression
# Works on the data frame instead of the 3d array
filter.df.DOY = function(df) {
  d.r = readRDS('~esswein/science/region/daq/CA2013/StatModel/SJV.PM2.5.Regress.results/Regress.PM2.5.CWV.Bc.rds')
  rows.used = as.integer(rownames(model.frame(d.r$fit)))
  index.used = which(as.integer(rownames(df)) %in% rows.used)
  df = df[index.used,]
  df
}
