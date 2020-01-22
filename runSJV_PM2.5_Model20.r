library(stringr)
source("~esswein/science/region/daq/CA2013/StatModel/PM2.5.Analysis.filters.r")
source("~esswein/science/region/daq/CA2013/StatModel/maiac.OK.R")
source('~esswein/lib/R/strssep.R')
source('~esswein/lib/R/getbits.R')

# Get rid of old warnings:
## doesn't work: suppressWarnings(warning(''))
assign("last.warning", NULL, envir = baseenv())
# Grid resolution in degrees:
res.lat = .01
# Margin in degrees for "large" grid.
# The large grid is used for calculating streamlines.
ext.margin = 10

region = 'SJV'
subregion = ''
regdotsub = region
if (subregion != '') regdotsub = str_c(region,'.',subregion)

# Define top level; someone else can change it.
data.dir = '~esswein/science/region/daq/CA2013/Data.extracted/'

# Need both read and write permssions for this directory:
result.dir = str_c('~esswein/science/region/daq/CA2013/StatModel/NEW.',region,'.PM2.5.Regress.results')
# result.dir = '~esswein/science/region/daq/CA2013/testdir/'
rdperm = file.access(result.dir,mode=2+4)
if (rdperm != 0) stop(str_c('Permissions not set to use ',result.dir))


# Specify the actual data to be used for the regression:
# For example, AOT might be either MAIAC 'AOT_550' or Aeronet 'AOT_500.D'
# The list index in model.vars is the generic name; the element is the particular name.
# See ~esswein/science/region/daq/CA2013/Maps/site.data.names.doc
# for information on the "extracted" data array
# submodel corresponds to the choices made by further edits below here.
# "info" = Any additional information the user may want to use to characterize the run:
info = c()
model20.def = list(series.id = 'CWV' , model = PM2.5 ~ I(AOT/CWV)  + I(lon - mean(lon)) + (I(AOT/CWV)+1 | DOY ) , model.id = 'CM.hdist' , model.vars = list(AOT='AOT550.M',CWV='CWV.M') )


rgr = model20.def

info = c('No PM2.5 filtering')
daterange = c(20121119,20130218)
filters = c()
filters.df = c()
# filter.list = c('SJV.geography') # Don't need; don't have "hdist" anymore.
# filter.list = c('CWV')
# For models using fewer variables, ordinarily there will be more
# complete cases. But want to not have more; want to use the same
# points as for the more restrictive model.
# Note that we want to make sure not to go in the other direction of using days
# from a less restricted data frame, for analyzing a more restrictive
# data frame.
# 
# To compare models, want to use the same data for each.
# Since different variables are regressedin different models,
# the number of complete cases will differ. Thereofore, to do a set
# of models, need the intersection of the data that would have
# been used for each.
# The present case we are using the days from the following.
# We are not set up to do a set of models, only one at at time,
# So the common data set must be chosen "manually".
dat.regress.longformula = readRDS('/Users/esswein/science/region/daq/CA2013/StatModel/SJV.PM2.5.Regress.results/Regress.PM2.5.AOT.B.hdist.Xc.rds')
row.list = rownames(model.frame(dat.regress.longformula$fit))
DOY.list = unique(sort(dat.regress.longformula$site.df$DOY))
site.delete.list = strssep('EPA_Davis EPA_Coarsegold EPA_Roseville EPA_Sacramento-TStreet EPA_San.Andreas')
local.hour.list = 12:18
  do.save = F ; do.pdf = F ; do.txt = F
file.regress = str_c(result.dir,'/',region,'.',rgr$series.id,'.')
if (subregion != '') file.regress = str_c(file.regress,subregion,'.')
if (rgr$model.id != '') file.regress = str_c(file.regress,rgr$model.id,'.')
file.regress = str_c(file.regress,'Regress.rds')

  source("~esswein/science/region/daq/CA2013/StatModel/run.Regress.PM2.5.r")
  source("~esswein/science/region/daq/CA2013/StatModel/Regress.PM2.5.Plot.r")
  source("~esswein/science/region/daq/CA2013/StatModel/ConnnectHiCorResiduals.r")
  source("~esswein/science/region/daq/CA2013/StatModel/xvalid.loo.r")
