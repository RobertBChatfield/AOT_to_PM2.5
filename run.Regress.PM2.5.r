#
library(lme4)
library(stringr)

source('~esswein/lib/R/day_of_year.R')
source('~esswein/lib/R/runinfo.R')
source('~esswein/lib/R/strssep.R')
source('~esswein/lib/R/julday.R')
source('~esswein/lib/R/caldat.R')
source('~esswein/lib/R/yyyymmdd2jd.R')
source('~esswein/lib/R/formula.2.character.R')
source('~esswein/science/region/daq/CA2013/StatModel/PM2.5.rinfo.R')
source('~esswein/science/region/daq/CA2013/StatModel/regress.PM2.5.R')

# If do.save, will try to write to an esswein directory.
# If not available, save by hand:
#  saveRDS(dat.regress,file= file to save to )
if (!exists('do.save')) do.save = F

# Set up the regression parameters:

series.id = rgr[['series.id']]
model.id = rgr[['model.id']]
model = rgr[['model']]
model.vars = rgr[['model.vars']]
filter.list = rgr[['filter.list']]

# Series.CWV PM2.5 ~ AOT/CWV
#  A PM2.5 ~ I(AOT/ColWV) + (1 | DOY )
# Series.ML  PM2.5 ~ AOT/ML

# When giving model, use only "top level" names, don't use secondary expressions,
# such as "PM2.5 ~ Ratio + (1 | DOY )" where "Ratio" is set to AOT/ColWV.
# This is so scripts further down the line can figure out what was done.

# Specify the actual data to be used for the regression:
# For example, AOT might be either MAIAC 'AOT_550' or Aeronet 'AOT_500.D'
# The list index in model.vars is the generic name; the element is the particular name.
# See ~esswein/science/region/daq/CA2013/Maps/site.data.doc
# for information on the "extracted" data array
# submodel corresponds to the choices made by further edits below here.
# "info" = Any additional information the user may want to use to characterize the run:

# File where regression results are saved.
# Save file name for regression result:
# It is the responsibility of the user to make a file name. However, it is impossibile
# to fully characterize the model in a realisticly short file name. The data saved in the
# file, however, does characterize the run and can be examined to get the details.

if (!exists('DOY.list')) DOY.list = c()
if (!exists('row.list')) row.list = c()
if (!exists('local.hour.list')) local.hour.list = c()
if (!exists('filter.list')) filter.list = c()
if (!exists('site.delete.list')) site.delete.list = c()
if ('model.title' %in% names(rgr)) model.title = rgr$model.title else model.title = formula.2.character(rgr$model)

# Run the regression:

dat.regress = regress.PM2.5(region, subregion, daterange, filters, filters.df, series.id, model.id, model, submodel.id, model.vars, info, data.dir, DOY.list=DOY.list, local.hour.list=local.hour.list, filter.list=filter.list, site.delete.list=site.delete.list, row.list=row.list)
PM2.5.rinfo(dat.regress)
dat.regress[['model.title']] = model.title

  if (do.save) {
    if (file.access(file.regress) == 0) cat('overwrite file\n')
    saveRDS(dat.regress,file=file.regress)
    cat(file.regress,'\n')
    text = capture.output(PM2.5.rinfo(dat.regress))
    text = c(text,'',capture.output(summary(dat.regress$fit)))
    file.txt = sub('\\.rds$','.txt',file.regress)
    writeLines(text,file.txt)
  }
