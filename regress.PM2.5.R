regress.PM2.5 = function(region,subregion,daterange,filters,filters.df,series.id,model.id,model,submodel.id,model.vars,info,dir.merge,DOY.list=c(),local.hour.list=c(),filter.list=c(),site.delete.list=c(),row.list=c()) {
## Add checks for valid arguments
source('~esswein/science/region/daq/CA2013/Maps/data.atsites.R')
source('~esswein/science/region/daq/CA2013/StatModel/dat3.to.df.R')

ri = runinfo()

jd.1 = yyyymmdd2jd(daterange[1])
jd.2 = yyyymmdd2jd(daterange[2])
jd.all = seq(jd.1,jd.2,by=1)
cd.all = caldat(jd.all)
doylist = day_of_year(cd.all$year,cd.all$month,cd.all$day)

year = unique(sort(cd.all$year))

# Read data for all years, SC, and regions, apply misc filters, recast to table, and filter by DOY

  site.data = data.atsites(region,jd.1,jd.2)

  # Delete all sites with no PM2.5:
  site.ct = rep.int(NA,dim(site.data)[3])
  for (i in 1:dim(site.data)[3]) site.ct[i] = length(which(!is.na(site.data[,'PM2.5',i])))
  site.data = site.data[,,site.ct != 0]

  # Delete possible other sites:
####
## Need to check this doesn't change ordering
####
  site.data = site.data[,,setdiff(dimnames(site.data)[[3]],site.delete.list)]

  if (length(DOY.list) > 0) {
    w.keep = which(site.data[,'DOY',1] %in% DOY.list)
    site.data = site.data[w.keep,,]
  }

  if (length(local.hour.list) > 0) {
    lon.mean = mean(site.data[1,'lon',])
    local.hour = round(site.data[,'hour',1] + lon.mean / 15)
    w.keep = which(local.hour %in% local.hour.list)
    site.data = site.data[w.keep,,]
  }
  for (f in filters) site.data = f(site.data)
  for (f in filter.list) {
    f.file = str_c('filter.',f,'.R')
    if (file.access(f.file) != 0) stop(paste('Cant find',f.file))
    source(f.file)
    site.data = do.call(str_c('filter.',f),list(dat=site.data))
  }

site.df = dat3.to.df(site.data)

site.df = data.frame(site.df,jd.int=floor(site.df$jd))

for (f in filters.df) site.df = f(site.df)

site.data.jd = julday(site.data[,'month',1],site.data[,'day',1],site.data[,'year',1],site.data[,'hour',1],site.data[,'minute',1],0)
# Substitute variable names:
names.df = names(site.df)
for (v in names(model.vars)) {
  names.df = sub(model.vars[[v]],v,names.df)
}
names(site.df) = names.df
rownames(site.df) = paste('A',1:nrow(site.df),sep='')

if (length(row.list) > 0) site.df = site.df[rownames(site.df) %in% row.list,]

# In "lmer", avoid "Error: grouping factors must have > 1 sampled level"
# Try 'try':
if (length(unique(site.df$DOY)) >= 2) {
  fit = lmer(model,data=site.df,na.action=na.exclude )
  rows.used = as.integer(sub('A','',rownames(model.frame(fit))))

# Special extra regressions:
# site.df = site.df[rows.used,]
# fit = lm(PM2.5 ~ I(lon + 119.9),data=site.df)
# saveRDS(dat.regress,'~esswein/science/region/daq/CA2013/StatModel/SJV.PM2.5.Regress.results/Regress.PM2.5.AOT.A.LMhdist.rds')
#  fit = lmer(PM2.5 ~ (1 | DOY), data=site.df,na.action=na.exclude)
# saveRDS(dat.regress,'~esswein/science/region/daq/CA2013/StatModel/SJV.PM2.5.Regress.results/Regress.PM2.5.AOT.A.ranef.rds')
# fit = lmer(PM2.5 ~ I(lon + 119.9) + (1 | DOY),data=site.df,na.action=na.exclude)
# saveRDS(dat.regress,'~esswein/science/region/daq/CA2013/StatModel/SJV.PM2.5.Regress.results/Regress.PM2.5.AOT.A.hdist0+ranef.rds')
# fit =lmer(PM2.5 ~ AOT + (AOT|DOY),data=site.df,na.action=na.exclude)
# saveRDS(dat.regress,'~esswein/science/region/daq/CA2013/StatModel/SJV.PM2.5.Regress.results/Regress.PM2.5.AOT.A.AOT+ranef.rds')
  warn = capture.output(warnings())
  if (length(warn) == 0) {
    warn = ''
  } else {
    warn = warn[-1]
    warn = warn[warn != '']
  }
  # Add residuals:
  # NOTE: length(residuals(fit)) == nrow(site.df (data used for fit))
  # There is no mismatch like there is for prediction.
  site.df = data.frame(site.df,residuals=residuals(fit),stringsAsFactors=F)  
  attr(fit,'validDOY') = doylist
  obs = site.df$PM2.5
  rval =  cor(fitted(fit),obs,use='complete.obs')
  RMSerr = summary(fit)$sigma
  if (length(local.hour.list) > 0) hour.list.out = local.hour.list else hour.list.out = 'all hours'
  dat.regress = list(site.data=site.data,site.df=site.df,fit=fit,
    Rval=rval,RMSerr=RMSerr,filters=filters,year=year,
    daterange=daterange,region=region,subregion=subregion,
    model=model,model.vars=model.vars,local.hour.list=hour.list.out,info=info,runinfo=ri,warnings=warn)
  class(dat.regress) = 'PM2.5.regression'
  return(dat.regress)
# end 

} else {
  cat("Error: grouping factors must have > 1 sampled level\n")
    return("Error: grouping factors must have > 1 sampled level")
}

}

