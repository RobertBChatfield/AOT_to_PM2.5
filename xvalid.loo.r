library(lme4)
library(fields)
library(stringr)
source('~esswein/lib/R/runinfo.R')
source('~esswein/lib/R/strssep.R')

if (!exists('do.pdf')) do.pdf = F
if (!exists('do.txt')) do.txt = F

ri = runinfo()

file.pdf = sub('Regress.rds$','LOOxvalid.pdf',file.regress)
file.txt = sub('pdf$','txt',file.pdf)
file.rds = sub('pdf$','rds',file.pdf)

if (do.txt) {
  sink(file.txt)
  cat('cross validation for',file.regress,'\n\n')
  cat('written_by',ri$prog,'\n\n')
} 
base.dat = readRDS(file.regress)

site.list = dimnames(base.dat$site.data)[[3]]
n.site = length(site.list)
DOYlist = sort(unique(base.dat$site.df$DOY))
n.DOY = length(DOYlist)

all.fit = list()
DOYranef.site = matrix(NA,n.site,n.DOY)
DOYsumcoef = rep.int(0,n.DOY)
DOYcoefcount = rep.int(0,n.DOY)
DOYmeancoef = rep.int(NA,n.DOY)
fixint.site = rep.int(NA,n.site)
fixcoef.site = rep.int(NA,n.site)
mse.loo = rep.int(NA,n.site)
mse.pred = rep.int(NA,n.site)

dat = base.dat$site.df
row.names(dat) = str_c('A',1:nrow(dat))

if (do.pdf) pdf(file.pdf,title=paste('written_by',ri$prog))

ifit = 0
for (i.site in 1:n.site) {
  site = site.list[i.site]
  cat(site,'\n')
  w.site = which(dat$name == site)
  if (length(w.site) == 0) {
    cat(site,'not present\n')
    next
  }
  if (length(which(!is.na(dat$PM2.5[w.site]))) == 0) {
    cat('no observed PM2.5 for',site,'\n')
    next
  }
  if (do.txt) cat('site',site,'\n\n')
  dat.loo = dat[-w.site,]
  fit.loo = lmer(base.dat$model,data=dat.loo)
  if (do.txt) print(summary(fit.loo))
  all.fit[[str_c('omit_',site)]] = fit.loo
  ranef.int = ranef(fit.loo)$DOY[,1]
  ranef.DOY = as.integer(as.character(rownames(ranef(fit.loo)$DOY)))
  fixef.int = fixef(fit.loo)[1]
  fixef.coef = fixef(fit.loo)[2]
  mse.loo[i.site] = mean(residuals(fit.loo)^2,na.rm=T)
  if (do.txt) cat('\nMSE for fit',mse.loo[i.site],'\n')
  ## This causes an error:
# fit.loo = lmer(base.dat$model,data=dat.loo,na.action=na.exclude)
  # Indices of DOY for this fit, in the entire DOY list:
  i.DOY = match(ranef.DOY,DOYlist)
  DOYsumcoef[i.DOY] = DOYsumcoef[i.DOY] + ranef.int
  DOYcoefcount[i.DOY] = DOYcoefcount[i.DOY] + 1
  DOYranef.site[i.site,i.DOY] = ranef.int
  fixint.site[i.site] = fixef.int
  fixcoef.site[i.site] = fixef.coef

  predicted = predict(fit.loo,dat[w.site,],allow.new.levels=T)
  mse.pred[i.site] = mean((predicted-dat$PM2.5[w.site])^2,na.rm=T)
  if (do.txt) cat('\nMSE for predicted',mse.pred[i.site],'\n')

  # Attempt to calculate predicted without "predict()":
  if (model == 'Ac') pred.test = ranef.int[match(dat$DOY[w.site],ranef.DOY)] + fixef.int + fixef.coef * dat$AOT[w.site]/dat$CWV[w.site]
  if (model == 'Bc') pred.test = ranef.int[match(dat$DOY[w.site],ranef.DOY)] * dat$AOT[w.site]/dat$CWV[w.site] + fixef.int + fixef.coef * dat$AOT[w.site]/dat$CWV[w.site]
# cat('predictions diffs',mean(abs(predicted-pred.test),na.rm=T),'\n')
# cat('predictions mean',mean(abs(predicted),na.rm=T),'\n')

  title = c(str_c(site,' LOO xvalid'),paste(base.dat$daterange,collapse=' '),as.character(deparse(formula(base.dat$model))))
  plot(dat[w.site,'PM2.5'],predicted,xlim=c(0,80),ylim=c(0,80),main=title,xlab='Obs PM2.5',ylab='Pred PM2.5')
  ifit = ifit + 1
  cat('\n\n')
}

  fit = lmer(base.dat$model,data=dat)
  all.fit[['all_sites']] = fit
  plot(fitted(fit)+residuals(fit),fitted(fit),xlab='Obs',ylab='Fitted',main='Fit for all sites')
  cat('\n\nFit with all data\n\n')
  if (do.txt) {
    print(summary(fit))
    cat('\nMSE for fit',mean(residuals(fit)^2,na.rm=T),'\n')
  }

  DOYmeancoef[DOYcoefcount > 0] = DOYsumcoef[DOYcoefcount > 0] / DOYcoefcount[DOYcoefcount > 0]
  fixintmean = mean(fixint.site,na.rm=T)
  fixcoefmean = mean(fixcoef.site,na.rm=T)
###
## w.cc = which(complete.cases(dat[,names(model.frame(base.dat$fit))]))
####
  w.cc = which(complete.cases(dat[,strssep('PM2.5 AOT CWV DOY')]))
  if (model.id == 'A') {
    pred.all = DOYmeancoef[match(dat$DOY[w.cc],DOYlist)] + fixintmean + fixcoefmean * dat$AOT[w.cc]/dat$CWV[w.cc]
    have.predict = T
  } else if (model.id == 'B') {
    pred.all = DOYmeancoef[match(dat$DOY[w.cc],DOYlist)] *dat$AOT[w.cc]/dat$CWV[w.cc] + fixintmean + fixcoefmean * dat$AOT[w.cc]/dat$CWV[w.cc]
    have.predict = T
  } else {
    cat('Prediction formula not available for this model\n')
    have.predict = F
  }

  if (have.predict) {
    if (do.txt) cat('\nMSE for predicted using averaged coefficients',mean((pred.all-dat$PM2.5[w.cc])^2,na.rm=T),'\n')

    plot(pred.all,fitted(fit),xlab='predicted using mean coefficients',ylab='fitted using all sites',main='compare PM2.5 predictions') 
    plot(fitted(fit)-pred.all,xlab='',ylab='fitted - predicted',main='compare PM2.5 predictions') 
  }

  plot(fixint.site,main='fixed intercept by site',xlab='site')
  plot(fixcoef.site,main='fixed coefficient by site',xlab='site')
  image.plot(DOYranef.site,xlab='site',ylab='DOY',main='DOY ranef')

if (do.txt) {
  sink()
  cat(file.txt,'\n')
  saveRDS(all.fit,file.rds)
}

if (do.pdf) {
  dev.off()
  cat(file.pdf,'\n')
}
