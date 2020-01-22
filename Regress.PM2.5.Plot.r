#
library(fields)
library(gplots)
library(lme4)
library(abind)
library(stringr)

source("~esswein/lib/R/pt.color.r")
source("~esswein/lib/R/rle_ext.r")
source("~esswein/lib/R/halt.r")
source('~esswein/lib/R/day_of_year.R')
source('~esswein/lib/R/runinfo.R')
source('~esswein/lib/R/strssep.R')
source('~esswein/lib/R/julday.R')
source('~esswein/lib/R/midpoint.R')
source('~esswein/lib/R/earthdist.R')
source('~esswein/lib/R/formula.2.character.R')
source('~esswein/lib/R/position.labels.R')
add.alpha <- function (hex.color.list, alpha) sprintf("%s%02X", hex.color.list, floor(alpha * 256))

if (!exists('do.pdf'))  do.pdf = F

ri = runinfo()
hdr = paste('written_by',ri$prog)

file.pdf = sub('Regress.rds','PlotRegress.pdf',file.regress)

timefrac.color = tim.colors(9)
timefrac.color = timefrac.color[-6]
timefrac.color = add.alpha(timefrac.color,.5)
n.timefrac.color = length(timefrac.color)

if (file.access(file.regress) == 0) {
  dat.regress = readRDS(file.regress)
}
if (!exists('dat.regress')) stop('dat.regress not available')
site.df = dat.regress$site.df
site.data = dat.regress$site.data
doylist = unique(sort(site.df$DOY))
model.s = as.character(deparse(formula(dat.regress$fit)))
model.title = dat.regress$model.title

# Sites that have some PM2.5 for this region and time period:
sitelist = c()
lonlist = c()
latlist = c()
for (i in 1:dim(site.data)[3]) {
  if (length(which(!is.na(site.data[,'PM2.5',i]))) > 0) {
    sitelist = c(sitelist,dimnames(site.data)[[3]][i])
    lonlist = c(lonlist,site.data[1,'lon',i])
    latlist = c(latlist,site.data[1,'lat',i])
  }
}
lon.ctr = mean(lonlist)
lat.ctr = mean(latlist)
dist.ctr = earthdist(lat.ctr,lon.ctr,latlist,lonlist)
hdist.ctr = earthdist(lat.ctr,lon.ctr,lat.ctr,lonlist)
hdist.ctr[lonlist < lon.ctr] = -hdist.ctr[lonlist < lon.ctr]
vdist.ctr = earthdist(lat.ctr,lon.ctr,latlist,lon.ctr)
vdist.ctr[latlist < lat.ctr] = -vdist.ctr[latlist < lat.ctr]
angle.ctr = atan2(vdist.ctr,hdist.ctr)*180/pi

sitechroma = 90
sitehue = angle.ctr
sitelum = 90 - 70*(dist.ctr-min(dist.ctr))/(max(dist.ctr)-min(dist.ctr))
sitecolor = hcl(sitehue,sitechroma,sitelum)
ptcolor = sitecolor[match(site.df$name,sitelist)]

if (do.pdf) pdf(file=file.pdf,title=hdr)

# Coverage plots:
var = 'PM2.5'
# Same range as for predicted maps:
# There are a few values outside this range.
lim.PM2.5 = c(0,75)
n.time = dim(site.data)[1]
n.site = dim(site.data)[3]

daterange.dt = strptime(dat.regress$daterange,format='%Y%m%d',tz='GMT')
date.tick = pretty(daterange.dt)
date.tick = gsub('[A-Z]','',date.tick)

image.plot(site.data[,var,n.site:1],axes=F,main=str_c(dat.regress$region,' ',var),zlim=lim.PM2.5)

lim = par('usr')
xtick = midpoint(seq(lim[1],lim[2],len=n.time+1))
ytick = midpoint(seq(lim[3],lim[4],len=n.site+1))
####
## Note: for this set of runs only
## TBD: programmaticaly choose dates
####
doylab = c(324,345,1,25,48)
wdoytick = rle_ext(site.data[,'DOY',1])$start
wdoylab = rep.int(NA,length(doylab))
for (i in 1:length(doylab)) wdoylab[i] = min(which(site.data[,'DOY',1] == doylab[i]))
doylab = paste(month.abb[site.data[wdoylab,'month',1]],site.data[wdoylab,'day',1])

sitelab = dimnames(site.data)[[3]][n.site:1]
sitebase = sub('.*_','',sitelab)
siterle = rle_ext(sitebase)
# Delete prefix from sites with unduplicated base names;
wsingle = siterle$start[siterle$lengths == 1]
sitelab[wsingle] = sub('^..._','',sitelab[wsingle])
sitelab[grepl('EPA',sitelab)] = paste(sitebase[grepl('EPA',sitelab)],'E')
sitelab[grepl('DAQ',sitelab)] = paste(sitebase[grepl('DAQ',sitelab)],'D')

# Draw a point off the plot:
# This is necessary to get "text" to work.
points(-10,-10)

toffset = -.02
nday = dim(site.data)[1]
xtitle = paste('Day of Year',site.data[1,'day',1],month.name[site.data[1,'month',1]],site.data[1,'year',1],'to',site.data[nday,'day',1],month.name[site.data[nday,'month',1]],site.data[nday,'year',1])
text(xtick[wdoylab],lim[3]+toffset,lab=doylab,srt=90,xpd=NA,adj=1,cex=1)
# text(mean(xtick),lim[3]+6*toffset,lab=xtitle,xpd=NA)
text(lim[1]+toffset,ytick,lab=sitelab,xpd=NA,adj=1,cex=.8)
axis(1,at=xtick[wdoytick],label=F,tcl=-.25)
# halt()

var = 'Residuals'
resid.by.site = matrix(NA,n.time,n.site)
sites = unique(site.df$name)
dates = unique(sort(site.df$yyyymmddhhmm))
colnames(resid.by.site) = sites
isite.used = match(site.df$name,sites)
itime.used = match(site.df$yyyymmddhhmm,dates)
resid.by.site[cbind(itime.used,isite.used)] = site.df$residuals
image.plot(resid.by.site[,n.site:1],axes=F,main=str_c(dat.regress$region,' ',var))

lim = par('usr')
xtick = midpoint(seq(lim[1],lim[2],len=n.time+1))
ytick = midpoint(seq(lim[3],lim[4],len=n.site+1))
points(-10,-10)
toffset = -.02
text(xtick,lim[3]+toffset,lab=site.data[,'DOY',1],srt=90,xpd=NA,adj=1,cex=.6)
text(lim[1]+toffset,ytick,lab=dimnames(site.data)[[3]][n.site:1],xpd=NA,adj=1,cex=.6)

plot(lonlist,latlist,col=sitecolor,pch=19,cex=2,xlim=range(pretty(site.df$lon)),ylim=range(pretty(site.df$lat)),main='Site Colors',xlab='',ylab='',bty='n')
  position.labels(lonlist,latlist,sitelist,xlab='',ylab='',map=F,cex=2,cex.lab=.8)

# Scatterplot fitted vs. obs:
title = c(dat.regress$region,model.title,str_c('Rval',signif(dat.regress$Rval,3),'RMSerr',signif(dat.regress$RMSerr,3),sep=' '))
plot(fitted(dat.regress$fit),fitted(dat.regress$fit)+residuals(dat.regress$fit),xlim=lim.PM2.5,ylim=lim.PM2.5,xlab='Fitted',ylab='Obs',main=title,col=ptcolor,pch=19)

# Time series fitted/obs vs day
plot(site.df$jd,fitted(dat.regress$fit)/(fitted(dat.regress$fit)+residuals(dat.regress$fit)),xlab='DOY',ylab='Fitted/Obs',main=title,col=ptcolor,pch=19)

# Obs vs AOT
if ('AOT' %in% names(dat.regress$model.vars)) {
  rval =  cor(site.df$AOT,fitted(dat.regress$fit)+residuals(dat.regress$fit),use='complete.obs')
  RMSerr = sigma(lm(PM2.5~AOT,data=site.df))
  title = c(dat.regress$region,str_c('Rval',signif(rval,3),'RMSerr',signif(RMSerr,3),sep=' '))
  plot(site.df$AOT,site.df$PM2.5,xlab='AOT',ylab='Obs',main=title,col=ptcolor,pch=19,ylim=c(0,70))
}

# Obs vs AOT/CWV
if ('AOT' %in% names(dat.regress$model.vars) & 'CWV' %in% names(dat.regress$model.vars)) {
  rval =  cor(site.df$AOT/site.df$CWV,fitted(dat.regress$fit)+residuals(dat.regress$fit),use='complete.obs')
  RMSerr = sigma(lm(PM2.5~I(AOT/CWV),data=site.df))
  title = c(dat.regress$region,str_c('Rval',signif(rval,3),'RMSerr',signif(RMSerr,3),sep=' '))
  plot(site.df$AOT/site.df$CWV,fitted(dat.regress$fit)+residuals(dat.regress$fit),xlab='AOT/CWV',ylab='Obs',main=title,col=ptcolor,pch=19)
}

# Random effects
randeffects = ranef(dat.regress$fit)
plot(randeffects[[1]][,1],type='h',xlab='DOY',ylab=paste(randeffects$name,'Random Effect'),main=title)
points(randeffects[[1]][,1],pch=21,bg="cadetblue",cex=0.6)

### When rest of the script is fixed, move this to end
if (do.pdf) {
  dev.off()
  cat(file.pdf,'\n')
}


## May want to randomize point plot order, so that points later in time
## do not obscure earlier points so much.

