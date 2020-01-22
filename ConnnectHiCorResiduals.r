# ConnnectHiCorResiduals.r
# connect regions of high residual correlation
#
library(stringr)
library(fields)
source('~esswein/lib/R/runinfo.R')
source('~esswein/lib/R/midpoint.R')
add.alpha <- function (hex.color.list, alpha) sprintf("%s%02X", hex.color.list, floor(alpha * 256))
ri = runinfo()
if (!exists('do.pdf')) do.pdf = F

# Get list of all subregions of this region:
coord.files = dir('~esswein/science/region/boundary',str_c(region,'.*.coords'),full.names=T)
subs = basename(coord.files)
subs = sub(str_c(region,'.'),'',subs)
subs = sub('coords','',subs)
subs = sub('\\.','',subs)

if (subregion != '') {
  # If subregion was specified, make map only for that subregion
  map.regions = subregion
  file.pdf = sub(str_c(subregion,'.rds'),str_c('ConnnectHiCorResiduals.',subregion,'.pdf'),file.regress)
} else {
  # If subregion was not specified, make maps for whole region and each subregion
  map.regions = subs
  file.pdf = sub('.rds','.ConnnectHiCorResiduals.pdf',file.regress)
  file.pdf = sub('Regress.rds','ConnnectHiCorResiduals.pdf',file.regress)
}
file.rds = sub('\\.pdf','.rds',file.pdf)

dat.rgr = readRDS(file.regress)

site.data = dat.rgr$site.data
site.df = dat.rgr$site.df
sites = unique(site.df$name)
dates = unique(sort(site.df$yyyymmddhhmm))
n.site = length(sites)
n.date = length(dates)

# Get residuals by site:
resid.by.site = matrix(NA,n.date,n.site)
colnames(resid.by.site) = sites
isite.used = match(site.df$name,sites)
itime.used = match(site.df$yyyymmddhhmm,dates)
resid.by.site[cbind(itime.used,isite.used)] = site.df$resid

# Delete empty rows and columns:
all.na.site = rep.int(F,n.site)
for (i.site in 1:n.site) all.na.site[i.site] =  all(is.na(resid.by.site[,i.site]))
all.na.date = rep.int(F,n.date)
for (i.date in 1:n.date) all.na.date[i.date] = all(is.na(resid.by.site[i.date,]))
resid.by.site = as.matrix(resid.by.site[!all.na.date,][,!all.na.site])
n.site = ncol(resid.by.site)
n.date = nrow(resid.by.site)

if (do.pdf) pdf(file=file.pdf,title=paste('written_by',ri$prog))

index.in.all.sta = match(colnames(resid.by.site),dimnames(site.data)[[3]])
scale.lon = cos(pi/180*mean(site.data[1,"lat",index.in.all.sta ]))

opar = par(no.readonly=T)
par(xpd=NA)
darkaqua6 = add.alpha(gplots::col2hex("aquamarine4"),0.6)

sta.res.cor = cor(resid.by.site, use="pairwise")
sta.res.cov =  cov(resid.by.site, use="pairwise")

min.cor = 0.55 
min.cov = 25

for (mapreg in map.regions) {

  # get coords
  coord.file = str_c('~esswein/science/region/boundary/',region)
  if (mapreg == '') coord.file = str_c(coord.file,'.coords') else coord.file = str_c(coord.file,'.',mapreg,'.coords')
  coords = read.table(coord.file,header=T)
## NOTE: scale lon for spatial correctness.
  xlim = scale.lon*range(coords$lon)
  ylim = range(coords$lat)
plot(scale.lon*site.data[1,"lon",index.in.all.sta ],site.data[1,"lat",index.in.all.sta ],asp=1,xlim=xlim,ylim=ylim,xlab='',ylab='',xaxt='n',yaxt='n')
text( (scale.lon*site.data[1,"lon",index.in.all.sta]), ( site.data[1,"lat",index.in.all.sta] ), 
  labels = colnames(resid.by.site), pos=2, offset=1, cex=0.7, col=darkaqua6,font=4) 

  for ( first.sta  in 2:(n.site) )   {
    end.sta = first.sta - 1
    for ( second.sta in 1:(end.sta) )   {
      if (is.na(sta.res.cor[first.sta, second.sta])) next
      if (sta.res.cor[first.sta, second.sta] > min.cor ) {
        lines( c(scale.lon*site.data[1,"lon",index.in.all.sta[first.sta ]], scale.lon*site.data[1,"lon",index.in.all.sta[second.sta ]]) ,
          c( site.data[1,"lat",index.in.all.sta[first.sta ]], site.data[1,"lat",index.in.all.sta[second.sta ]]) ,
          lwd= (sta.res.cor[first.sta,second.sta]/0.3 ) )

        text( x= 0.5*(scale.lon*site.data[1,"lon",index.in.all.sta[first.sta ]] +  scale.lon*site.data[1,"lon",index.in.all.sta[second.sta ]]) ,
          y = 0.5*( site.data[1,"lat",index.in.all.sta[first.sta ]] + site.data[1,"lat",index.in.all.sta[second.sta ]]),
          labels=format(sta.res.cor[first.sta, second.sta],digits=2) , pos=4, offset = 1, cex=2.25*(sta.res.cor[first.sta, second.sta])^2 )
      }
    }
  }

}

toffset = -.02
site.lab = rev(sites[index.in.all.sta])
m = t(sta.res.cor)
m[upper.tri(m,diag=T)] = NA
par(plt=c(.2,.85,.25,.90))
pal = two.colors(n=64,start='blue2',middle='gray80',end='red')
image.plot(m,xaxt='n',yaxt='n',col=pal,main='Residuals correlation')
lim = par('usr')
xtick = midpoint(seq(lim[1],lim[2],len=n.site+1))
ytick = midpoint(seq(lim[3],lim[4],len=n.site+1))
points(-10,-10)
text(xtick,lim[3]+toffset,lab=site.lab,srt=90,xpd=NA,adj=1,cex=.5)
text(lim[1]+toffset,ytick,lab=site.lab,xpd=NA,adj=1,cex=.5)


m = t(sta.res.cov)
m[upper.tri(m)] = NA
par(plt=c(.2,.85,.25,.90))
pal = two.colors(n=64,start='blue2',middle='gray80',end='red')
image.plot(m,xaxt='n',yaxt='n',col=pal,main='Residuals covariance')
lim = par('usr')
xtick = midpoint(seq(lim[1],lim[2],len=n.site+1))
ytick = midpoint(seq(lim[3],lim[4],len=n.site+1))
points(-10,-10)
text(xtick,lim[3]+toffset,lab=site.lab,srt=90,xpd=NA,adj=1,cex=.5)
text(lim[1]+toffset,ytick,lab=site.lab,xpd=NA,adj=1,cex=.5)



if (do.pdf) {
  dev.off()
  cat(file.pdf,'\n')
}
par(opar)

dat.save = list(resid.by.site=resid.by.site,sta.res.cov=sta.res.cov,sta.res.cor=sta.res.cor)
attr(dat.save,'written_by') = sub('/Users/','~',ri$prog)
saveRDS(dat.save,file=file.rds)
