dat3.to.df = function(dat3) {
# Convert 3d site data to data.frame
  first = T
  for (i in 1:(dim(dat3)[3])) {
    if (first) {
      dat2 = data.frame(dat3[,,i],name=dimnames(dat3)[[3]][i],stringsAsFactors=F)
      first = F
    } else {
      dat2 = rbind(dat2,data.frame(dat3[,,i],name=dimnames(dat3)[[3]][i],stringsAsFactors=F))
    }
  }
  jd.dat2 = julday(dat2[,'month',1],dat2[,'day',1],dat2[,'year',1],dat2[,'hour',1],dat2[,'minute',1],0)
  ord = order(jd.dat2)
  dat2 = dat2[ord,]
  dat2$jd = jd.dat2[ord]

  dat2
}
