position.labels = function(x,y,labels,map=F,plot=F,bty='n',join.type='n',pch=0,cex=1,cex.lab=1,...) {
source('~esswein/lib/R/deintersect.boxes.R')

n.pt = length(x)

# Allow plotting outside the plot region:
oxpd = par('xpd')
par(xpd=NA)

if (map) xlim = range(pretty(x)) else xlim = NULL
if (map) ylim = range(pretty(y)) else ylim = NULL
if (map) plot.default(x,y,xlim=xlim,ylim=ylim,cex=cex,cex.lab=1,...)

# Coords of boxes: points and labels
# 1:n.pt are the site locations; n.pt=1:2*n.pt are the starting positions
# of the labels.
xll = rep.int(x,2)
yll = rep.int(y,2)
# Make the symbol boxes somewhat larger then the size of one character:
# (Not sure if this is working)
wid = rep.int(strwidth('X',cex=cex),2*n.pt)
hgt = rep.int(strheight('X',cex=cex),2*n.pt)
wid[(n.pt+1):(2*n.pt)] = 1.1*strwidth(labels,cex=cex.lab)
hgt[(n.pt+1):(2*n.pt)] = 1.1*strheight(labels,cex=cex.lab)
xll[(n.pt+1):(2*n.pt)] = xll[(n.pt+1):(2*n.pt)] - .5*wid[(n.pt+1):(2*n.pt)]
# Labels that initially extend off the plot to the right.
# Change initial positions so hopefully the final position will not
# extend too far right.
# In future, modify deintersect.boxes to keep all boxes within
# some bounding box.
w.off = which(xll[(n.pt+1):(2*n.pt)] + wid[(n.pt+1):(2*n.pt)] > par('usr')[2])
xll[(n.pt+1):(2*n.pt)][w.off] = xll[(n.pt+1):(2*n.pt)][w.off] - wid[(n.pt+1):(2*n.pt)][w.off]

# Set the indices of the stationary boxes; ie. the boxes of the site locations.
# (The labels are not stationary)
sta = 1:n.pt
# Indices of the site labels:
sta.l = (n.pt+1):(2*n.pt)

# Calculate the adjusted box locations:
newxy = deintersect.boxes(xll,yll,wid,hgt,sta)
xll = newxy[,'xll']
yll = newxy[,'yll']
xctr = xll + wid/2
yctr = yll + hgt/2

# For each stationary box, find the closest point among the
# corners and midpoints of the corresponding label box.
# Then draw a line between them.
n.sta = length(sta)
all.labels = c(rep.int('',n.pt),labels)
for (i in 1:n.sta) {
  i.sta = sta[i]
  i.part = sta.l[i]
  # Corners and midpoints:
  attachx = c(xll[i.part], xll[i.part]+wid[i.part]/2, xll[i.part]+wid[i.part], xll[i.part]+wid[i.part], xll[i.part]+wid[i.part], xll[i.part]+wid[i.part]/2, xll[i.part], xll[i.part])
  attachy = c(yll[i.part], yll[i.part], yll[i.part], yll[i.part]+hgt[i.part]/2, yll[i.part]+hgt[i.part], yll[i.part]+hgt[i.part], yll[i.part]+hgt[i.part], yll[i.part]+hgt[i.part]/2)
  dist2 = (xctr[i.sta]-attachx)^2 + (yctr[i.sta]-attachy)^2
  iattach = which.min(dist2)
  if (join.type != 'n') lines(c(x[i.sta],attachx[iattach]),c(y[i.sta],attachy[iattach]),type=join.type)
  text(xll[i.part],yll[i.part],all.labels[i.part],adj=c(0,0),cex=cex.lab)
  if (bty != 'n') lines(c(attachx,attachx[1]),c(attachy,attachy[1]))
}
par(xpd=oxpd)

NULL
}
