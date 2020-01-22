pt.color<- function(x,low=min(x,na.rm=T),high=max(x,na.rm=T),nvals=10) {
#+
# PURPOSE
# To provide color numbers for each point in x in the
# data-interval from low to high
# along a palette of colors (specified externally),
# one of nvals color numbers provided for length(x) points
# revised 2012.03.07
# -
npts = length(x)
ptcolor = vector(length=npts,mode="numeric")
hlrange = high-low
x = pmin(x,high)
x = pmax(x,low)
for (i in 1:npts) {
    ptcolor[i] = ((nvals-1)/hlrange)*(x[i] - low)+ 1
}
return(ptcolor)
}
