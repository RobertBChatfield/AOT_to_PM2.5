rle_ext = function(x) {
# Results of rle plus more info
  r = rle(x)
  n.run = length(r$lengths)
  start = c(1,1+cumsum(r$lengths[1:(n.run-1)]))
  stop = cumsum(r$lengths)
  list(n.run=n.run,lengths=r$lengths,values=r$values,start=start,stop=stop)
}
