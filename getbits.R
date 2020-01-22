getbits = function(dat,start,end) {
# Extract a bitfield
# indexing starts at 1
# Dimension is preserved
  ddim = dim(dat)
  mask = as.integer(0)
  for (i in start:end) mask = mask + as.integer(2^(i-1))
  if (length(ddim) == 0) {
    bitwShiftR(bitwAnd(dat,mask),start-1)
  } else {
    matrix(bitwShiftR(bitwAnd(dat,mask),start-1),ddim)
  }
}
