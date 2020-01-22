library(stringr)

deintersect.boxes = function(xll.0,yll.0,wid,hgt,sta=NULL,show.progress=FALSE) {
# 23 Feb 2018 fix infinite loop when stationary boxes intersect
  interval.intersect = function(i1,i2) {
    ! (max(i1) < min(i2) | min(i1) > max(i2))
  }

  xll = xll.0
  yll = yll.0
  n.box = length(xll)

  if (n.box < 2) {
    cat('nothing to do\n')
    return(cbind(xll=xll,yll=yll))
  }

  have.sta = length(sta) > 0

  n.pass.max = 2*n.box

  xctr = xll + wid/2
  yctr = yll + hgt/2

  xlim = par('usr')[c(1,2)]
  ylim = par('usr')[c(3,4)]

  # To have spacing between boxes:
  xmarg = c(-.005,.005)
  ymarg = c(-.005,.005)

  n.pass = 0

  # Move boxes away from each other until there are no more overlaps:
  while (T) {
    n.pass = n.pass + 1

    if (show.progress) {
      plot(c(),xlim=xlim,ylim=ylim,xlab='',ylab='',xaxt='n',yaxt='n')
      rect(xll,yll,xll+wid,yll+hgt,col='gray90')
      if (have.sta) {
        rect(xll.0[-sta],yll.0[-sta],xll.0[-sta]+wid[-sta],yll.0[-sta]+hgt[-sta],border='gray50')
        rect(xll.0[sta],yll.0[sta],xll.0[sta]+wid[sta],yll.0[sta]+hgt[sta],border='red',lwd=2)
      } else {
        rect(xll.0,yll.0,xll.0+wid,yll.0+hgt,border='gray50')
      }
    }

    # Find intersecting boxes:
    # Form graph with edges between intersecting boxes.
    # Represent as adjacency matrix.
    adj = matrix(FALSE,n.box,n.box)
    for (i in 1:(n.box-1)) {
      for (j in (i+1):n.box) {
        adj[i,j] = interval.intersect(c(xll[i],xll[i]+wid[i])+xmarg,c(xll[j],xll[j]+wid[j])+xmarg) & interval.intersect(c(yll[i],yll[i]+hgt[i])+ymarg,c(yll[j],yll[j]+hgt[j])+ymarg)
        adj[j,i] = adj[i,j]
      }
    }
    if (have.sta) {
      if (any(adj[sta,sta])) {
#       cat('There are intersecting stationary boxes\n')
#       break
      }
    }
    if (n.pass > n.pass.max) {
      cat('max interations exceeded; giving up\n')
      break
    }

    # Find connected components:
    # Algorithm from Cormen, Lieserson, and Rivest, 1st ed., Chap 22
    cc = as.list(1:n.box)
    # Iterate over edges:
    for (i in 1:(n.box-1)) {
      for (j in (i+1):n.box) {
        if (adj[i,j]) {
          # Must be calculated each time because sets may be deleted
          n.set = length(cc)
          # Find indices of the sets that i and j belong to:
    ## Can we reduce these 3 lines to one? :
          w = rep.int(FALSE,n.set)
          for (k in 1:n.set) w[k] = (i %in% cc[[k]])
          w.i = which(w)

          w[] = FALSE
          for (k in 1:n.set) w[k] = (j %in% cc[[k]])
          w.j = which(w)

          ### Should not happen:
          if (length(w.i) != 1 | length(w.j) != 1) stop('multiple set membership')

          if (!setequal(cc[[w.i]],cc[[w.j]])) {
            cc[[w.i]] = union(cc[[w.i]],cc[[w.j]])
            cc[[w.j]] = NULL	# deletes the set from the list
          }
        }
      }
    }

    n.set = length(cc)
    ct.nsta = rep.int(999,n.set)	# counts of nonstationary boxes in each CC
    for (i.set in 1:n.set) {
      n.sta = length(intersect(cc[[i.set]],sta))
      ct.nsta[i.set] = length(cc[[i.set]]) - n.sta
    }
# browser()
    if (all(ct.nsta <= 1)) break
#   if (n.set == n.box) break	# no more intersections

    # Move boxes:
    for (i.set in 1:n.set) {
      set.size = length(cc[[i.set]])
      if (set.size > 1) {
        set.xctr = mean(xctr[cc[[i.set]]])
        set.yctr = mean(yctr[cc[[i.set]]])

        # For each element of the set, get the resultant of the vectors from it
        # to the other elements of the set:
        r.x = rep.int(NA,set.size)
        r.y = rep.int(NA,set.size)
        for (i.elem in 1:set.size) {
          r.x[i.elem] = mean(xctr[cc[[i.set]][i.elem]]-xctr[cc[[i.set]][-i.elem]])
          r.y[i.elem] = mean(yctr[cc[[i.set]][i.elem]]-yctr[cc[[i.set]][-i.elem]])
        }
        # The boxes are moved in the resultant direction.
        # Add a random component to (hopefully) avoid cycling.
        r.x = .05*(1+runif(set.size,max=.01))*r.x
        r.y = .05*(1+runif(set.size,max=.01))*r.y

        if (have.sta) {
          r.x[cc[[i.set]] %in% sta] = 0
          r.y[cc[[i.set]] %in% sta] = 0
        }

        xll[cc[[i.set]]] = xll[cc[[i.set]]] + r.x
        yll[cc[[i.set]]] = yll[cc[[i.set]]] + r.y
        xctr = xll + wid/2
        yctr = yll + hgt/2
      }
    }

  }
  cbind(xll=xll,yll=yll)
}
##  
##  if (!exists('newset')) newset = TRUE
##  
##  words = readRDS(str_c(R.home(),'/share/dictionaries/en_stats.rds'))
##  words = words[-grep('[^a-zA-Z]',words)]
##  
##  if (newset) {
##    n.box = 20
##    xll.0 = runif(n.box,min=.2,max=.8)
##    yll.0 = runif(n.box,min=.2,max=.8)
##    wid = runif(n.box,min=.05,max=.1)
##    hgt = runif(n.box,min=.05,max=.1)
##    n.sta = ceiling(n.box/2)
##    sta = sample(1:n.box,n.sta)
##    sta.l = sample(setdiff(1:n.box,sta),n.sta)
##  # The small stationary boxes are points
##  # The corresponding nonstationary boxes are their labels.
##    wid[sta] = .001
##    hgt[sta] = .001
##    txt.l = rep.int('',n.box)
##    txt.l[sta.l] = sample(words,n.sta)
##    # Start with labels close to corresponding points:
##    xll.0[sta.l] = xll.0[sta]
##    yll.0[sta.l] = yll.0[sta]
##    # Need this for "strwidth" to work:
##    plot(c(),xlim=xlim,ylim=ylim,xlab='',ylab='',xaxt='n',yaxt='n')
##    wid[sta.l] = strwidth(txt.l[sta.l])
##    hgt[sta.l] = strheight(txt.l[sta.l])
##  }
##  
##  newxy = unintersect.boxes(xll.0,yll.0,wid,hgt,sta)
##  
##  xll = newxy[,'xll']
##  yll = newxy[,'yll']
##  
##  
##  # For each stationary box, find the closest point among the
##  # corners and midpoints of the corresponding label box.
##  # Then draw a line between them.
##  for (i in 1:n.sta) {
##    i.sta = sta[i]
##    i.part = sta.l[i]
##    # Corners and midpoints:
##    attachx = c(xll[i.part], xll[i.part]+wid[i.part]/2, xll[i.part]+wid[i.part], xll[i.part]+wid[i.part], xll[i.part]+wid[i.part], xll[i.part]+wid[i.part]/2, xll[i.part], xll[i.part])
##    attachy = c(yll[i.part], yll[i.part], yll[i.part], yll[i.part]+hgt[i.part]/2, yll[i.part]+hgt[i.part], yll[i.part]+hgt[i.part], yll[i.part]+hgt[i.part], yll[i.part]+hgt[i.part]/2)
##    # Just midpoints:
##  # attachx = c(xll[i.part]+wid[i.part]/2, xll[i.part]+wid[i.part], xll[i.part]+wid[i.part]/2, xll[i.part])
##  # attachy = c(yll[i.part], yll[i.part]+hgt[i.part]/2, yll[i.part]+hgt[i.part], yll[i.part]+hgt[i.part]/2)
##    dist2 = (xctr[i.sta]-attachx)^2 + (yctr[i.sta]-attachy)^2
##    iattach = which.min(dist2)
##    lines(c(xctr[i.sta],attachx[iattach]),c(yctr[i.sta],attachy[iattach]))
##    text(xll[i.part],yll[i.part],txt.l[i.part],adj=c(0,0))
##  }
##  
##  cat('npass = ',n.pass,'\n')
