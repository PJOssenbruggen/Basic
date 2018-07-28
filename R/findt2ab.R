#' \code{t2} is the intersection point of the following driver sight-line and lead vehicle \code{x} trajectory.
#' Lead vehicle location \code{xl2} and speed \code{ul2} at the intersection point, respectively.
#'
#' @param tstart start time, a number
#' @param tmid intermediate time, a number
#' @param tend end time, a number
#' @param ulstart lead vehicle starting speed at \code{tstart}, a number
#' @param ulmid lead vehicle end speed at \code{tmid}, a number
#' @param xlstart lead vehicle location at \code{tstart}, a number
#' @param xlmid lead vehicle location at \code{tmid}, a number
#' @param xfstart following vehicle location at \code{tstart}, a number
#' @param uf following vehicle speed, a number
#' @param lty line type, a number
#' @param lwd line width, a number
#' @param col line color, a factor
#' @usage findt2ab(tstart, tmid, tend, ulstart, ulmid, xlstart, xlmid, xfstart, uf, lty, lwd, col)
findt2ab <- function(tstart, tmid, tend, ulstart, ulmid, xlstart, xlmid, xfstart, uf, lty, lwd, col) {
  tstart <- tstart
  tend   <- tmid
  ustart <- ulstart
  uend   <- ulmid
  xstart <- xlstart
  xend   <- xlmid
  step   <- 0.25
  ab     <- trajectoryab(tstart, tend, ustart, uend, xstart, xend, step)
  a      <- ab[[1]][1]
  b      <- ab[[1]][2]
  h01 <- function(t, tstart, ulstart, ulmid, xlstart, xlmid, xfstart, uf, a, b)
    xab(x0 = xlstart, u0 = ulstart, a, b, t, t0 = tstart) -
    xfollow(x0 = xfstart, u = uf, t, t0 = tstart)
  h12 <- function(t, tstart, tmid, xlmid, ulmid, xfstart, uf)
    xlmid + ulmid * (t - tmid) -
    xfollow(x0 = xfstart, u = uf, t, t0 = tstart)
  h0 <-  h01(t = tstart, tstart, ulstart, ulmid, xlstart, xlmid, xfstart, uf, a, b)
  h1 <-  h01(t = tmid,   tstart, ulstart, ulmid, xlstart, xlmid, xfstart, uf, a, b)
  j1 <-  h12(t = tstart, tstart, tmid, xlmid, ulmid, xfstart, uf)
  j2 <-  h12(t = tend,   tstart, tmid, xlmid, ulmid, xfstart, uf)
  s0 <-  sign(h0)
  s1 <-  sign(h1)
  w1 <-  sign(j1)
  w2 <-  sign(j2)
#  print(data.frame(s0,s1,w1,w2))
  t2 <- NA
  if(s0 != s1) {
    t2 <-  uniroot(h01,
                    tstart  = tstart,
                    ulstart = ulstart,
                    ulmid   = ulmid,
                    xlstart = xlstart,
                    xlmid   = xlmid,
                    xfstart = xfstart,
                    uf      = uf,
                    a, b,
                    interval = c(tstart, tmid), tol = 1e-9)$root
    xl2   <-  xab(xlstart, ulstart, a, b, t2, tstart)
    ul2   <-  uab(ulstart, a, b, t2, tstart)
    ul2   <-  min(uf, ulmid)
    dest   <- abs((ul2 - ulstart)/(t2 - tstart))
    answer <- as.matrix(data.frame(dest = dest, t2 = t2, xl2, ul2, test = 4))
#    points(tstart,xlstart)
#    points(tstart,xfstart)
#    points(t2,xl2)
#    text(tstart, xfstart, "test 4", cex = 1, pos = 3, offset = 1)
#    lines(c(tstart, t2), c(xfstart, xfstart + uf * (t2-tstart)), lty = 5, lwd = 1, col = gray(0.5))
    return(answer)
  }
  if(w1 != w2 & is.na(t2)) {
    t2 <-  uniroot(h12,
                    tstart = tstart,
                    tmid   = tmid,
                    xlmid  = xlmid,
                    ulmid  = ulmid,
                    xfstart = xfstart,
                    uf     = uf,
                    interval = c(tstart, tend), tol = 1e-9)$root
    xl2   <-  xfollow(xfstart, uf, t2, tstart)
    ul2   <-  min(uf, ulmid)
    dest  <-  abs((ul2 - ulstart)/(t2 - tstart))
    answer <- as.matrix(data.frame(dest = dest, t2 = t2, xl2, ul2, test = 1))
#    lines(c(tstart, t2), c(xfstart, xfstart + uf * (t2-tstart)), lty = 4, lwd = 1, col = gray(0.5))
    return(answer)
  }
  if(is.na(t2)) {
 #   print(data.frame(tstart, tmid, tend, ulstart, ulmid, xlstart, xlmid, xfstart, uf, a, b))
 #   print(data.frame(tstart, tmid, xfstart, xlmid, ulmid, uf))
    t2 <- (xlmid - xfstart + uf  * tstart - ulmid * tmid)/(uf - ulmid)
#    print(data.frame(t2, tstart))
    if(t2 > tstart) {
      xl2    <- xfstart + uf * (t2 - tstart)
      ul2    <- min(uf, ulmid)
      dest   <- abs((ul2 - ulstart)/(t2 - tstart))
      answer <- as.matrix(data.frame(dest = dest, t2 = t2, xl2, ul2, test = 2))
#     print(answer)
#     text(tstart, xfstart, "test 2", cex = 1, pos = 3, offset = 1)
#     lines(c(tstart, t2), c(xfstart, xfstart + uf * (t2-tstart)), lty = 2, lwd = 1, col = gray(0.5))
      return(answer)
    } else {
      t2  <- tmid
      ul2 <- ulmid
      xl2 <- xlstart + ulmid * (t2 - tstart)
      answer <- as.matrix(data.frame(dest = NA, t2 = t2, xl2, ul2, test = 2))
#      print(answer)
      }
  }
}
