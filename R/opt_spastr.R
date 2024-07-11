opt_spastr<-
  function (X_df, inter = 100, pop = 0.2, smark = c("spe", "storey",
                                                    "dbh", "cw", "x", "y"), xrange = c(0, 100), yrange = c(0,
                                                                                                           100), xwidth = 5, ywidth = 5, buf.xwid = 5, buf.ywid = 5)
  {
    sample_id <- function(id, pop, inter) {
      selid <- list()
      for (i in 1:inter) {
        selid[[i]] <- sample(id, replace = FALSE, size = round(length(id) *
                                                                 pop, 0))
      }
      return(selid)
    }
    selid <- sample_id(id = X_df$id, pop = pop, inter = inter)
    Xppp <- spatstat.geom::ppp(x = X_df$x, y = X_df$y, xrange = xrange,
                               yrange = yrange, marks = X_df[4:ncol(X_df)])
    rmX <- list()
    cutdata <- list()
    for (i in 1:inter) {
      rmX[[i]] <- rebuild.ppp(X = Xppp, id = X_df$id, rm.id = selid[[i]])
      cutdata[[i]] = spastr(X_df = as.data.frame(rmX[[i]]),
                            xrange = xrange, yrange = yrange, smark = smark,
                            xwidth = xwidth, ywidth = ywidth, buf.xwid = buf.xwid,
                            buf.ywid = buf.ywid)
    }
    loc <- order(unlist(lapply(cutdata, function(x) x$Omega)),
                 decreasing = TRUE)[1]
    cutid <- selid[[loc]]
    Omega <- unlist(lapply(cutdata, function(x) x$Omega))[loc]
    Index_mean <- lapply(cutdata, function(x) x$Index_mean)[[loc]]
    Index_pv <- lapply(cutdata, function(x) x$Index_pv)[[loc]]
    coredata <- lapply(cutdata, function(x) x$Coredata)[[loc]]
    datappp <- lapply(cutdata, function(x) x$Datappp)[[loc]]
    list(Cutid = cutid, Index_mean = Index_mean, Index_pv = Index_pv,
         Omega = Omega, Coredata = coredata, Datappp = datappp)
  }
