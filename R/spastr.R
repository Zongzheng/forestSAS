spastr<-
  function (X_df, smark = c("spe", "storey", "dbh", "cw", "x",
                            "y"), buffer = FALSE, xrange = c(0, 100), yrange = c(0, 100),
            xwidth = 5, ywidth = 5, buf.xwid = 5, buf.ywid = 5, exclusion = FALSE)
  {
    expandX <- expandedge(spatstat.geom::ppp(x = X_df$x, y = X_df$y,
                                             xrange = xrange, yrange = yrange, marks = X_df[4:ncol(X_df)]),
                          xwidth = xwidth, ywidth = ywidth, id = X_df$id, type = "ppp")
    nnM <- nnIndex(X = expandX, id = expandX$marks$id, buf.xwid = buf.xwid,
                   buf.ywid = buf.ywid, N = 4, smark = smark)
    coredata <- subset(as.data.frame(nnM$data), zone == "core")
    coredata$M <- fsasN4(nnM$nnspe, match.fun = mingling)$result$index
    coredata$H <- fsasN4(nnM$nnstorey, match.fun = differ)$result$index
    coredata$W <- fsasN4(nnangle(nnM$nndist, nnM$nnx, nnM$nny)$nnangle,
                         match.fun = uniform.angle, para = 72)$result$index
    coredata$C <- fsasN4(nnoverlap(nnM$nncw, nnM$nndist), match.fun = crowding)$result$index
    coredata$U <- fsasN4(nnM$nndbh, match.fun = dominance)$result$index
    index_pv <- data.frame(M = pv(coredata$M, optm = 1), H = pv(coredata$H,
                                                                optm = 1), C = pv(coredata$C, optm = 0.5), W = pv(coredata$W,
                                                                                                                  optm = 0.5), U = pv(coredata$U, optm = 0))
    index_mean <- apply(coredata[c("M", "H", "W", "C", "U")],
                        2, mean)
    index_pv <- as.numeric(as.character(index_pv))
    names(index_pv) <- names(index_mean)
    list(Index_mean = index_mean, Index_pv = index_pv,
         Omega = sum(index_pv^2)/length(index_pv),
         Coredata = coredata, Datappp = nnM$data)
  }

