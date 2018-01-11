nnIndex<-function (X, id = 1:(X$n), smark = NULL, N = NULL, R = NULL, 
                   rm.id = NULL, add.X = NULL, add.id = paste("add", 1:(add.X$n), 
                                                              sep = ""), buffer = FALSE, buf.xwid = 5, buf.ywid = 5, 
                   exclusion = FALSE) 
{
  library(spatstat)
  ppp = rebuild.ppp(X = X, id = id, rm.id = rm.id, add.X = add.X, 
                    add.id = add.id)
  buf.ppp <- buffer(X = ppp, buf.xwid = buf.xwid, buf.ywid = buf.ywid)
  zone = as.data.frame(buf.ppp)[c("id", "x", "y", "zone")]
  data <- as.data.frame(buf.ppp)
  nnid <- nnid(X = ppp, N = N, R = R, id = as.data.frame(ppp)$id, 
               exclude = FALSE)
  if (!is.null(N)) {
    nndist <- spatstat::applynbd(X = ppp, N = N, function(dists, 
                                                          ...) {
      sort(dists)
    }, exclude = TRUE)
    if (N == 1) {
      nndist.id <- cbind(id = as.data.frame(ppp)$id, data.frame(nndist))
    }
    else {
      nndist.id <- cbind(id = as.data.frame(ppp)$id, data.frame(t(nndist)))
    }
  }
  if (!is.null(R)) {
    minnndist = spatstat::minnndist(X = ppp)
    if (R <= spatstat::minnndist(X = ppp)) 
      stop(paste("R must exceed the minimum nearest-neighbour distance (", 
                 minnndist, ")", sep = ""))
    nndist <- spatstat::applynbd(X = ppp, R = R, function(dists, 
                                                          ...) {
      sort(dists)
    }, exclude = TRUE)
    nndist <- list_to_matrix(nndist)
    if (nrow(nndist) == 1) {
      nndist.id <- cbind(id = as.data.frame(ppp)$id, data.frame(t(nndist)))
    }
    else {
      nndist.id <- cbind(id = as.data.frame(ppp)$id, data.frame(nndist))
    }
  }
  colnames(nndist.id) = c("id", paste("dist", 1:(ncol(nndist.id) - 
                                                   1), sep = ""))
  nndist <- nndist.id[, which(colnames(nndist.id) != "id"), 
                      drop = FALSE]
  C <- list()
  H <- list()
  for (i in 1:length(smark)) {
    for (m in 1:ncol(nnid)) {
      C[m][[1]] <- data[smark[i]][, 1][match(nnid[, m], 
                                             data$id)]
    }
    H[[i]] <- sapply(C, as.matrix)
  }
  nnIndex <- lapply(H, as.data.frame)
  names(nnIndex) <- paste("nn", smark, sep = "")
  for (i in 1:length(nnIndex)) {
    names(nnIndex[[i]]) = c(smark[i], paste(smark[i], 1:(length(names(nnIndex[[i]])) - 
                                                           1), sep = ""))
  }
  nnIndex$nndist = nndist
  nnIndex$nnid = nnid
  if (exclusion) {
    nnIndex <- lapply(nnIndex, function(x) x[, -1])
  }
  else {
    nnIndex <- nnIndex
  }
  nnIndex$zone = zone
  if (buffer) {
    nnIndex <- nnIndex
  }
  else {
    nnIndex <- lapply(nnIndex, function(x) as.data.frame(x)[-which(data$zone == 
                                                                     "buffer"), ])
  }
  nnIndex$data <- buf.ppp
  return(nnIndex)
}
  
  
  
  
  

























