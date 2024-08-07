nnangle <-
  function (nndist, nnx, nny)
  {
    xl <- as.data.frame(t(apply(nnx, 1, function(x) cbind(x[2:5] -
                                                            x[1]))))
    yl <- as.data.frame(t(apply(nny, 1, function(x) cbind(x[2:5] -
                                                            x[1]))))
    data1 <- as.matrix(acos(yl/nndist) * sign(xl) * 180/pi +
                         360)
    data1[which(data1 > 360)] <- data1[which(data1 > 360)] -
      360
    data2 <- data.frame(id = rownames(nndist), N = rownames(nndist),
                        data1)
    data3 <- split(data2[, -c(which(colnames(data2) == c("id",
                                                         "N")))], list(data2$N))
    data4 <- as.data.frame(t(sapply((lapply(data3,
                                            function(x) sort(unlist(as.vector(x))))), as.matrix)))
    data5 <- data4[rownames(nndist), ]
    names(data5) <- c("A1", "A2", "A3", "A4")
    angle1 <- data5$A2 - data5$A1
    angle2 <- data5$A3 - data5$A2
    angle3 <- data5$A4 - data5$A3
    angle4 <- data5$A4 - data5$A1
    nnangle <- cbind(angle1, angle2, angle3, angle4)
    nnangle[which(nnangle > 180)] <- 360 - nnangle[which(nnangle >
                                                           180)]
    nnangle <- data.frame(nnangle)
    rownames(nnangle) <- rownames(nndist)
    list(angle = data5, nnangle = nnangle)
  }
