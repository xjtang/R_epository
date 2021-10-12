# Accuracy Tools
# By xjtang
#----------------------------------------------------------------

# confusion matrix
confusion_matrix <- function(map, ref) {
  cls <- sort(unique(c(map, ref)))
  nCls <- length(cls)
  n <- length(map)
  conf <- matrix(0, nCls + 1, nCls + 1)
  conf[1, -1] <- cls
  conf[-1, 1] <- cls
  conf[1, 1] <- n
  for (i in 1:n) {
    conf[ref[i], map[i]] <- conf[ref[i], map[i]] + 1
  }
  return(conf)
}

#--------------------------------------

# accuracy assessment
accuracy_assessment <- function(map, ref, sta, staSize) {
  staCls <- sort(unique(sta))
  cls <- sort(unique(c(ref, map)))
  n <- sum(staSize[, 2])
  nCls <- length(cls)
  nSta <- length(staCls)
  nSample <- length(ref)
  staCnt <- rep(0, nSta)
  nh = rep(0, nSta)
  for (i in 1:nSta) {
    nh[i] <- sum(sta == staCls[i])
    staCnt[i] <- staSize[staSize[, 1] == staCls[i], 2]
  }
  
  # initialize coefficients
  y_all <- rep(0, nSample)
  y_user <- matrix(0, nSample, nCls)
  x_user <- matrix(0, nSample, nCls)
  y_prod <- matrix(0, nSample, nCls)
  x_prod <- matrix(0, nSample, nCls)
  y_area <- matrix(0, nSample, nCls)
  y_err <- array(0, c(nCls, nCls, nSample))
  
  # initialize coefficient means
  yh_all <- rep(0, nSta)
  yh_user <- matrix(0, nCls, nSta)
  xh_user <- matrix(0, nCls, nSta)
  yh_prod <- matrix(0, nCls, nSta)
  xh_prod <- matrix(0, nCls, nSta)
  yh_area <- matrix(0, nCls, nSta)
  yh_err <- array(0, c(nCls, nCls, nSta))
  
  # initialize coefficient variances and covariances
  yv_all <- rep(0, nSta)
  yv_user <- matrix(0, nCls, nSta)
  xv_user <- matrix(0, nCls, nSta)
  co_user <- matrix(0, nCls, nSta)
  yv_prod <- matrix(0, nCls, nSta)
  xv_prod <- matrix(0, nCls, nSta)
  co_prod <- matrix(0, nCls, nSta)
  yv_area <- matrix(0, nCls, nSta)
  
  # initialize accuracies
  X_user <- rep(0, nCls)
  X_prod <- rep(0, nCls)
  conf <- matrix(0, nCls, nCls)
  a_user <- rep(0, nCls)
  a_prod <- rep(0, nCls)
  a_all <- 0
  area <- rep(0, nCls)
  f1 <- rep(0, nCls)
  
  # initialize standard error
  v_area <- rep(0, nCls)
  v_user <- rep(0, nCls)
  v_prod <- rep(0, nCls)
  v_all <- 0
  se_area <- rep(0, nCls)
  se_user <- rep(0, nCls)
  se_prod <- rep(0, nCls)
  se_all <- 0
  
  # calculate coefficients
  for (i in 1:nSample) {
    y_all[i] <- (map[i] == ref[i])
    y_user[i, ] <- (map[i] == ref[i]) & (cls == map[i])
    x_user[i, ] <- (cls == map[i])
    y_prod[i, ] <- (map[i] == ref[i]) & (cls == ref[i])
    x_prod[i, ] <- (cls == ref[i])
    y_area[i, ] <- (cls == ref[i])
    y_err[cls == map[i], cls == ref[i], i] <- 1
  }
  
  # calculate coefficients means
  for (i in 1:nSta) {
    yh_all[i] <- mean(y_all[sta == staCls[i]])
    yh_user[, i] <- apply((y_user[sta == staCls[i], ]), 2, mean)
    xh_user[, i] <- apply((x_user[sta == staCls[i], ]), 2, mean)
    yh_prod[, i] <- apply((y_prod[sta == staCls[i], ]), 2, mean)
    xh_prod[, i] <- apply((x_prod[sta == staCls[i], ]), 2, mean)
    yh_area[, i] <- apply((y_area[sta == staCls[i], ]), 2, mean)
    yh_err[, , i] <- rowMeans((y_err[, , sta == staCls[i]]), dims=2)
  }
  
  # calculate coefficients variance
  for (i in 1:nSta) {
    yv_all[i] <- var(y_all[sta == staCls[i]])
    for (j in 1:nCls) {
      yv_user[j, i] <- var(y_user[sta == staCls[i], j])
      xv_user[j, i] <- var(x_user[sta == staCls[i], j])
      yv_prod[j, i] <- var(y_prod[sta == staCls[i], j])
      xv_prod[j, i] <- var(x_prod[sta == staCls[i], j])
      yv_area[j, i] <- var(y_area[sta == staCls[i], j])
    }
  }
  
  # calculate coefficients covariance
  for (i in 1:nSta) {
    for (j in 1:nCls) {
      co_user[j, i] <- cov(y_user[sta == staCls[i], j], x_user[sta == staCls[i], j])
      co_prod[j, i] <- cov(y_prod[sta == staCls[i], j], x_prod[sta == staCls[i], j])
    }
  }
  
  # calculate accuracies
  a_all <- (yh_all %*% staCnt) / n
  for (i in 1:nCls) {
    X_user[i] <- xh_user[i, ] %*% staCnt
    X_prod[i] <- xh_prod[i, ] %*% staCnt
    a_user[i] <- (yh_user[i, ] %*% staCnt) / (xh_user[i, ] %*% staCnt)
    a_prod[i] <- (yh_prod[i, ] %*% staCnt) / (xh_prod[i, ] %*% staCnt)
    area[i] <- (yh_area[i, ] %*% staCnt) / n
    f1[i] <- getF1(a_user[i], a_prod[i])
    for (j in 1:nCls) {
      conf[i, j] <- (yh_err[i, j, ] %*% staCnt) / n
    }
  }
  
  # calculate standard errors
  v_all <- (((staCnt / n) ^ 2) * (1 - nh / staCnt)) %*% (yv_all / nh)
  se_all <- sqrt(v_all)
  for (i in 1:nCls) {
    v_area[i] <- (((staCnt / n) ^ 2) * (1 - nh / staCnt)) %*% (yv_area[i, ] / nh)
    se_area[i] <- sqrt(v_area[i])
    v_user[i] <- (((staCnt / X_user[i]) ^ 2) * (1 - nh / staCnt)) %*% ((yv_user[i, ] + a_user[i]^2 * xv_user[i, ] - 2 * a_user[i] * co_user[i, ]) / nh)
    se_user[i] <- sqrt(v_user[i])
    v_prod[i] <- (((staCnt / X_prod[i]) ^ 2) * (1 - nh / staCnt)) %*% ((yv_prod[i, ] + a_prod[i]^2 * xv_prod[i, ] - 2 * a_prod[i] * co_prod[i, ]) / nh)
    se_prod[i] <- sqrt(v_prod[i])
  }
    
  # prepare output
  r <- vector(mode="list", length=6)
  names(r) <- c('overall', 'user', 'producer', 'area', 'conf', 'f1')
  r[[1]] <- c(a_all, se_all)
  r[[2]] <- rbind(a_user, se_user)
  r[[3]] <- rbind(a_prod, se_prod)
  r[[4]] <- rbind(area, se_area)
  r[[5]] <- conf
  r[[6]] <- f1
  
  # done
  return(r)
}

#--------------------------------------

# numeric example
numeric_example <- function() {
  sta <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
  map <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
  ref <- c(1, 1, 1, 1, 1, 3, 2, 1, 2, 3, 1, 2, 2, 2, 2, 2, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4, 2, 2, 1, 4, 4, 4, 4, 4, 4, 4, 3, 3, 2)
  staSize = t(rbind(c(1, 2, 3, 4), c(40000, 30000, 20000, 10000)))
  return(accuracy_assessment(map, ref, sta, staSize))
}

#--------------------------------------

# calculate F1 score
getF1 <- function(user, producer) {
  return(2 * (user * producer) / (user + producer))
}

#--------------------------------------

# calculate lag time from doy
lagTime <- function(map, ref) {
  if ((map == 0) || (ref == 0)) {
    return(0)
  } else {
    return(as.numeric(as.Date(map) - as.Date(ref)))
  }
}

#--------------------------------------

# assess change with date
accuracy_curve <- function(data, staSize, lagRange=c(0,200)) {
  n <- nrow(data)
  lagtime <- seq(lagRange[1], lagRange[2])
  nLag <- length(lagtime)
  user <- rep(0, nLag)
  se_user <- rep(0, nLag)
  prod <- rep(0, nLag)
  se_prod <- rep(0, nLag)
  f1 <- rep(0, nLag)
  for (i in 1:nLag) {
    lag <- lagtime[i]
    ref <- data[, 'ref']
    map <- (data[, 'map']) & (data[, 'lag'] <= lag)
    sta <- data[, 'sta']
    aa <- accuracy_assessment(map, ref, sta, staSize)
    user[i] <- aa$user[1, 2]
    se_user[i] <- aa$user[2, 2]
    prod[i] <- aa$producer[1, 2]
    se_prod[i] <- aa$producer[2, 2]
    f1[i] <- aa$f1[2]
  }
  return(rbind(lagtime, user, se_user, prod, se_prod, f1))
}

#--------------------------------------

# find level-off point
level_off <- function(curve) {
  
  
}

#--------------------------------------
# end