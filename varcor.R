varcor <- function(x, y = NULL, adjust = TRUE, na.rm = TRUE, upper = FALSE,
                   cor = FALSE, test = FALSE, alpha = .05)
{
  if(!is.matrix(x)) x <- as.matrix(x)
  if(is.null(y)){
    y <- x
  } else {
    if(!is.matrix(y)) y <- as.matrix(y)
  }
  xnames <- colnames(x)
  ynames <- colnames(y)
  mx <- nrow(x)
  nx <- ncol(x)
  my <- nrow(y)
  ny <- ncol(y)
  nn <- vector(mode = 'numeric', length = (nx*(nx-1))/2)
  ni <- 1
  if(mx != my | nx != ny) stop(gettext('"x" and "y" are different dimensions!'))
  if(is.null(xnames)) xnames <- paste0("x", 1:nx)
  if(is.null(ynames)) ynames <- paste0("y", 1:ny)
  v <- matrix(0, nx, nx)
  for(j in 1:nx){
    na_y <- !is.na(y[, j])
    ymm  <- y[, j] - mean(y[, j], na.rm = na.rm)
    for(i in j:nx){
      if(na.rm){
        idx <- which(na_y & !is.na(x[, i]))
        n   <- length(idx)
      } else {
        idx <- 1:mx
        n   <- mx
      }
      if(adjust) n <- n - 1
      xmm <- x[idx, i] - mean(x[idx, i])
      ymm <- ymm[idx]
      v[i, j] <- sum(xmm * ymm) / n
      if(cor){
        sdx <- sqrt(sum(xmm^2)/n)
        sdy <- sqrt(sum(ymm^2)/n)
        v[i, j] <- v[i, j] / (sdx * sdy)
      }
      if(j != i){
        nn[ni] <- n
        ni <- ni + 1
      }
    }
  }
  if(upper) v[which(upper.tri(v))] <- v[which(lower.tri(v))]
  if(test){
    if(!cor) stop(gettext('Method "test" for cor = TRUE only!'))
    if(adjust) nn <- nn + 1
    ROW <- row(v)
    COL <- col(v)
    rgc <- which(ROW > COL)
    v  <- v[rgc]
    se <- sqrt((1 - v^2) / (nn - 2))
    zr <- 0.5 * log((1 + v) / (1 - v))
    zq <- qnorm(alpha/2, 0, 1, lower.tail = FALSE)
    zi <- zq * sqrt(1 / (nn - 3))
    cl <- exp(2 * (zr - zi))
    cl <- (cl - 1) / (cl + 1)
    cu <- exp(2 * (zr + zi))
    cu <- (cu - 1) / (cu + 1)
    v  <- data.frame(
      cor  = v,
      df   = nn - 2,
      se   = se,
      ci.l = cl,
      ci.u = cu,
      row.names = paste(ynames[COL[rgc]], xnames[ROW[rgc]],  sep = "-")
    )
  } else {
    dimnames(v) <- list(xnames, ynames)
  }
  v
}
