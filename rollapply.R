rollapply <- function(data, var = NULL, k, fill = NA, .fun, align = "left", ...)
{
  var <- substitute(var)
  if(!is.matrix(data) & !is.data.frame(data)) data <- matrix(data)
  NAMES <- dimnames(data)[[2]]
  if(!is.null(data) & !is.null(var)){
    var_pos <- setNames(as.list(seq_along(data)), NAMES)
    idx <- eval(var, var_pos)
    data <- data[, idx, drop = FALSE]
  } 
  N <- nrow(data)
  V <- ncol(data)
  if(!is.list(k)) k <- list(k)
  if(!is.list(fill)) fill <- list(fill)
  if(!is.list(.fun)) .fun <- list(.fun)
  if(!is.list(align)) align <- list(align)
  dots  <- list(...)
  nk    <- length(k)
  nfill <- length(fill)
  nfun  <- length(.fun)
  nline <- length(align)
  ndots <- length(dots)
  nvals <- c(k = nk, fill = nfill, .fun = nfun, dots = ndots, align = nline)
  nmax  <- max(nvals)
  if(any(nmax != V & nmax != 1)){
    stop(gettext("Arguments must be length 1 or equal to ncol(data)"))
  } else {
    if(nk == 1) k <- as.list(rep(k, V))
    if(nfill == 1) fill <- as.list(rep(fill, V))
    if(nfun == 1) .fun <- as.list(rep(.fun, V))
    if(nline == 1) align <- as.list(rep(align, V))
    if(ndots == 1) dots <- as.list(rep(dots, V))
  }
  out <- matrix(0, N, V)
  for(j in 1:V){
    if(align[[j]] == "left"){
      idx <- 1L
      out[(N-(k[[j]]-1)):N, j] <- fill[[j]]
      n <- N-(k[[j]]-1)
    } else if(align[[j]] == "right"){
      idx <- k[[j]]
      out[1:(k[[j]]-1), j] <- fill[[j]]
      n <- N
    } else if(align[[j]] == "center"){
      idx <- median(1:k[[j]])
      out[1:(idx-1), j] <- fill[[j]]
      out[(N-(idx-1)):N, j] <- fill[[j]]
      n <- N-(idx-1)
    }
    for(i in 1:n){
      if(k[[j]] > N) break
      out[idx, j] <- do.call(.fun[[j]], c(list(data[i:k[[j]], j]), dots[j]))
      idx <- idx + 1
      k[[j]] <- k[[j]] + 1
    }
  }
  out
}
rollapply(mtcars, var = disp, 7, .fun = mean, align = "center", na.rm = T)
