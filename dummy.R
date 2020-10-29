# dummy() is an extension of stats::contrasts() which returns a matrix of 
# contrast codes for all observations in data set "data" when return.all = FALSE,
# and the complete dataset with the contrasts "cbinded" when return.all = TRUE
# if no data are given and levels is given, dummy() simplifies to stats::contrasts()
dummy <- function(data = NULL, var = NULL, levels = NULL, contrasts = FALSE, 
                  return.all = FALSE, fix.names = TRUE, ...)
{
  var  <- substitute(var)
  dots <- list(...)
  if(is.null(data)){
    if(length(dots) > 1){
      data <- dots[[1]]
      if(!is.matrix(data) & !is.data.frame(data)) data <- matrix(data)
    } 
  } else if(!is.matrix(data) & !is.data.frame(data)){
    data <- matrix(data)
  } 
  
  NAMES <- dimnames(data)[[2]]
  m <- nrow(data)
  n <- ncol(data)
  
  if(!is.null(data) & !is.null(var)){
    var_pos <- setNames(as.list(seq_along(data)), NAMES)
    idx <- eval(var, var_pos)
    tmp <- data[, idx, drop = TRUE]
  } else if(!is.null(data) & is.null(var)){
    if(n == 1){
      tmp <- as.vector(data)
    } else {
      stop('Cannot match "var" to names(data)!')
    }
  } else {
    tmp <- NULL
  }
  
  if(is.null(levels) & !is.null(tmp)){
    levels <- tmp
  } else if(is.null(levels) & is.null(tmp)){
    stop('No levels to contrast!')
  } else if(!any(levels %in% tmp)){
    stop('"levels" not found in data!')
  }
  
  if(is.factor(levels)){
    levels <- levels(levels)
  } else {
    levels <- unique(levels)
  }
  nlevels <- length(levels)
  
  if(is.null(data)){
    out <- diag(1, nlevels, nlevels)
    if(contrasts) out[1, ] <- 0
    dimnames(out) <- list(levels, levels)
    return(out)
  }
  
  out <- matrix(0, m, nlevels)
  out[which(tmp == levels[1]), 1] <- ifelse(contrasts, 0, 1)
  for(i in 2:nlevels){
    idx <- which(tmp == levels[i])
    out[idx, i] <- 1
  }
  if(fix.names) {
    levels <- gsub(pattern = "[[:space:]]|[[:punct:]]", "_", levels)
    levels <- as.character(tolower(levels))
    if(any(is.na(levels))) levels[is.na(levels)] <- "none"
  }
  colnames(out) <- as.character(levels)
  if(return.all){
    colnames(data) <- NAMES
    out <- cbind(data, out)
  }
  as.data.frame(out)
}
