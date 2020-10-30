# compute_scales is a generalized function for computing scale information.
# its default behavior is to use regular expression pattern matching to 
# identify columns from the same "construct" set. when using defaults, 
# compute_scales() assumes a consistent naming scheme for variables from 
# the same set and that reverse scored items are demarcated by "r" or "R"
# at the end of their names. If using an unconventional or inconsistent 
# naming scheme, the "stem" argument allows the user to specify a character
# vector matching the names of all columns in a set and the "keys" argument
# allows the user to specify columns that are reverse coded. "stem" and "keys"
# may also be numeric vectors specifying column locations. 
# for integration with tidy data wrangling, specifying return.all = TRUE causes 
# compute_scales() to behave like dplyr::mutate().
# users may specify any summary function of interest using the .funs argument. 
# scale averages are computed when no function is specified.
# users may add additional arguments for .fun in "..."
compute_scales <- function(data, stem = NULL, keys = NULL, .fun = mean,
                           return.all = FALSE, ...)
{
  dots <- list(...)
  if(!is.data.frame(data) & !is.matrix(data)) stop('"data" must be a matrix or data.frame with >= 2 columns!')
  .names <- dimnames(data)[[2]]
  use.names <- tolower(.names)
  use.names <- gsub(pattern = "[[:space:]]|[[:punct:]]|[[:digit:]]", ".", use.names)
  if(is.null(stem)){
    stem <- use.names[!duplicated(use.names)]
  } else if(is.numeric(stem)){
    stem <- use.names[stem]
  } else if(is.character(stem)){
    stem <- gsub(pattern = "[[:space:]]|[[:punct:]]|[[:digit:]]", ".", stem)
  }
  if(!is.null(keys)){
    if(is.numeric(keys)){
      tmp <- logical(length(use.names))
      tmp[keys] <- TRUE
    } else if(is.character(keys)){
      tmp <- logical(length(use.names))
      tmp[which(!is.na(match(.names, keys)))] <- TRUE
    }
    keys <- tmp
  } else {
    keys <- grepl("r$", use.names)
  }
  n <- length(stem)
  out <- matrix(0, nrow(data), n)
  for(i in 1:n){
    idx <- which(grepl(stem[i], use.names))
    tmp <- data[, idx, drop = FALSE]
    if(any(keys[idx])){
      for(j in which(keys[idx])){
        tmp[, j] <- max(tmp[, j]) - tmp[, j]
      }
    }
    out[, i] <- do.call(apply, args = c(X = list(tmp), MARGIN = 1, FUN = .fun, dots))
  }
  stem <- gsub("\\.", "", stem)
  stem <- paste0(stem, "_scale")
  colnames(out) <- stem
  if(return.all) out <- cbind(data, out)
  out
}
