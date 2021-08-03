heterogeneity <- function(x, type = "blau", na.rm = T)
{
  type <- tolower(type)
  if(any(is.na(x))){
    if(na.rm){
      use_x <- x[!is.na(x)]
    } else {
      x[is.na(x)] <- "..this_is_missing.."
      use_x <- x
    }
  } else {
    use_x <- x
  }
  unique_vals <- unique(use_x)
  N <- length(use_x)
  n_vals <- length(unique_vals)
  p <- numeric(length = n_vals)
  for(i in 1:n_vals){
    p[i] <- (sum(use_x == unique_vals[i]) / N)
  }
  if(type == "blau"){
    h <- 1 - sum(p^2)
  } else if(type == "teachman"){
    h <- -1 * sum(p * log(p))
  }
  h
}
