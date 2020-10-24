chronbach <- function(data, vars = NULL, na.action = "na.rm", if.item.removed = FALSE)
{
  # preparing the data
  vars <- substitute(vars)
  if(!is.data.frame(data)) data <- as.data.frame(data)
  if(!is.null(vars)){
    vars <- substitute(vars)
    var_pos <- setNames(as.list(seq_along(data)), names(data))
    pos <- eval(vars, var_pos)
    data <- data[, pos, drop = FALSE]
  }
  complete_obs <- nrow(data)
  n <- ncol(data)
  if(n < 2) stop(gettext('"data" must have at least two columns!'))
  numericalize <- vector(mode = "logical", length = n)
  for(i in 1:n){
    numericalize[i] <- FALSE
    if(is.numeric(data[, i])) numericalize[i] <- TRUE
  }
  if(any(!numericalize)){
    if(all(!numericalize)){
      stop(gettext('No numeric columns detected in "data"!'))
    }
    data <- data[, numericalize]
    warning(paste0("Non-numeric variables detected. Dropping\n",
                   names(data)[!numericalize]))
  }
  if(na.action == "na.rm"){
    complete <- apply(data, 1, function(i) sum(is.na(i))) == 0
    data <- data[complete, ]
  }
  m <- nrow(data)
  if(!m > 0) stop(gettext('No usable observations in "data"!'))
  nam <- names(data)
  
  # caluclating chronbach's alpha, item-total, & interitem correlations
  data <- as.matrix(data)
  total <- rowMeans(data)     # scale scores
  mu <- colMeans(data)        # item means
  x_mu <- t(data) - mu        # item-from-mean deviations
  alpha <- item_total_cors <- interitem_cors <- list()
  # with item removed -- maybe give option "if.item.removed == TRUE"?
  ind <- 1L
  if(if.item.removed == TRUE){
    ind <- n+1
    for(i in 1:n){
      # alpha
      v     <- tcrossprod(x_mu[-i, , drop = FALSE])/(m-1) # item (co)variance
      v_bar <- mean(diag(v))                    # mean item variance
      c_bar <- v[row(v) > col(v)]               # item covariances
      c_bar <- mean(c_bar)                      # average item covariance
      alpha[[i]] <- ((n-1)*c_bar)/(v_bar+(n-2)*c_bar)
      # item-total correlation
      tmp <- cor(data[, -i], total)
      item_total_cors[[i]] <- mean(tmp)
      # interitem correlation
      tmp <- cor(data[, -i])
      interitem_cors[[i]] <- mean(tmp[row(tmp) > col(tmp)])
    }
    if.item.removed <- data.frame(
      alpha = do.call(c, alpha)[1:n],
      item.total.r = do.call(c, item_total_cors)[1:n],
      inter.item.r = do.call(c, interitem_cors)[1:n]
    )
  }
  # all items
  # alpha
  v     <- tcrossprod(x_mu)/(m-1) # item (co)variance
  v_bar <- mean(diag(v))                    # mean item variance
  c_bar <- v[row(v) > col(v)]               # item covariances
  c_bar <- mean(c_bar)                      # average item covariance
  alpha[[ind]] <- (n*c_bar)/(v_bar+(n-1)*c_bar)
  if(any(c_bar < 0)){
    warning("Some inter-item correlations are negative. Check coding!")
  }
  f_l <- qf(1-.05/2, m-1, (m-1)*(n-1))
  f_u <- qf(.05/2, m-1, (m-1)*(n-1))
  ci_l <- 1-(1-alpha[[ind]])*f_l
  ci_u <- 1-(1-alpha[[ind]])*f_u
  
  # item-total & interitem correlations
  tmp <- cor(data, total)
  item_total_cors[[ind]] <- mean(tmp)
  tmp <- cor(data)
  interitem_cors[[ind]] <- mean(tmp[row(tmp) > col(tmp)])
  list(
    alpha = data.frame(
      alpha = alpha[[ind]],
      ci.lower = ci_l,
      ci.upper = ci_u
    ),
    item.total.r = item_total_cors[[ind]],
    inter.item.r = interitem_cors[[ind]],
    if.item.removed = if.item.removed
  )
}
