heatColors <- function(x, col, out = "hex", na = "#FFFFFF") {
  if(all(grepl("#", col))) {
    col <- col2rgb(col)
  }
  
  if(grepl("#", na)) {
    na <- col2rgb(na)
  }
  
  if(nrow(col) != 3) stop("'col' must be a 3 x ncolor matrix")
  if(nrow(na) != 3) stop("'na' must be a 3 x 1 matrix")
  
  lx <- length(x)
  nc <- ncol(col)
  qx <- quantile(x, probs = seq(0, 1, 1 / (nc - 1)), na.rm = TRUE)
  dupqx <- duplicated(qx)
  RGB <- matrix(0, nrow = nc, ncol = lx, dimnames = list(c("red", "green", "blue"), NULL))
  
  if(any(dupqx)) qx[dupqx] <- qx[dupqx] + .Machine$double.eps
  
  for(i in seq_len(lx)) {
    if(is.na(x[i])) {
      RGB[, i] <- na[, 1]
    } else {
      for(j in seq_len(nc - 1)) {
        if(x[i] == qx[j]) {
          RGB[, i] <- col[, j]
        } else if(x[i] > qx[j] & x[i] < qx[j + 1]) {
          RGB[, i] <- col[, j] + (col[, j + 1] - col[, j]) * (x[i] - qx[j]) / (qx[j + 1] - qx[j])
        }
      }
      if(x[i] == qx[nc]) {
        RGB[, i] <- col[, nc]
      }
    }
  }
  if(out == "hex") {
    return(rgb(RGB[1, ], RGB[2, ], RGB[3, ], maxColorValue = 255))
  } else {
    return(RGB)
  }
}

# x <- matrix(
#   c(
#     1, 2, 2.1, 2.2, 2.3, 3, 3.1, 3.2, 3.3, 4, 5,
#     1, 2, 2.2, 2.4, 2.6, 3, 3.2, 3.4, 3.6, 4, 5,
#     1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
#   ),
#   nrow = 11
# )
# 
# # try with hex
# pal <- apply(x, 2, heatColors, col = c("#F8696B", "#FFEB84", "#63BE7B"))
# # try with rgb
# pal <- apply(x, 2, heatColors, col = col2rgb(c("#F8696B", "#FFEB84", "#63BE7B")))
# 
# image(
#   x = seq_len(nrow(pal)),
#   y = seq_len(ncol(pal)),
#   z = matrix(seq_along(x), ncol = ncol(x)),
#   col = pal
# )
