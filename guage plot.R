guage <- function(data = NULL, condition = NULL, label = NULL, fun = mean, ...)
{
  if(!is.function(fun)) stop(gettext('"fun" must be a function that returns a descriptive statistic!'))
  if(is.null(label)) label <- deparse(substitute(fun))
  # formatting data for guage plot
  user_names <- names(data)
  if(is.null(user_names)) user_names <- colnames(data)
  if(is.null(user_names)){
    calls  <- as.list(match.call())
    calls2 <- deparse(calls[[2]])
    if(grepl("[$]", calls2)) user_names <- unlist(strsplit(calls2, "[$]"))[2]
  }
  if(!is.data.frame(data)) data <- data.frame(data)
  names(data) <- user_names
  first <- try(eval(substitute(condition), data), silent = T)
  if(inherits(first, "try-error")){
    stop(gettext('Names in "condition" must match names in "data"!'))
  }
  dotdotdot <- list(...)
  # formatting names & such
  condition <- deparse(substitute(condition))
  user_names <- user_names[which(sapply(user_names, function(i) grepl(i, condition)))]
  
  # getting summary statistics for guage plot
  data <- data.frame(
    var = user_names,
    val = fun(first, na.rm = T)
  )
  
  # checking for user input for plot specs
  # NOT SURE HOW TO DO THIS YET--VERY BETA AT THIS POINT
  plot_args <- list(
    ymin = (0:2)/3, 
    ymax = (1:3)/3, 
    color = "black", 
    fill = c("red", "yellow", "forestgreen"), 
    size = 1
  )
  user_args <- match(names(dotdotdot), names(plot_args), nomatch = FALSE)
  user_args <- names(plot_args)[user_args[user_args != 0]]
  if(length(user_args)>0){
    for(i in 1:length(user_args)){
      plot_args[[user_args[i]]] <- dotdotdot[[user_args[i]]]
    }
  }
  n_fill <- length(plot_args$fill)
  if(length(plot_args$ymin) < n_fill |
     length(plot_args$ymax) < n_fill){
    plot_args$ymin <- (0:(n_fill-1))/n_fill
    plot_args$ymax <- (1:n_fill)/n_fill
  }
  if(length(plot_args$ymin) > n_fill |
     length(plot_args$ymax) > n_fill){
    stop(gettext('"fill" must be of length 1 or length of y limits!'))
  }
  if(length(plot_args$ymin) != length(plot_args$ymax)){
    stop(gettext("y limits must be of equal length!"))
  }

  # producing guage plot
  df <- data.frame(
    ymin = plot_args$ymin,
    ymax = plot_args$ymax,
    fill = plot_args$fill,
    stringsAsFactors = F)
  p <- ggplot()+
    geom_rect(
      mapping = aes(
        xmin = 1,
        xmax = 2,
        ymin = 0,
        ymax = 1/n_fill
      ),
      color = "black",
      fill = plot_args$fill[1]
    )
  for(i in 2:n_fill){
    tmp <- df[i, ]
    p <- p + geom_rect(
      data = tmp,
      mapping = aes(
        xmin = 1,
        xmax = 2,
        ymin = ymin,
        ymax = ymax
      ),
      color = "black",
      fill = tmp$fill
    )
  }
  p <- p +
    geom_segment(
      data = data,
      mapping = aes(
        x = 0.5,
        xend = 2,
        y = val,
        yend = val
      ),
      size = 1
    )+
    coord_polar(theta = "y", start = -pi/2)+
    xlim(c(0, 2))+
    ylim(c(0, 2))+
    geom_text(
      data = data, mapping = aes(x = 0, y = 0, label = round(val*100, 2))
    )+
    geom_text(data = data, aes(x = 0.5, y = 1.5, label = label))+
    theme_void()
  p
}
guage(data = df%>%distinct(id, .keep_all = T), 
      condition = hire_5 == "Hired on this req" | hire_5 == "Hired on another req", 
      label = "% Hired",
      fun = mean)

      