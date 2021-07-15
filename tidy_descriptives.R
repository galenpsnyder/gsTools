# this is a tidy function for calculating descriptive 
# statistics across one or more variables in a dataset. 
# the function takes as its input a dataframe, the variables
# over which to calculate descriptive statistics, and the 
# desired statistics to calculate.
# the function outputs a dataframe with one column containing
# variable names and one column for each descriptive statistic.
# if no variables are specified, everything() is the default.
# if no statistics are specified, mean and sd are the default.

tidy_descriptives <- function(.data, .cols, .fns, ...){
  if(missing(.cols)){
    .cols <- substitute(everything())
  } else {
    .cols <- substitute(.cols)
  }
  
  if(missing(.fns)){ 
    .fns <- substitute(list(mean = mean, sd = sd))
  } else {
    .fns <- substitute(.fns)
  }
  
  if(is_grouped_df(.data)){
    n_groups <- seq_along(groups(.data))
    pivot_cols <- substitute(-n_groups)
  } else {
    pivot_cols <- substitute(everything())
  }
  
  name_key <- paste(sample(letters, 3), collapse = "")
  name_key <- paste0("_", name_key, "_")
  .names <- paste0("{.col}", name_key, "{.fn}")
  
  .data%>%
    summarise(
      across(
         .cols = !!.cols,
         .fns = !!.fns,
         .names = .names
      )
    )%>%
    pivot_longer(
      cols = !!pivot_cols,
      names_to = "var_fun",
      values_to = "val"
    )%>%
    separate(
      col = "var_fun",
      into = c("var", "fun"),
      sep = name_key
    )%>%
    pivot_wider(
      id_cols = -c("fun", "val"),
      names_from = "fun",
      values_from = "val"
    )
}
