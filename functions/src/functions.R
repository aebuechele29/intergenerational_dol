efficient_min <- function(x, na.rm = TRUE) {

  if (all(is.na(x))) return(x[1])
  return(min(x, na.rm = na.rm))

}

efficient_max <- function(x, na.rm = TRUE) {

  if (all(is.na(x))) return(x[1])
  return(max(x, na.rm = na.rm))

}

efficient_mean <- function(x, na.rm = TRUE) {

  if (all(is.na(x))) return(x[1])
  return(mean(x, na.rm = na.rm))

}

efficient_sum <- function(x, na.rm = TRUE) {

  if (all(is.na(x))) return(x[1])
  return(sum(x, na.rm = na.rm))

} 