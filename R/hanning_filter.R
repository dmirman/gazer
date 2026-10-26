#' Perfroms hanning smoothing on pupil data
#' @param pupil column with pupil values 
#' @param degree default of 11.
#' @return vector containing hanning filtered pupil data
#' @export
hanning_filter <- function(pupil, degree=11){
    i <- 0:(degree - 1)
    w <- 0.5 - 0.5 * cos((2 * pi * i)/(degree - 1))
    w <- w/sum(w)
    # class(w) <- 'Ma'
    y <- as.vector(stats::filter(pupil, w))
    # Fill the filter's edge NAs; needs at least two valid values
    if (sum(!is.na(y)) >= 2) y <- zoo::na.approx(y, rule=2)
    return (y)
  }
  
  
  