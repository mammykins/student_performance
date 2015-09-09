#This function takes a vector x of numeric values, and for each value in x
#subtract the min value in x and divide by the range of values in x.
#The following vector is returned.

normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
#test
#normalise(1:5)
#normalise(seq(0, 1, 0.25))