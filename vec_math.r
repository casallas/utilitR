#' Calculates the norm of a vector
vec_norm <- function(vec) sqrt(sum(vec^2))

#' Calculates the distance between two points
vec_distance <- function(p1, p2) vec_norm(p1 - p2)
