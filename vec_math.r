#' Calculates the norm of a vector
vec_norm <- function(vec) sqrt(sum(vec^2))

#' Calculates the distance between two points
vec_distance <- function(p1, p2) vec_norm(p1 - p2)

#' Gets the unit vector of v
unit_vec <- function(v) v/vec_norm(v)

#' Calculates the dot product between a and b
vec_dot_prod <- function(a, b) sum(a*b)

#' Calculates the cross product between a and b
#' Examples
#' vec_cross_prod(c(3,-3,1),c(4,9,2)) # c(-15, -2, 39)
#' vec_cross_prod(c(3,-3,1),c(-12,12,-4)) # c(0, 0, 0)
vec_cross_prod <- function(a, b){
  c(a[2]*b[3] - a[3]*b[2],
   -a[1]*b[3] + a[3]*b[1],
    a[1]*b[2] - a[2]*b[1]
  )
}

#' Rotates vector v around k by theta using the rodrigues formula
#' @see https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula
#' @param v the vector to be rotated
#' @param k the vector around which v should be rotated
#' @param theta the angle of rotation in radians
#' vec_rotate(c(-1,0,0), c(1,1,0), pi) # c(0, -1, 0)
vec_rotate <- function(v, k, theta){
  k <- unit_vec(k)
  v*cos(theta) +
    vec_cross_prod(k, v)*sin(theta) +
    k*vec_dot_prod(k,v)*(1-cos(theta))
}

#' Gets the scalar projection of vector v on vector k
#' http://www.math.ucla.edu/~ronmiech/Calculus_Problems/32A/chap11/section3/701d37/701_37.html
#' vec_scalar_proj(c(4,1), c(2,3)) # 3.05 # 11/sqrt(13)
vec_scalar_proj <- function(v, k) vec_dot_prod(v, unit_vec(k))

#' Gets the vector projection of v on k
# http://www.math.ucla.edu/~ronmiech/Calculus_Problems/32A/chap11/section3/701d37/701_37.html
#' vec_proj(c(4,1), c(2,3)) # c(1.69, 2.53) # c(22/13, 33/13)
vec_proj <- function(v, k){
  k <- unit_vec(k)
  k*vec_dot_prod(v, k)
}

#' Gets the vector rejection of v on k
#' vec_rej(c(4,1), c(2,3)) # c(1.69, 2.53) # c(22/13, 33/13)
vec_rej <- function(v, k){
  v - vec_proj(v, k)
}
