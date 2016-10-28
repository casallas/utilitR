#' The functions below only consider quaternions that represent rotations
#' (unit quaternions).
#' w = cos(theta/2), v = u*sin(theta/2)
#' Typically quaternions are written [w, (i, j, k)], but we instead follow
#' OSG's convention [(i, j, k), w] for reasons of compatibility
#' Bear this in mind when comparing to packages like onion

#' Constructs a quaternion from its imaginary vector (i,j,k) and its real scalar (w)
quat <- function(vec, w) c(vec, w)

#' Gets the real (w) scalar component of a quaternion
q_w <- function(q) q[4]

#' Gets the imaginary (i,j,k) components of a quaternion in the form of a vector
q_vec <- function(q) q[1:3]


#' Quaternion or "Hamilton" product
q_product <- function(q1, q2){quat(
  vec=q_w(q1)*q_vec(q2) + q_w(q2)*q_vec(q1) + vec_cross_prod(q_vec(q1), q_vec(q2)),
  w=q_w(q1)*q_w(q2) - vec_dot_prod(q_vec(q1), q_vec(q2))
)}

#' The conjugate q* of a quaternion
q_conj <- function(q) quat(vec=-q_vec(q), w=q_w(q))

#' Gets the angle and axis of the rotation represented by quat
#' Implementation translated from osg::getRotate
quat2angle_axis <- function(q){
  sinhalfangle <- vec_norm(q_vec(q))
  angle <- 2 * atan2( sinhalfangle, q_w(q) )
  if(!sinhalfangle) axis <- c(0,0,1)
  if(sinhalfangle) axis <- q_vec(q)/sinhalfangle
  list(angle = angle, axis = axis)
}

#' Constructs a quaternion based on a rotation angle and the rotation axis
angle_axis2quat <- function(angle, axis){
  quat(vec=unit_vec(axis)*cos(angle/2), w=sin(angle/2))
}
