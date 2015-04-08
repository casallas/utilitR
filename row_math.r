#' Gets the norm of each of the rows of a matrix or data frame
#' @param vec_cols a matrix or data.frame containing numeric vectors in each row
row_norms <- function(vec_cols) sqrt(rowSums(vec_cols^2))

#' Gets the squared norm of each of the rows of a matrix or data frame
#' @param vec_cols a matrix or data.frame containing numeric vectors in each row
row_norms2 <- function(vec_cols) rowSums(vec_cols^2)

#' Gets the unit vector of each of the rows of a matrix or data frame
#' @param vec_cols a matrix or data.frame containing numeric vectors in each row
row_uvecs <- function(vec_cols) vec_cols/row_norms(vec_cols)

#' Gets the geometric distances between the rows of two matrices or data frames
#' @param p1_cols a matrix or data.frame containing numeric vectors in each row
#' @param p2_cols a matrix or data.frame containing numeric vectors in each row
row_distances <- function(p1_cols, p2_cols) row_norms(p1_cols - p2_cols)

#' Gets the dot products between the rows of two matrices or data frames
#' @param v1_cols a matrix or data.frame containing numeric vectors in each row
#' @param v2_cols a matrix or data.frame containing numeric vectors in each row
row_dot_prods <- function(v1_cols, v2_cols){
  rowSums(v1_cols * v2_cols)
  # Alternative implementation: diag(as.matrix(v1_cols) %*% t(v2_cols))
}

#' Calculates the cross product between a and b
#' Examples
#' row_cross_prods(rbind(c(3,-3,1), c(3, -3, 1)), rbind(c(4,9,2), c(-12, 12, -4))) # rbind(c(-15, -2, 39), c(0, 0, 0))
row_cross_prods <- function(a, b){
  cbind(a[,2]*b[,3] - a[,3]*b[,2],
   -a[,1]*b[,3] + a[,3]*b[,1],
    a[,1]*b[,2] - a[,2]*b[,1]
  )
}

#' Rotates vector vec by quaternion quat
#' translated to R from osg::Quat::operator*(osg::Vec3)
row_rotateq <- function(vec_cols, quat_cols){
  # Subset the first 3 elements of quat to calculate the cross products with vec
  quat3_cols <- quat_cols[,1:3]
  uvec_cols  <- row_cross_prods(quat3_cols, vec_cols)
  uuvec_cols <- row_cross_prods(quat3_cols, uvec_cols)
  uvec_cols  <- uvec_cols*( 2*quat_cols[,4] )
  uuvec_cols <- uuvec_cols * 2
  vec_cols + uvec_cols + uuvec_cols
}


#' Distance between a point "p", and a line (l.n, l.p)
#' @see https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line#Vector_formulation
#' @param p_cols a matrix or data.frame containing a point in each row
#' @param l.p_cols a matrix or data.frame containing a point along the line in each row
#' @param l.n_cols a matrix or data.frame containing the line's unit vector in each row
#' @examples
#' # 2D Taken from http://www.vitutor.com/geometry/line/point_line.html
#' p <- rbind(c(x=2, y=-1), c(x=0, y=0))
#' l.p <- rbind(c(x=1, y=-3/4), c(x=3, y=-4))
#' l.n <- row_uvecs(rbind(c(x=1, y=-3/4), c(x=4, y=3)))
#' row_pt_line_distances(p, l.p, l.n) # .4, and 5
#' # 3D Taken from http://onlinemschool.com/math/library/analytic_geometry/p_line/
#' # and http://math.harvard.edu/~ytzeng/worksheet/distance.pdf
#' p <- rbind(c(x=0, y=2, z=3), c(x=2, y=3, z=1))
#' l.p <- rbind(c(x=3, y=1, z=-1), c(x=1, y=1, z=2))
#' l.n <- row_uvecs(rbind(c(x=2, y=1, z=2), c(x=5, y=0, z=1)))
#' row_pt_line_distances(p, l.p, l.n) # 5, and ~2.32
row_pt_line_distances <- function(p_cols, l.p_cols, l.n_cols){
  row_norms(
    (p_cols - l.p_cols) -
    (row_dot_prods(p_cols - l.p_cols, l.n_cols)*l.n_cols)
  ) # /row_norms
}

#' Distance between the projection of point "p" on line (l.n, l.p), and point "l.p"
#' @see https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line#Vector_formulation
#' @param p_cols a matrix or data.frame containing a point in each row
#' @param l.p_cols a matrix or data.frame containing a point along the line in each row
#' @param l.n_cols a matrix or data.frame containing the line's unit vector in each row
#' @examples
#' # Based on the row_pt_line_distances examples
#' # 2D
#' p <- rbind(c(x=2, y=-1), c(x=0, y=0))
#' l.p <- rbind(c(x=1, y=-3/4), c(x=3, y=-4))
#' l.n <- row_uvecs(rbind(c(x=1, y=-3/4), c(x=4, y=3)))
#' all.equal(
#'   sqrt(
#'     row_pt_line_distances(p, l.p, l.n)^2 +
#'     row_pt_line_proj_distances(p, l.p, l.n)^2
#'   ), #/row_norms
#'   row_distances(p, l.p)
#' ) # /all.equal
#' # 3D
#' p <- rbind(c(x=0, y=2, z=3), c(x=2, y=3, z=1))
#' l.p <- rbind(c(x=3, y=1, z=-1), c(x=1, y=1, z=2))
#' l.n <- row_uvecs(rbind(c(x=2, y=1, z=2), c(x=5, y=0, z=1)))
#' all.equal(
#'   sqrt(
#'     row_pt_line_distances(p, l.p, l.n)^2 +
#'     row_pt_line_proj_distances(p, l.p, l.n)^2
#'   ), #/row_norms
#'   row_distances(p, l.p)
#' ) # /all.equal
row_pt_line_proj_distances <- function(p_cols, l.p_cols, l.n_cols){
  abs(row_dot_prods(p_cols - l.p_cols, l.n_cols))
}

#' Gets the scalar projections of the rows of v_cols on the rows of k_cols
#' row_scalar_projs(data.frame(x=1:4,-2:1), data.frame(-1:2,0:3)) # -1 -1  1.341641  3.050851
row_scalar_projs <- function(v_cols, k_cols){
  row_dot_prods(v_cols, row_uvecs(k_cols))
}

#' Gets the vector projections of the rows of v_cols on the rows of k_cols
row_projs <- function(v_cols, k_cols){
  k_cols <- row_uvecs(k_cols)
  k_cols*row_dot_prods(v_cols, k_cols)
}

#' Gets the vector rejections of the rows of v_cols on the rows of k_cols
row_rejs <- function(v_cols, k_cols){
  v_cols - row_projs(v_cols, k_cols)
}
