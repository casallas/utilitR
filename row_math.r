#' Gets the norm of each of the rows of a matrix or data frame
#' @param vec_cols a matrix or data.frame containing numeric vectors in each row
row_norms <- function(vec_cols) sqrt(rowSums(vec_cols^2))

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

#' Distance between a point "p", and a line (l.n, l.p)
#' @see https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line#Vector_formulation
#' @param p_cols a matrix or data.frame containing a point in each row
#' @param l.p_cols a matrix or data.frame containing a point along the line in each row
#' @param l.n_cols a matrix or data.frame containing the line's unit vector in each row
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
row_pt_line_proj_distances <- function(p_cols, l.p_cols, l.n_cols){
  abs(row_dot_prods(p_cols - l.p_cols, l.n_cols))
}
