#' Gets the norm of each of the rows of a matrix or data frame
#' @param vec_cols a matrix or data.frame containing numeric vectors in each row
row_norms <- function(vec_cols) sqrt(rowSums(vec_cols^2))

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
