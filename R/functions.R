#' Compute the inner product of two three-dimensional tensors.
#'
#' @param X A three-dimensional tensor.
#' @param Y An optional three-dimensional tensor to take the inner
#' product with. If not provided, defaults to \code{X}.
#' @return The inner product of \code{X} and \code{Y}.
#' @details The norm of a three-dimensional tensor is defined as the
#' square root of the sum of squares of all elements. This function
#' computes the inner product of two three-dimensional tensors,
#' analogous to the Frobenius norm for matrices.
#' @examples
#' X <- array(1:27, c(3, 3, 3))
#' tensor_ip(X) # returns 3555
tensor_ip <- function(X, Y = X) {
  return(sum(X * Y))
}

#' Tensor-based Higher Order Singular Value Decomposition (HOSVD)
#'
#' This function performs the Tensor-based Higher Order Singular Value Decomposition
#' (HOSVD) on a given tensor \code{X} using a specified number of ranks \code{num_ranks}.
#'
#' @param X A tensor to be decomposed.
#' @param num_ranks A vector specifying the number of ranks for each mode of \code{X}.
#'
#' @return A list containing:
#' \item{G}{A core tensor.}
#' \item{U}{A list of factor matrices for each mode of \code{X}.}
#' \item{X_hat}{The estimated tensor using the HOSVD.}
#' \item{resid}{The Frobenius norm of the difference between \code{X} and \code{X_hat}.}
#'
#' @import tensorFun
#' @importFrom stats svd
#'
#' @examples
#' X <- array(1:24, dim=c(2,3,4))
#' tnsr_hosvd(X, num_ranks=c(2,2,2))
#'
#' @seealso \code{\link{tensor}}, \code{\link{tensor_ip}}
#'
#' @export
tnsr_hosvd <- function(X, num_ranks) {
  num_modes <- length(dim(X))
  U_list <- vector("list", num_modes)
  
  # Factor Matrices
  for (m in 1:num_modes) {
    temp_mat <- tensorFun::unfold(X, m)
    U_list[[m]] <- svd(temp_mat, nu = num_ranks[m])$u
  }
  
  # Core Tensor
  G <- X
  for (n in 1:num_modes) {
    G <- tensor(G, t(U_list[[n]]), alongA = 1, alongB = 2)
  }
  
  # Estimate
  X_hat <- G
  for (o in 1:num_modes) {
    X_hat <- tensor(X_hat, U_list[[o]], alongA = 1, alongB = 2)
  }
  
  # Compute residual and return results
  fnorm_X <- sqrt(tensor_ip(X - X_hat))
  return(list(G = G, U = U_list, X_hat = X_hat, resid = fnorm_X))
}



# final stat tensor is X
#' find the HOSVD for a tensor with a fixed time dimension
hosvd2 <- function(X, country_set, econ_set) {
  num_modes <- length(dim(X))
  U_list <- vector("list", num_modes)
  
  num_ranks <- c(163, country_set, econ_set)
  
  # Factor Matrices
  for (m in 1:num_modes) {
    temp_mat <- tensorFun::unfold(X, m)
    U_list[[m]] <- svd(temp_mat, nu = num_ranks[m])$u
  }
  
  # Core Tensor
  G <- X
  for (n in 1:num_modes) {
    G <- tensor(G, t(U_list[[n]]), alongA = 1, alongB = 2)
  }
  
  # Estimate
  X_hat <- G
  for (o in 1:num_modes) {
    X_hat <- tensor(X_hat, U_list[[o]], alongA = 1, alongB = 2)
  }
  return(sqrt(tensor_ip(X - X_hat)))
}