
#' Moore-Penrose pseudoinverse
#'
#' Compute the pseudoinverse of a matrix using the
#' SVD-construction 
#'
#' @param A a matrix
#' @param eps tolerance for determining zero singular values
#'
#' @keywords internal
pinv <- function(A, eps=1e-8){
  L <- svd(A)
  d <- L$d
  i <- abs(d) > eps
  d[i] <- 1/d[i]
  L$v %*% diag(d) %*% t(L$u)
}



#' Impute values derived from linear equality restrictions.
#'
#' @param dat an R object carrying data
#' @param x an R object carrying validation rules
#' @param ... arguments to be passed to other methods.
#' @export
setGeneric("impute_ler", function(dat, x,...) standardGeneric("impute_ler"))


#' @rdname impute_ler
setMethod("impute_ler", c("data.frame","validator"), function(dat, x, ...){
  x
  dat
})











  