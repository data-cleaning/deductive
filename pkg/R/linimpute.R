



#' Impute values derived from linear (in)equality restrictions.
#'
#' @param dat an R object carrying data
#' @param x an R object carrying validation rules
#' @param ... arguments to be passed to other methods.
#' @export
setGeneric("impute_ler", function(dat, x,...) standardGeneric("impute_ler"))


#' @rdname impute_lr
setMethod("impute_lr", c("data.frame","validator"), function(dat, x, ...){
  lc <- x$linear_coefficients()
  X <- t(dat[colnames(lc$A)])
  if (!is.numeric(X)){
    stop("Linear restrictions on nonnumeric data")
  }
  X <- pivimpute(A=lc$A, b=lc$b, x = X)
  dat[rownames(X)] <- t(X)
  dat
})



#### Implementations -----

# 
# A: m x n 
# b: m x 1
# x: n x k
#
# Attempt to impute empty values in the columns of x using the
# pseudoinverse method.
pivimpute <- function(A, b, x, eps=1e-8){
  # note: embarisingly parallel loop
  for ( i in seq_len(ncol(x))){
    miss <- is.na(x)
    if (!any(miss)) next
    Am <- A[,miss,drop=FALSE]
    Ami <- lintools::pinv(Am)
    Id <- diag(1,nrow(Ami))
    C <- abs(Id - Ami %*% Am)
    J <- rowSums( C < eps) == ncol(C)
    v <- Ami%*%(b - A[,t(!miss),drop=FALSE]%*%x[!miss,i,drop=FALSE])
    x[which(miss)[J] , i] <- v[J]
  }
  x
}















  