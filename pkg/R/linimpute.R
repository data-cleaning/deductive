



#' Impute values derived from linear (in)equality restrictions.
#'
#' @param dat an R object carrying data
#' @param x an R object carrying validation rules
#' @param ... arguments to be passed to other methods.
#' @export
setGeneric("impute_lr", function(dat, x,...) standardGeneric("impute_lr"))


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

# In
# A: m x n 
# b: m x 1
# x: n x k
#
# Out:
# x, with missings filled in where possible 
#
# Attempt to impute empty values in the columns of x using the
# pseudoinverse method.
pivimpute <- function(A, b, x, eps=1e-8){
  miss <- is.na(x)
  if (!any(miss)) next
  Am <- A[,miss,drop=FALSE]
  Ami <- lintools::pinv(Am)
  Id <- diag(1,nrow(Ami))
  C <- abs(Id - Ami %*% Am)
  J <- rowSums( C < eps) == ncol(C)
  v <- Ami%*%(b - A[,t(!miss),drop=FALSE]%*%x[!miss,drop=FALSE])
  x[which(miss)[J]] <- v[J]
  x
}


is_gt_zero_constraint <- function(A, b,ops){
  sapply(seq_len(nrow(A)),function(i){
    a <- A[i,]
    j <- which(a < -eps)
    b[i] == 0 &&  length(j) == 1 && ops[i] == "<"
  })
}

# Impute zeros when possible
#
# Normalized system of equations Ax = b, meaning that all
# inequations are written in the < or <= form.
#
zeroimpute <- function(A, b, ops, x, eps=1e-8){
  gt_zero <- is_gt_zero_constraint(A,b,ops)
  eq <- ops = "=="
  #todo: C-implementation
}

# implied values
# impute values implied by two simple inequalities
#










  