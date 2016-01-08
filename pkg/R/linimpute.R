



#' Impute values derived from linear (in)equality restrictions.
#'
#' @param dat an R object carrying data
#' @param x an R object carrying validation rules
#' @param ... arguments to be passed to other methods.
#' @export
setGeneric("impute_lr", function(dat, x,...) standardGeneric("impute_lr"))


#' @rdname impute_lr
setMethod("impute_lr", c("data.frame","validator"), function(dat, x, ...){
  eps <- 1e-8 # TODO: need to integrate with validate::voptions
  lc <- x$linear_coefficients()
  X <- t(dat[colnames(lc$A)])
  if (!is.numeric(X)){
    stop("Linear restrictions on nonnumeric data")
  }
  attr(X,"changed") <- FALSE
  X <- pivimpute(A=lc$A, b=lc$b, ops=lc$operators, x = X, eps=eps)
  X <- zeroimpute(A=lc$A,b=lc$b, ops=lc$operators, x = X, eps=eps)
  while ( attr(X,"changed") ){
    X <- pivimpute(A=lc$A, b=lc$b, ops=lc$operators, x = X, eps=eps)
    X <- zeroimpute(A=lc$A,b=lc$b, ops=lc$operators, x = X, eps=eps)
  }
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
pivimpute <- function(A, b, ops, x, eps=1e-8){
  changed <- FALSE
  for ( col in seq_len(ncol(x)) ){
    x_ <- x[,col,drop=FALSE]
    miss <- is.na(x_)
    if (!any(miss)) next
    eq <- ops == "=="
    Am <- A[eq,miss,drop=FALSE]
    Ami <- lintools::pinv(Am)
    Id <- diag(1,nrow(Ami))
    C <- abs(Id - Ami %*% Am)
    J <- rowSums( C < eps) == ncol(C)
    v <- Ami%*%(b[eq,,drop=FALSE] - A[eq,t(!miss),drop=FALSE]%*%x_[!miss,drop=FALSE])
    x_[which(miss)[J]] <- v[J]
    changed <- changed | any(J)
    x[,col] <- x_
  }
  attr(x,"changed") <- changed
  x
}


is_gt_zero_constraint <- function(A, b,ops,eps){
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
  nonneg <- is_gt_zero_constraint(A,b,ops,eps)
  eq <- ops == "=="
  storage.mode(A) <- "double"
  storage.mode(b) <- "double"
  storage.mode(x) <- "double"
  storage.mode(eps) <- "double"
  changed <- attr(x,"changed")
  x <- .Call("R_imputezero",A[eq,,drop=FALSE],b[eq,,drop=FALSE], x, nonneg, eps)
  attr(x,"changed") <- attr(x,"changed") | changed
  x
}

# implied values
# impute values implied by two simple inequalities
#










  