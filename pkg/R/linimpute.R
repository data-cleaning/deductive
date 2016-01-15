



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
impute_implied <- function(A, b, ops, x, eps=1e-8){
  missing <- is.na(x)
  L <- list(A=A,b=b)
  prev_nmiss <- Inf 
  curr_nmiss <- sum(missing) 

  while (prev_nmiss > curr_nmiss){
    prev_nmiss <- curr_nmiss
    # substitute observed values
    L <- lintools::subst_value(
      A = A
      , b = b
      , variables = !missing
      , values = x[!missing]
    )
    
    #
    iA <- abs(L$A) > eps
    isingle <- rowSums(iA>eps) == 1 
    
    # replace single-variable equalities
    ieq <- isingle & ops == "=="
    varindex <- apply(iA[ieq,,drop=FALSE],1,which)
    x[varindex] <- L$b[isingle] / L$A[cbind(which(isingle),varindex)]

    # single-variable inequalities
    ileq <- which(isingle & ops == "<=")
    # construct normalized 
    ba <- abs(L$b[ileq])
    i <- ba<eps
    ba[i] <- 0
    Aa <- L$A[ileq,,drop=FALSE]/(ba + i) # divide by 1 if ba is numerically zero
    
    # compute pair locations
    I <- rep(ileq, times=length(ileq))
    J <- rep(ileq, each=length(ileq))
    # avoid double work
    ii <- I<J
    I <- I[ii]
    J <- J[ii]
    # actual comparison
    ipairs <- which( rowSums(abs(Aa[I,,drop=FALSE] + Aa[J,,drop=FALSE])) < eps )
    ipairs <- I[ipairs]
    
    iA <- iA[ileq,,drop=FALSE]
    varindex <-  apply(iA[ipairs,,drop=FALSE],1,which)   
    x[varindex] <- ba[ipairs] / Aa[cbind(ipairs,varindex)]
    
    missing <- is.na(x)
    curr_nmiss <- sum(missing)
    
  }
    
  x
}


#debugonce(impute_implied)


#  library(validate)
#  v <- validator(x + 2*y == 3,   x + y + z == 7)
#  L <- v$linear_coefficients()
#  x_ <- c(1,NA,NA)
#  impute_implied(L$A, L$b, L$operators, x_)
#  
#  x_ <- c(1,1,NA)
#  lintools::subst_value(L$A,L$b,!is.na(x_),x_[!is.na(x_)])
#  
#  debugonce(impute_implied)






