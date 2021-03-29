



#' Impute values derived from linear (in)equality restrictions.
#'
#' Partially filled records \eqn{\boldsymbol{x}} under linear (in)equality
#' restrictions may reveal unique imputation solutions when the system
#' of linear inequalities is reduced by substituting observed values.
#' This function applies a number of fast heuristic methods before
#' deriving all variable ranges and unique values using Fourier-Motzkin
#' elimination.
#'
#'
#' @param dat an R object carrying data
#' @param x an R object carrying validation rules
#' @param methods What methods to use. Add 'fm' to also compute variable ranges
#'        using fourier-motzkin elimination (can be slow and may use a lot of memory).
#'
#' @param ... arguments to be passed to other methods.
#'
#' @note
#' The Fourier-Motzkin elimination method can use large amounts of memory and
#' may be slow. When memory allocation fails for a ceratian record, the method
#' is skipped for that record with a message. This means that there may be
#' unique values to be derived but it is too computationally costly on the
#' current hardware.
#' 
#'
#' @examples
#'
#' v <- validate::validator(y ==2,y + z ==3, x +y <= 0)
#' dat <- data.frame(x=NA_real_,y=NA_real_,z=NA_real_)
#' impute_lr(dat,v)
#' 
#' @export
setGeneric("impute_lr", function(dat, x,...) standardGeneric("impute_lr"))


#' @rdname impute_lr
setMethod("impute_lr", c("data.frame","validator"), function(dat, x, methods=c("zeros","piv","implied"), ...){
  # iterate over blocks of independent rule sets
  exprs  <- x$exprs(lin_ineq_eps=0, lin_eq_eps=0, expand_groups=TRUE, expand_assignments=TRUE)
  rules  <- do.call("validator", exprs)
  
  if (identical(methods,"all")){
    methods <- c("zeros","piv","implied","fm")
  }

  impute_lr_work(dat, rules, methods,...)
})

impute_lr_work <- function(dat, x, methods,...){
  eps <- 1e-8 # TODO: need to integrate with validate::voptions



  # skip cases where all variables are missing
  i_skip <- rowSums(is.na(dat))==ncol(dat)
  if (all(i_skip)) return(dat)


  lc <- x$linear_coefficients()
  ops <- lc$operators
  lc <- lintools::normalize(lc$A,lc$b,lc$operators)
  lc$operators <- ops[lc$order]
  
  X <- t(dat[!i_skip, colnames(lc$A),drop=FALSE])
  if (!is.numeric(X)){
    stop("Linear restrictions on nonnumeric data")
  }
  
  attr(X,"changed") <- TRUE
  while ( attr(X,"changed") ){
    if("piv" %in% methods){    
      X <- pivimpute(A=lc$A, b=lc$b, ops=lc$operators, x = X, eps=eps)
    }
    if ("zeros" %in% methods){
      X <- zeroimpute(A=lc$A,b=lc$b, ops=lc$operators, x = X, eps=eps)
    }
    if ("implied" %in% methods){
      X <- impute_implied(A = lc$A, b=lc$b, ops=lc$operators, x = X, eps=eps)
    }
    if (!any(c("piv","implied","zeros") %in% methods)){
      attr(X,"changed") <- FALSE
    }
  }
  # Impute by determining implied variable ranges.
  #
  if ("fm" %in% methods){
    X <- impute_range(A=lc$A,b=lc$b, x = X, ops=lc$operators, eps = eps)
  }
  dat[!i_skip, rownames(X)] <- t(X)
  dat
}

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
  if (any(ops=="==")){ #prevent svd on matrix of dimension 0
    for ( col in seq_len(ncol(x)) ){
      x_ <- x[,col,drop=FALSE]
      miss <- is.na(x_)
      if (!any(miss)) next
      eq <- ops == "=="
      Am <- A[eq,miss,drop=FALSE]
      Ami <- lintools::pinv(Am)
      Id <- diag(1,nrow(Ami))
      C <- abs(Id - Ami %*% Am)
      J <- rowSums(C < eps) == ncol(C)
      v <- Ami%*%(b[eq] - A[eq,!miss,drop=FALSE]%*%x_[!miss,drop=FALSE])
      x_[which(miss)[J]] <- v[J]
      changed <- changed | any(J)
      x[,col] <- x_
    }
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
  nonneg_var <- colSums(abs(A[nonneg,,drop=FALSE])) > eps
  eq <- ops == "=="
  storage.mode(A) <- "double"
  storage.mode(b) <- "double"
  storage.mode(x) <- "double"
  storage.mode(eps) <- "double"
  x <- .Call("R_imputezero",A[eq,,drop=FALSE],b, x, nonneg_var, eps)
  changed <- if ( is.null(attr(x,"changed")) ) FALSE else attr(x,"changed")
  attr(x,"changed") <- changed
  x
}




# implied values
# impute values implied by two simple inequalities

impute_implied <- function(A, b, ops, x, eps=1e-8){
  nna <- sum(is.na(x))
  x <- apply(x,2,impute_implied_x, A=A, b=b, ops=ops, eps=eps)
  attr(x,"changed") <- nna > sum(is.na(x))
  x
}


impute_implied_x <- function(A, b, ops, x, eps=1e-8){
  missing <- is.na(x)
  prev_nmiss <- Inf 
  curr_nmiss <- sum(missing) 

  while (nrow(A) > 0 && prev_nmiss > curr_nmiss){
    prev_nmiss <- curr_nmiss
    # substitute observed values
    L <- lintools::subst_value(
      A = A
      , b = b
      , variables = !missing
      , values = x[!missing]
    )
    L <- lintools::compact(A=L$A, b=L$b, x=NULL,
        								  , neq               = sum(ops=="==")
        								  , nleq              = sum(ops=="<=")
        								  , remove_columns    = FALSE
        								  , remove_rows       = TRUE
        								  , deduplicate       = TRUE
        								  , implied_equations = TRUE)
    A <- L$A
    b <- L$b
    ops <- c(rep("==", L$neq)
           , rep("<=", L$nleq)
           , rep("<", nrow(L$A)-L$neq-L$nleq) )

    iA <- abs(A) > eps
    isingle <- rowSums(iA) == 1 
    
    # replace single-variable equalities
    ieq <- isingle & ops == "=="
    varindex <- apply(iA[ieq,,drop=FALSE],1,which)
    x[varindex] <- b[ieq] / A[cbind(which(ieq),varindex)]

    missing <- is.na(x)
    curr_nmiss <- sum(missing)
    
  }
    
  x
}




# loop over a numeric array
impute_range <- function(A, b, x, ops, eps=1e-8){
  neq <- sum(ops=="==")
  nleq <- sum(ops == "<=")
  apply(x,2,impute_range_x,A=A,b=b,neq=neq,nleq=nleq,eps=eps)
}

# impute by deriving variable ranges (may be computationally expensive)
impute_range_x <- function(x,A,b,neq, nleq,eps=1e-8){
  obs <- !is.na(x)
  if (all(obs)) return(x)
  tryCatch({
    L <- lintools::subst_value(A=A,b=b,variables=obs, values=x[obs])
    R <- lintools::ranges(A=L$A,b=L$b,neq=neq,nleq=nleq,eps=eps)
    i <- (R[ ,"upper"] - R[ ,"lower"] < eps) & ( R[,"lower"] <= R[,"upper"] )
    i[!is.finite(i)] <- FALSE
    x[i] <- R[i,"upper"]
  }, error=function(e){
    message(sprintf("FM method skipped because %s", e$message))
  })
  x
}
