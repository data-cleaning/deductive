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

  # skip cases where all variables are missing
  i_skip <- rowSums(is.na(dat)) == ncol(dat)
  dat[!i_skip, ] <- impute_lr_work(dat[!i_skip, ], rules, methods,...)
  dat
})

impute_lr_work <- function(dat, x, methods,...){
  eps <- 1e-8 # TODO: need to integrate with validate::voptions

  # If dat is empty, do nothing.
  if (nrow(dat) == 0) {
    return(dat)
  }

  # Find rule violations in dat
  confront_values <- validate::values(validate::confront(dat, x))
  old_violations <- (confront_values == FALSE & !is.na(confront_values))

  lc <- x$linear_coefficients()
  ops <- lc$operators
  lc <- lintools::normalize(lc$A,lc$b,lc$operators)
  lc$operators <- ops[lc$order]

  # X is the transpose of the relevant subset of rows and columns of the data,
  # so the columns of x correspond to rows (records) in the original data.
  X <- t(dat[, colnames(lc$A), drop = FALSE])
  if (!is.numeric(X)){
    stop("Linear restrictions on nonnumeric data")
  }

  # Loop over imputation methods until no more imputations are made.
  all_loop_methods <- list(piv = pivimpute, zeros = zeroimpute, implied = impute_implied)
  loop_method_funcs <- all_loop_methods[methods %in% names(all_loop_methods)]
  done <- FALSE
  while (!done) {
    done <- TRUE
    for (method_func in loop_method_funcs) {
      impute_result <- impute_lr_single(
        func = method_func, dat = dat, rules = x, old_violations = old_violations,
        A = lc$A, b = lc$b, ops = lc$operators, x = X, eps = eps
      )
      X <- impute_result$x
      dat <- impute_result$dat
      done <- !any(impute_result$is_changed)
    }
  }

  # Impute by determining implied variable ranges.
  # This method is outside the method loop because it is computationally expensive.
  if ("fm" %in% methods) {
    impute_result <- impute_lr_single(
      func = impute_range, dat = dat, rules = x, old_violations = old_violations,
      A = lc$A, b = lc$b, ops = lc$operators, x = X, eps = eps
    )
    dat <- impute_result$dat
    X <- impute_result$x
  }
  dat
}

# Single pass imputation using the given imputation function.
impute_lr_single <- function(func, dat, rules, old_violations, A, b, ops, x, eps) {
  x_new <- func(A = A, b = b, ops = ops, x = x, eps = eps)
  is_changed <- attr(x_new, "changed")
  if (any(is_changed)) {
    # Determine changed records of dat.
    x_changed <- x_new[, is_changed, drop = FALSE]
    dat_changed <- dat[is_changed, ]
    dat_changed[, rownames(x_new)] <- t(x_changed)
    # Find rule violations in new dat.
    confront_values <- validate::values(validate::confront(dat_changed, rules))
    confront_violations <- (confront_values == FALSE & !is.na(confront_values))
    # Find violations in dat that are not old violations.
    is_new_violation <- (confront_violations & !old_violations[is_changed, ])
    is_bad_change <- rowSums(is_new_violation) > 0
    # Remove changes that introduced new rule violations.
    x_changed <- x_changed[, !is_bad_change, drop = FALSE]
    is_changed[is_bad_change] <- FALSE
    # Apply the non-bad changes.
    x[, is_changed] <- x_changed
    dat[is_changed, rownames(x_new)] <- t(x_changed)
  }
  list(dat = dat, x = x, is_changed = is_changed)
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
pivimpute <- function(A, b, ops, x, eps=1e-8) {
  nx <- ncol(x)
  is_changed <- rep(FALSE, nx)
  eq <- ops == "=="
  if (any(eq)) { #prevent svd on matrix of dimension 0
    for (col in seq_len(nx)) {
      x_ <- x[,col,drop=FALSE]
      miss <- is.na(x_)
      if (!any(miss)) next
      Am <- A[eq,miss,drop=FALSE]
      Ami <- lintools::pinv(Am)
      Id <- diag(1,nrow(Ami))
      C <- abs(Id - Ami %*% Am)
      J <- rowSums(C < eps) == ncol(C)
      if (any(J)) {
        v <- Ami%*%(b[eq] - A[eq,!miss,drop=FALSE]%*%x_[!miss,drop=FALSE])
        x_[which(miss)[J]] <- v[J]
        x[,col] <- x_
        is_changed[col] <- TRUE
      }
    }
  }
  attr(x, "changed") <- is_changed
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
  .Call("R_imputezero",A[eq,,drop=FALSE],b, x, nonneg_var, eps)
}




# implied values
# impute values implied by two simple inequalities

impute_implied <- function(A, b, ops, x, eps=1e-8){
  isna_old <- is.na(x)
  x <- apply(x,2,impute_implied_x, A=A, b=b, ops=ops, eps=eps)
  isna_new <- is.na(x)
  attr(x, "changed") <- (colSums(isna_new != isna_old) > 0)
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
      , eps=eps
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
impute_range <- function(A, b, ops, x, eps=1e-8){
  isna_old <- is.na(x)
  neq <- sum(ops=="==")
  nleq <- sum(ops == "<=")
  x <- apply(x,2,impute_range_x,A=A,b=b,neq=neq,nleq=nleq,eps=eps)
  isna_new <- is.na(x)
  attr(x, "changed") <- (colSums(isna_new != isna_old) > 0)
  x
}

# impute by deriving variable ranges (may be computationally expensive)
impute_range_x <- function(x,A,b,neq, nleq,eps=1e-8){
  obs <- !is.na(x)
  if (all(obs)) return(x)
  tryCatch({
    L <- lintools::subst_value(A=A,b=b,variables=obs, values=x[obs], eps=eps)
    L <- lintools::compact(A=L$A, b=L$b, x=NULL,
        								  , neq               = neq
        								  , nleq              = nleq
        								  , remove_columns    = FALSE
        								  , remove_rows       = TRUE
        								  , deduplicate       = TRUE
        								  , implied_equations = TRUE)
    R <- lintools::ranges(A=L$A,b=L$b,neq=L$neq,nleq=L$nleq,eps=eps)
    i <- (R[ ,"upper"] - R[ ,"lower"] < eps) & ( R[,"lower"] <= R[,"upper"] )
    i[!is.finite(i)] <- FALSE
    x[i] <- R[i,"upper"]
  }, error=function(e){
    message(sprintf("FM method skipped because %s", e$message))
  })
  x
}
