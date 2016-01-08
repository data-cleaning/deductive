
context("Imputation using linear restrictions")


test_that("imputation by pseudoinverse",{
  # example from de Waal et al (2009) pp 304.
  v <- validator(
    x1 + x2 == x3
    , x2 == x4
    , x5 + x6 + x7 == x8
    , x3 + x8 == x9
    , x9 - x10 == x11
  )
  dat <- data.frame(
    x1 = 145
    , x2 = NA
    , x3 = 155
    , x4 = NA
    , x5 = NA
    , x6 = NA
    , x7 = NA
    , x8 = 86
    , x9 = NA
    , x10 = 217
    , x11 = NA
  )
  
  d2 <- data.frame(
    x1 = 145
    , x2 = 10
    , x3 = 155
    , x4 = 10
    , x5 = NA_real_
    , x6 = NA_real_
    , x7 = NA_real_
    , x8 = 86
    , x9 = 241
    , x10 = 217
    , x11 = 24
  )
  expect_equal(impute_lr(dat,v), d2)
  
})

test_that("imputation with zeros",{
  # example from de Waal et al (2009) pp 307.
  v <- validator(
    x1 + x2 == x3
    , x2 == x4
    , x5 + x6 + x7 == x8
    , x3 + x8 == x9
    , x9 - x10 == x11
    , x6 > 0, x7 > 0
  )
  
  dat <- data.frame(
    x1 = 145
    , x2 = 10
    , x3 = 155
    , x4 = 10
    , x5 = 86
    , x6 = NA_real_
    , x7 = NA_real_
    , x8 = 86
    , x9 = 241
    , x10 = 217
    , x11 = 24
  )
  d2 <- data.frame(
    x1 = 145
    , x2 = 10
    , x3 = 155
    , x4 = 10
    , x5 = 86
    , x6 = 0
    , x7 = 0
    , x8 = 86
    , x9 = 241
    , x10 = 217
    , x11 = 24
  )
  expect_equal(impute_lr(dat,v), d2)
  
})



test_that("imputation of zeros",{
  v <- validator(
    x1 + x2 + x3 == x4
    , x2 > 0, x3 > 0
  )
  X <- matrix(c(
    1,NA,NA,1,
    NA,1,1,2
  ),ncol=2)
  attr(X,"changed") <- FALSE
  out <- matrix(c(1,0,0,1, NA,1,1,2),ncol=2)
  attr(out,"changed") <- TRUE
  lc <- v$linear_coefficients()
  expect_equal(
    zeroimpute(A=lc$A, b=lc$b, ops=lc$operators, x=X)
    ,  out
  )
})


