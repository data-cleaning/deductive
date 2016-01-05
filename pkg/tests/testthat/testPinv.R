
context("Imputation by pseudoinverse")

test_that("Computing pseudoinverse",{
 # example from appendix of Schaum's outline (2009) on linear algebra
  A <- matrix(c(
     1,  1, -1,  2,
     2,  2, -1,  3,
    -1, -1,  2, -3
  ),byrow=TRUE,nrow=3)
  Aplus55 <- matrix(c(
     1, 18,  15,
     1, 18,  15,
    -2, 19,  25,
     3, -1, -10
    ),byrow=TRUE, nrow=4)

  expect_equal(pinv(A),Aplus55/55)
})

