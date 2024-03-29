
if (packageVersion("lintools") <= "0.1.3") exit_file("insufficient lintools version")

## imputation by pseudoinverse ----
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
lc <- v$linear_coefficients()
expect_equivalent(
  deductive:::pivimpute(A=lc$A, b= lc$b,ops = lc$operators, x=t(dat), eps=1e-8)
, t(d2))



## imputation with zeros ----
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
lc <- v$linear_coefficients()
expect_equivalent(
  deductive:::zeroimpute(A=lc$A, b=lc$b, ops=lc$operators, x=t(dat) ,eps=1e-8)
  , t(d2)
)



## imputation of zeros ----
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
    deductive:::zeroimpute(A=lc$A, b=lc$b, ops=lc$operators, x=X)
    ,  out
  )


## imputation of implied values ----
  
v <- validator(x + 2*y == 3,   x + y + z == 7)
L <- v$linear_coefficients()
# takes two iterations to impute
x_ <- c(1,NA,NA)
expect_equal(
  deductive:::impute_implied_x(L$A, L$b, L$operators, x_)
  ,c(1,1,5)
)
# inequalities (single iteration)
v <- validator( x <= 0, x >=0, y <=1, y >= 1)
L <- v$linear_coefficients()
x_ <- c(NA,NA)
expect_equal(
    deductive:::impute_implied_x(L$A, L$b, L$operators, x_)
    , c(0,1)
)


## impute_lr errors when it should ----
v <- validator(x == y)
expect_error(impute_lr(data.frame(x="a",y=10),v), pattern="Linear restrictions on nonnumeric data")


## impute_lr ----
# example from DCAR 
v <- validate::validator( 
  x1 + x2      == x3 
  , x4 + x5 + x6 == x1 
  , x7 + x8      == x2 
  , x9 + x10     == x3) 
dat <- data.frame( 
  x1 = 100, x2=NA_real_, x3=NA_real_, x4 = 15, x5 = NA_real_ 
  , x6 = NA_real_, x7 = 25, x8 = 35, x9 = NA_real_, x10 = 5) 

expect_equal(
  impute_lr(dat,v) 
, data.frame( x1 = 100, x2=60, x3=160, x4 = 15, x5 = NA_real_ 
            , x6 = NA_real_, x7 = 25, x8 = 35, x9 = 155, x10 = 5)
) 


## imputation by range determination ----
# y == 2
# y + z == 3  (so z=1)
# x + y <= 0  
A <- matrix(c(0,0,1, 1,1,1,0,1,0),nrow=3)
b <- c(2,3,0)
deductive:::impute_range_x(x=c(NA,NA,NA), A=A, b=b, neq=2, nleq=1, eps=1e-8)
lintools::ranges(A,b,neq=2,nleq=1)

# this went haywire since one of the ranges results in -Inf - -Inf

v <- validator(
  x1 >= 0, x2 >= 0, x1 + x2 == x3
  , x4 >= 0, x5 >= 0, x6 >= 0
  , x4 + x5 + x6 == x7
  , x3 + x7 == x8
)

df_in <- data.frame(x1=25,x2=NA_real_,x3=25,x4=1,x5=NA_real_,x6=NA_real_,x7=1,x8=26)
df_out <- df_in; df_out[1,c(2,5,6)] <- 0

expect_equal(impute_lr(df_in,v, methods="fm"),df_out)


## works with var_group ----
rules <- validator(var_group(a,b,c,d) >= 0, a+b+c == d)
d <- data.frame(a=NA, b=NA, c=5,d=5)
expect_equal(impute_lr(d,rules, method="fm"), data.frame(a=0,b=0,c=5,d=5))

## works with ill-defined problem ----
rules <- validator(var_group(a,b,c,d)>=0, a+b+c==d)
d <- data.frame(a=NA_real_, b=NA_real_, c=10., d=9.)
expect_equal(impute_lr(d,rules), d)


## skipping all NA cases
d_in <- data.frame(x=c(NA_real_,1), y=c(NA_real_,NA_real_))
d_out <- data.frame(x=c(NA_real_,1), y=c(NA_real_,2))
rules <- validator(x>=0, y>=x, y - 2*x == 0)
expect_equal(impute_lr(d_in,rules), d_out, info="all d[1,] missing")

## regression test implied value imputation. 
#  thanks to Sigrid van Hoek for reporting the issue.
d <- read.csv("lr_implied_data.csv")
r <- validator(.file="lr_implied_rules.yml")
expect_true( all(confront(impute_lr(d,r, methods="implied"), r), na.rm=TRUE))
expect_true( all(confront(impute_lr(d,r, methods="fm"), r), na.rm=TRUE))

## regression test FM imputation. 
#  thanks to Sigrid van Hoek for reporting the issue.
d <- read.csv("lr_fm_data.csv")
r <- validator(.file="lr_fm_rules.yml")
expect_true( all(confront(impute_lr(d,r, methods="implied"), r), na.rm=TRUE))
expect_true( all(confront(impute_lr(d,r, methods="fm"), r), na.rm=TRUE))



