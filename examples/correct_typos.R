library(validate)

# example from section 4 in Scholtus (2009)

v <-validate::validator( 
   x1 + x2 == x3
 , x2 == x4
 , x5 + x6 + x7 == x8
 , x3 + x8 == x9
 , x9 - x10 == x11
 )
 

dat <- read.csv(textConnection(
"x1, x2 , x3  , x4 , x5 , x6, x7, x8 , x9   , x10 , x11
1452, 116, 1568, 116, 323, 76, 12, 411,  1979, 1842, 137
1452, 116, 1568, 161, 323, 76, 12, 411,  1979, 1842, 137
1452, 116, 1568, 161, 323, 76, 12, 411, 19979, 1842, 137
1452, 116, 1568, 161,   0,  0,  0, 411, 19979, 1842, 137
1452, 116, 1568, 161, 323, 76, 12,   0, 19979, 1842, 137"
))
cor <- correct_typos(dat,v)
dat - cor




