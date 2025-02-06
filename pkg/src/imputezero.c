
/*  Copyright (C) Mark van der Loo and Edwin de Jonge
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>. 
 *
 *  You can contact the author at: mark _dot_ vanderloo _at_ gmail _dot_ com
 */

#include <R.h>
#include <Rdefines.h>
#include <stdint.h>
#include <string.h>

/* get array dimensions */
void rdim(SEXP A, int *dim){
  SEXP dim_ = getAttrib(A, R_DimSymbol);
  dim[0] = INTEGER(dim_)[0];
  dim[1] = INTEGER(dim_)[1];
}

/* sign function with eps */
int dbl_sign(double val, double eps){
  double x = (fabs(val) < eps) ? 0. : val;
  return( (0. < x) - (x < 0.) );

}


/* impute zeros in x when a_obs . x_obs = b. */
int impute_zero(double *a, double b, int n, int *nneg, double eps, double *x){
  int changed = 0;
  double s = 0;
  int nNA = 0L   // number of missings
    , sgn = 0L   // number of a of equal sign
    , nzr = 0L   // number of a that are zero
    , nng = 0L;  // number of x that must be nonnegative
  for (int i=0; i < n; i++){
//Rprintf("nneg[%d] = %d\n",i,nneg[i]);
    if ( ISNA(x[i]) ){
      ++nNA;
      if ( fabs(a[i]) < eps ) nzr++;
      sgn += dbl_sign(a[i], eps);
      nng += nneg[i];
    } else {
      s += a[i]*x[i];
    }
//Rprintf("nNA = %d, nzr=%d, sgn=%d, nng=%d, s=%12.8f b=%12.8f\n",nNA,nzr,sgn,nng,s,b);
  }
  if ( nNA > 0 && (sgn-nzr) == nNA && nNA == nng && fabs(s - b) < eps   ){
    for (int i=0; i < n; i++ ){
      if ( ISNA(x[i]) ){ 
        x[i] = 0.;
        changed = 1;
      }
    }
  }
  return changed;
}


/* R interface
 *
 * [A_, b_]: matrices, expressing equalities Ax=b.
 * X_ matrix of column vectors.
 * nnoneg_ logical, expressing which rows of X_ (variables) must be nonnegative
 * eps_ Used to  determine when something is numerically zero.
 *
 *
 */
SEXP R_imputezero(SEXP A_, SEXP b_, SEXP X_, SEXP nonneg_, SEXP eps_ ){
  SEXP XC = PROTECT(duplicate(X_));
  double *A = REAL(A_);
  double *b = REAL(b_);
  double eps = REAL(eps_)[0];
  int *nonneg = INTEGER(nonneg_);

  int dim[2];
  rdim(A_, dim);
  int rowsA = dim[0]
    , colsA = dim[1];
  rdim(XC,dim);
  int nx = dim[1];  

  SEXP changed =  PROTECT(allocVector(LGLSXP, nx));
  int *ch = INTEGER(changed);
 
  double *a;
  a = (double *) malloc(sizeof(double) * colsA);
  double *x = REAL(XC);
  for ( int k=0; k < nx; k++ ){
    ch[k] = 0;
    for ( int i=0; i < rowsA; i++){
      for ( int j=0; j < colsA; j++) a[j] = A[j*rowsA + i];
      ch[k] |= impute_zero(a, b[i], colsA, nonneg, eps, x);
    }
    x += colsA;
  }
  setAttrib(XC, install("changed"), changed);
  free(a);
  UNPROTECT(2);
  return XC;
}













