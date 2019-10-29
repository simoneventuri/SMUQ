SubModule(MultiVarPolyFitModel_Class) MultiVarPolyFitModel_LSQ_SubClass

!  Module for unconstrained linear least-squares calculations.
!  The algorithm is suitable for updating LS calculations as more
!  data are added.   This is sometimes called recursive estimation.
!  Only One dependent variable is allowed.
!  Based upon Applied Statistics algorithm AS 274.
!  Translation from Fortran 77 to Fortran 90 by Alan Miller.
!  A subroutine, VARPRD, has been added for calculating the variances
!  of predicted values, and this uses a Module Procedure BKSUB2.

!  Version 1.11, 8 November 1999 - ELF90 compatible version
!  F version - 9 April 2000
!  Author: Alan Miller
!      CSIRO Mathematical Information Sciences
!      Private Bag 10, Clayton South MDC
!      Clayton 3169, Victoria, Australia
!  Phone: (+61) 3 9545-8036      Fax: (+61) 3 9545-8080
!  e-mail: Alan.Miller @ vic.cmis.csiro.au  & milleraj @ ozemail.com.au
!  WWW-page: http://www.ozemail.com.au/~milleraj

!  Bug fixes:
!  1. In REGCF a call to TOLSET has been added in case the user had
! not set tolerances.
!  2. In SING, each time a singularity is detected, unless it is in the
! variables in the last position, AddPoint is called.  AddPoint assumes
! that a new observation is being added and increments the number of
! cases, This%NOBS.   The line:  This%NOBS = This%NOBS - 1 has been added.
!  4. In COV, now calls SS if This%RSS_SET = .False.  29 August 1997

!  Other changes:
!  1. Array row_ptr added 18 July 1997.   This points to the first element
! stored in each row thus saving a small amount of time needed to
! calculate its position.
!  2. Optional parameter, EPS, added to routine TOLSET, so that the user
! can specify the accuracy of the input data.

!  The PUBLIC variables are:
!  rkp       = a KIND parameter for the floating-point quantities calculated
!         in this module.   See the more detailed explanation below.
!         This KIND parameter should be used for all floating-point
!         arguments passed to routines in this module.

!  nobs    = the number of observations processed to date.
!  ncol    = the total number of variables, including One for the constant,
!        if a constant is being fitted.
!  r_dim   = the dimension of array r = ncol*(ncol-1)/2
!  vorder  = an integer vector storing the current order of the variables
!        in the QR-factorization.   The initial order is 0, 1, 2, ...
!        if a constant is being fitted, or 1, 2, ... otherwise.
!  initialized = a logical variable which indicates whether space has
!            been allocated for various arrays.
!  tol_set = a logical variable which is set when Module Procedure TOLSET has
!        been called to calculate tolerances for use in testing for
!        singularities.
!  This%RSS_SET = a logical variable indicating whether residual sums of squares
!        are available and usable.
!  d()     = array of row multipliers for the Cholesky factorization.
!        The factorization is X = Q.sqrt(D).R where Q is an ortho-
!        normal matrix which is NOT stored, D is a diagonal matrix
!        whose diagonal elements are stored in array d, and R is an
!        upper-triangular matrix with 1's as its diagonal elements.
!  rhs()   = vector of RHS projections (after scaling by sqrt(D)).
!        Thus Q'y = sqrt(D).rhs
!  r()     = the upper-triangular matrix R.   The upper triangle only,
!        excluding the implicit 1's on the diagonal, are stored by
!        rows.
!  tol()   = array of tolerances used in testing for singularities.
!  rss()   = array of residual sums of squares.   rss(i) is the residual
!        sum of squares with the first i variables in the model.
!        By changing the order of variables, the residual sums of
!        squares can be found for all possible subsets of the variables.
!        The residual sum of squares with NO variables in the model,
!        that is the total sum of squares of the y-values, can be
!        calculated as rss(1) + d(1)*rhs(1)^2.   If the first variable
!        is a constant, then rss(1) is the sum of squares of
!        (y - ybar) where ybar is the average value of y.
!  sserr   = residual sum of squares with all of the variables included.
!  row_ptr() = array of indices of first elements in each row of R.
!
!--------------------------------------------------------------------------

  use Parameters_Library  ,only:  Zero, One

  implicit none

  real(rkp) ,parameter       ::  Small = 10.0_rkp * tiny(Zero)

  contains

! This procedure adds a points {xi,y} with the specified weight to the data.
! This updates the variables This%D, This%R, This%RHS and This%SSERR.
Module Procedure AddPoint

  integer                                                               ::  i, k, NEXTR
  real(rkp)                                                             ::  x_, y_, Weight_
  real(rkp)                                                             ::  DI, WXI, DPI, CBAR, SBAR, XK
  real(rkp) ,dimension(size(xi))                                        ::  xi_

  This%NOBS     =   This%NOBS + 1
  This%RSS_SET  =   .False.
  Weight_   =   One
  if ( present(Weight) ) Weight_ = Weight
  xi_     =   xi
  y_      =   y
  NEXTR   =   1

  ! Skip unnecessary transformations.   Test on exact zeroes must be
  ! used or stability can be destroyed.
  do i = 1,This%NParam

    if (abs(Weight_) < Small) return
    x_        =   xi_(i)
    if (abs(x_) < Small) then
      NEXTR   =   NEXTR + This%NParam - i
    else
      DI      =   This%D(i)
      WXI     =   Weight_ * x_
      DPI     =   DI + WXI*x_
      CBAR    =   DI / DPI
      SBAR    =   WXI / DPI
      Weight_ =   CBAR * Weight_
      This%D(i) = DPI
      do k = i+1,This%NParam
        XK            =   xi_(k)
        xi_(k)       =   XK - x_ * This%R(NEXTR)
        This%R(NEXTR) =   CBAR * This%R(NEXTR) + SBAR * XK
        NEXTR         =   NEXTR + 1
      end do
      XK          =   y_
      y_           =   XK - x_ * This%RHS(i)
      This%RHS(i) =   CBAR * This%RHS(i) + SBAR * XK
    end if
  end do ! i = 1,This%NParam

! y_ * SQRT(W) is now equal to the Brown, Durbin & Evans recursive residual.

  This%SSERR = This%SSERR + Weight_ * y_ * y_

End Procedure

! ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
! Modified version of AS75.4 to calculate regression coefficients
! for the first NREQ variables, given an orthogonal reduction from
! AS75.1.
Module Procedure REGCF
  integer                                                               ::  i, j, NEXTR
  IFAULT  =   0
  if ( (NREQ<1) .or. (NREQ>This%NParam) ) IFAULT = IFAULT + 4
  if (IFAULT /= 0) return ! Some checks.
  if ( .Not. This%TOL_SET) call TOLSET(This)
  do i = NREQ,1,-1
    if (sqrt(This%D(i)) < This%TOL(i)) then
      BETA(i)   =   Zero
      This%D(i) =   Zero
      IFAULT    =   -i
    else
      BETA(i)   =   This%RHS(i)
      NEXTR     =   This%ROW_PTR(i)
      do j = i+1,NREQ
        BETA(i) =   BETA(i) - This%R(NEXTR) * BETA(j)
        NEXTR   =   NEXTR + 1
      end do
    end if
  end do
End Procedure

! ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
! Sets up array This%TOL for testing for zeroes in an orthogonal
! reduction formed using AS75.1.
! Unless the argument eps is set, it is assumed that the input data are
! recorded to full machine accuracy.   This is often not the case.
! If, for instance, the data are recorded to `single precision' of about
! 6-7 significant decimal digits, then singularities will not be detected.
! It is suggested that in this case eps should be set equal to
! 10.0 * EPSILON(1.0)
! If the data are recorded to say 4 significant decimals, then eps should
! be set to 1.0E-03
! The above comments apply to the predictor variables, not to the
! dependent variable.
Module Procedure TOLSET

  use Parameters_Library  ,only:  Ten

  integer                                                               ::  COL, ROW, POS
  real(rkp)                                                             ::  EPS1, TOTAL
  real(rkp) ,dimension(This%NParam)                                       ::  WORK

! EPS is a machine-dependent constant.
  if (present(EPS)) then
    EPS1  =   max(abs(EPS), Ten * epsilon(Ten))
  else
    EPS1  =   Ten * epsilon(Ten)
  end if

! Set tol(i) = sum of absolute values in column i of This%R after scaling each
! element by the square root of its row multiplier, multiplied by EPS1.
  WORK      =   sqrt(This%D)
  do COL = 1,This%NParam
    POS     =   COL - 1
    TOTAL   =   WORK(COL)
    do ROW = 1,COL-1
      TOTAL =   TOTAL + abs(This%R(POS)) * WORK(ROW)
      POS   =   POS + This%NParam - ROW - 1
    end do
    This%TOL(COL) = EPS1 * TOTAL
  end do

  This%TOL_SET = .True.
End Procedure




! ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
! Checks for singularities, reports, and adjusts orthogonal
! reductions produced by AS75.1.
! Auxiliary routines called: AddPoint, TOLSET
Module Procedure SING

  real(rkp)                                                             ::  TEMP, Y, Weight
  integer                                                               ::  COL, POS, ROW, POS2
  real(rkp) ,dimension(This%NParam)                                       ::  X, WORK

  IFAULT = 0

  WORK = sqrt(This%D)
  if (.Not. This%TOL_SET) call TOLSET(This)

    do COL = 1,This%NParam
    TEMP = This%TOL(COL)
    POS = COL - 1
    do ROW = 1,COL-1
      POS = POS + This%NParam - ROW - 1
    end do

  ! If diagonal element is near Zero, set it to Zero, set appropriate
  ! element of LINDEP, and use AddPoint to augment the projections in
  ! the lower rows of the orthogonalization.

    LINDEP(COL) = .False.
    if (WORK(COL) <= TEMP) then
      LINDEP(COL) = .True.
      IFAULT = IFAULT - 1
      if (COL < This%NParam) then
        POS2 = POS + This%NParam - COL + 1
        X = Zero
        X(COL+1:This%NParam) = This%R(POS+1:POS2-1)
        Y = This%RHS(COL)
        Weight = This%D(COL)
        This%R(POS+1:POS2-1) = Zero
        This%D(COL) = Zero
        This%RHS(COL) = Zero
        call This%AddPoint( X, Y, Weight=Weight )
        This%NOBS = This%NOBS - 1
      else
        This%SSERR = This%SSERR + This%D(COL) * This%RHS(COL)**2
      end if ! (col < This%NParam)
    end if ! (work(col) <= temp)
  end do ! col = 1,This%NParam

End Procedure

! ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
! Calculates partial residual sums of squares from an orthogonal
! reduction from AS75.1.
Module Procedure SS
  integer                                                               ::  i
  real(rkp)                                                             ::  TOTAL
  TOTAL   = This%SSERR
  This%RSS(This%NParam) = This%SSERR
  do i = This%NParam, 2, -1
    TOTAL         =   TOTAL + This%D(i) * This%RHS(i)**2
    This%RSS(i-1) =   TOTAL
  end do
  This%RSS_SET    =   .True.
End Procedure



! ALGORITHM AS274  APPL. STATIST. (1992) VOL.41,NO. 2
! Calculate covariance matrix for regression coefficients for the
! first nreq variables, from an orthogonal reduction produced from
! AS75.1.
! Auxiliary routine called: INV
Module Procedure COV

  integer                                                               :: DIM_RINV, POS, ROW, START, POS2, COL, POS1, k
  real(rkp)                                                             :: TOTAL
  real(rkp) ,allocatable                                                ::  RINV(:)

! Check that dimension of array covmat is adequate.
  if (DIMCOV < NREQ*(NREQ+1)/2) then
    IFAULT = 1
    return
  end if

! Check for small or Zero multipliers on the diagonal.
  IFAULT = 0
  do ROW = 1,NREQ
    if (abs(This%D(ROW)) < Small) then
      IFAULT = -ROW
    end if
  end do
  if (IFAULT /= 0) then
    return
  end if

! Calculate estimate of the residual variance.
  if (This%NOBS > NREQ) then
    if (.Not. This%RSS_SET) then
      call SS(This)
    end if
    VAR = This%RSS(NREQ) / (This%NOBS - NREQ)
  else
    IFAULT = 2
    return
  end if

  DIM_RINV = NREQ*(NREQ-1)/2
  allocate ( RINV(DIM_RINV) )

  call INV(This,NREQ, RINV)
  POS = 1
  START = 1
  do ROW = 1,NREQ
    POS2 = START
    do COL = ROW, NREQ
      POS1 = START + COL - ROW
      if (ROW == COL) then
        TOTAL = One / This%D(COL)
      else
        TOTAL = RINV(POS1-1) / This%D(COL)
      end if
      do k = COL+1,NREQ
        TOTAL = TOTAL + RINV(POS1) * RINV(POS2) / This%D(k)
        POS1 = POS1 + 1
        POS2 = POS2 + 1
      end do
      COVMAT(POS) = TOTAL * VAR
      if (ROW == COL) then
        STERR(ROW) = sqrt(COVMAT(POS))
      end if
      POS = POS + 1
    end do
    START = START + NREQ - ROW
  end do

  deallocate(RINV)

End Procedure

! ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
! Invert first nreq rows and columns of Cholesky factorization
! produced by AS 75.1.
Module Procedure INV
  integer                                                               ::  POS, ROW, COL, START, k, POS1, POS2
  real(rkp)                                                             ::  TOTAL
  ! Invert This%R ignoring row multipliers, from the bottom up.
  POS = NREQ * (NREQ-1)/2
  do ROW = NREQ-1,1,-1
    START = This%ROW_PTR(ROW)
    do COL = NREQ, ROW+1,-1
      POS1 = START
      POS2 = POS
      TOTAL = Zero
      do k = ROW+1,COL-1
        POS2 = POS2 + NREQ - k
        TOTAL = TOTAL - This%R(POS1) * RINV(POS2)
        POS1 = POS1 + 1
      end do ! k = row+1,col-1
      RINV(POS) = TOTAL - This%R(POS1)
      POS = POS - 1
    end do ! col = nreq, row+1,-1
  end do ! row = nreq-1,1,-1
End Procedure



! Replaces subroutines PCORR and COR of:
! ALGORITHM AS274  APPL. STATIST. (1992) VOL.41,NO. 2
! Calculate partial correlations after the variables in rows
! 1,2, ..., INVAR have been forced into the regression.
! If INVAR = 1,and the first row of This%R represents a constant in the
! model, then the usual simple correlations are returned.
! If INVAR = 0, the value returned in array CORMAT for the correlation
! of variables Xi & Xj is:
!   sum ( Xi.Xj ) / Sqrt ( sum (Xi^2) . sum (Xj^2) )
! On return, array CORMAT contains the upper triangle of the matrix of
! partial correlations stored by rows, excluding the 1's on the diagonal.
! e.g. if INVAR = 2, the consecutive elements returned are:
! (3,4) (3,5) ... (3,This%NParam), (4,5) (4,6) ... (4,This%NParam), etc.
! Array YCORR stores the partial correlations with the Y-variable
! starting with YCORR(INVAR+1) = partial correlation with the variable in
! position (INVAR+1).
Module Procedure PARTIAL_CORR

  integer                                                               ::  BASE_POS, POS, ROW, COL, COL1, COL2, POS1, POS2, NP
  real(rkp)                                                             ::  SUMXX, SUMXY, SUMYY
  real(rkp), dimension(INVAR+1:This%NParam)  :: RMS, WORK

  NP    =   This%NParam
  ! Some checks.

  IFAULT = 0
  if (INVAR < 0 .or. INVAR > NP-1) then
    IFAULT = IFAULT + 4
  end if
  if (DIMC < (NP-INVAR)*(NP-INVAR-1)/2) then
    IFAULT = IFAULT + 8
  end if
  if (IFAULT /= 0) then
    return
  end if

  ! Base position for calculating positions of elements in row (INVAR+1) of This%R.

  BASE_POS = INVAR*NP - (INVAR+1)*(INVAR+2)/2

  ! Calculate 1/RMS of elements in columns from INVAR to (NP-1).

  if (This%D(INVAR+1) > Zero) then
    RMS(INVAR+1) = One / sqrt(This%D(INVAR+1))
  end if
  do COL = INVAR+2, NP
    POS = BASE_POS + COL
    SUMXX = This%D(COL)
    do ROW = INVAR+1,COL-1
      SUMXX = SUMXX + This%D(ROW) * This%R(POS)**2
      POS = POS + NP - ROW - 1
    end do ! row = INVAR+1,col-1
    if (SUMXX > Zero) then
      RMS(COL) = One / sqrt(SUMXX)
    else
      RMS(COL) = Zero
      IFAULT = -COL
    end if
  end do

  ! Calculate 1/RMS for the Y-variable

  SUMYY = This%SSERR
  do ROW = INVAR+1,NP
    SUMYY = SUMYY + This%D(ROW) * This%RHS(ROW)**2
  end do ! row = INVAR+1,NP
  if (SUMYY > Zero) then
    SUMYY = One / sqrt(SUMYY)
  end if

  ! Calculate sums of cross-products.
  ! These are obtained by taking dot products of pairs of columns of This%R,
  ! but with the product for each row multiplied by the row multiplier
  ! in array This%D.

  POS = 1
  do COL1 = INVAR+1,NP
    SUMXY = Zero
    WORK(COL1+1:NP) = Zero
    POS1 = BASE_POS + COL1
    do ROW = INVAR+1,COL1-1
      POS2 = POS1 + 1
      do COL2 = COL1+1,NP
        WORK(COL2) = WORK(COL2) + This%D(ROW) * This%R(POS1) * This%R(POS2)
        POS2 = POS2 + 1
      end do ! col2 = col1+1,NP
      SUMXY = SUMXY + This%D(ROW) * This%R(POS1) * This%RHS(ROW)
      POS1 = POS1 + NP - ROW - 1
    end do ! row = INVAR+1,col1-1

  ! Row COL1 has an implicit 1 as its first element (in column COL1)

    POS2 = POS1 + 1
    do COL2 = COL1+1,NP
      WORK(COL2) = WORK(COL2) + This%D(COL1) * This%R(POS2)
      POS2 = POS2 + 1
      CORMAT(POS) = WORK(COL2) * RMS(COL1) * RMS(COL2)
      POS = POS + 1
    end do ! col2 = col1+1,NP
    SUMXY = SUMXY + This%D(COL1) * This%RHS(COL1)
    YCORR(COL1) = SUMXY * RMS(COL1) * SUMYY
  end do ! col1 = INVAR+1,NP-1

  YCORR(1:INVAR) = Zero

End Procedure


! ALGORITHM AS274 APPL. STATIST. (1992) VOL.41, NO. 2
! Move variable from position FROM to position DEST in an
! orthogonal reduction produced by AS75.1.
Module Procedure VMOVE

  real(rkp)                                                             ::  D1, D2, X, D1NEW, D2NEW, CBAR, SBAR, Y
  integer                                                               ::  M, FIRST, LAST, INC, M1, M2, MP1, COL, POS, ROW

  ! Check input parameters

  IFAULT = 0
  if (FROM < 1 .or. FROM > This%NParam) then
    IFAULT = IFAULT + 4
  end if
  if (DEST < 1 .or. DEST > This%NParam) then
    IFAULT = IFAULT + 8
  end if
  if (IFAULT /= 0) then
    return
  end if

  if (FROM == DEST) then
    return
  end if

  if (.Not. This%RSS_SET) then
    call SS(This)
  end if

  if (FROM < DEST) then
    FIRST = FROM
    LAST = DEST - 1
    INC = 1
  else
    FIRST = FROM - 1
    LAST = DEST
    INC = -1
  end if

  do M = FIRST, LAST, INC

  ! Find addresses of first elements of This%R in rows M and (M+1).

    M1 = This%ROW_PTR(M)
    M2 = This%ROW_PTR(M+1)
    MP1 = M + 1
    D1 = This%D(M)
    D2 = This%D(MP1)

  ! Special cases.

    if (D1 >= Small .or. D2 >= Small) then
      X = This%R(M1)
      if (abs(X) * sqrt(D1) < This%TOL(MP1)) then
        X = Zero
      end if
      if (D1 < Small .or. abs(X) < Small) then
        This%D(M) = D2
        This%D(MP1) = D1
        This%R(M1) = Zero
        do COL = M+2, This%NParam
          M1 = M1 + 1
          X = This%R(M1)
          This%R(M1) = This%R(M2)
          This%R(M2) = X
          M2 = M2 + 1
        end do
        X = This%RHS(M)
        This%RHS(M) = This%RHS(MP1)
        This%RHS(MP1) = X
      else if (D2 < Small) then
        This%D(M) = D1 * X**2
        This%R(M1) = One / X
        This%R(M1+1:M1+This%NParam-M-1) = This%R(M1+1:M1+This%NParam-M-1) / X
        This%RHS(M) = This%RHS(M) / X
      else

  ! Planar rotation in regular case.

        D1NEW = D2 + D1*X**2
        CBAR = D2 / D1NEW
        SBAR = X * D1 / D1NEW
        D2NEW = D1 * CBAR
        This%D(M) = D1NEW
        This%D(MP1) = D2NEW
        This%R(M1) = SBAR
        do COL = M+2, This%NParam
          M1 = M1 + 1
          Y = This%R(M1)
          This%R(M1) = CBAR*This%R(M2) + SBAR*Y
          This%R(M2) = Y - X*This%R(M2)
          M2 = M2 + 1
        end do
        Y = This%RHS(M)
        This%RHS(M) = CBAR*This%RHS(MP1) + SBAR*Y
        This%RHS(MP1) = Y - X*This%RHS(MP1)
      end if
    end if

  ! Swap columns M and (M+1) down to row (M-1).

    POS = M
    do ROW = 1,M-1
      X = This%R(POS)
      This%R(POS) = This%R(POS-1)
      This%R(POS-1) = X
      POS = POS + This%NParam - ROW - 1
    end do ! row = 1,m-1

  ! Adjust variable order (This%VORDER), the tolerances (This%TOL) and
  ! the vector of residual sums of squares (This%RSS).

    M1 = This%VORDER(M)
    This%VORDER(M) = This%VORDER(MP1)
    This%VORDER(MP1) = M1
    X = This%TOL(M)
    This%TOL(M) = This%TOL(MP1)
    This%TOL(MP1) = X
    This%RSS(M) = This%RSS(MP1) + This%D(MP1) * This%RHS(MP1)**2
  end do

End Procedure

! ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
! Re-order the variables in an orthogonal reduction produced by
! AS75.1 so that the N variables in LIST start at position POS1,
! though will not necessarily be in the same order as in LIST.
! Any variables in This%VORDER before position POS1 are not moved.
! Auxiliary routine called: VMOVE
Module Procedure REORDR

  integer                                                               ::  NEXT, i, L, j
  logical                                                               ::  FOUND

  ! Check N.

  IFAULT = 0
  if (N < 1 .or. N > This%NParam+1-POS1) then
    IFAULT = IFAULT + 4
  end if
  if (IFAULT /= 0) then
    return
  end if

  ! Work through VORDER finding variables which are in LIST.

  NEXT = POS1
  do i = POS1,This%NParam
    L = This%VORDER(i)
    FOUND = .False.
    do j = 1,N
      if (L == LIST(j)) then
        FOUND = .True.
        exit
      end if
    end do

  ! Variable L is in LIST; move it up to position NEXT if it is not
  ! already there.

    if (FOUND) then
      if (i > NEXT) then
        call VMOVE(This,i, NEXT, IFAULT)
      end if
      NEXT = NEXT + 1
    end if
  end do

  if (NEXT >= N+POS1) then
    return
  end if

  ! If this point is reached, One or more variables in LIST has not
  ! been found.

  IFAULT = 8

End Procedure

! ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2
Module Procedure HDIAG

  integer         :: COL, ROW, POS
  real(rkp)  :: TOTAL
  real(rkp) ,dimension(This%NParam)                                       ::  WK

  ! Some checks

  IFAULT = 0
  if (NREQ > This%NParam) then
    IFAULT = IFAULT + 4
  end if
  if (IFAULT /= 0) then
    return
  end if

  ! The elements of xrow.inv(This%R).sqrt(This%D) are calculated and stored
  ! in WK.

  HII = Zero
  do COL = 1,NREQ
    if (sqrt(This%D(COL)) <= This%TOL(COL)) then
      WK(COL) = Zero
    else
      POS = COL - 1
      TOTAL = XROW(COL)
      do ROW = 1,COL-1
        TOTAL = TOTAL - WK(ROW)*This%R(POS)
        POS = POS + This%NParam - ROW - 1
      end do ! row = 1,col-1
      WK(COL) = TOTAL
      HII = HII + TOTAL**2 / This%D(COL)
    end if
  end do ! col = 1,nreq

End Procedure


! Calculate the variance of x'b where b consists of the first nreq
! least-squares regression coefficients.
Module Procedure VARPRD

  integer         :: IFAULT, ROW
  real(rkp)  :: VAR
  real(rkp), dimension(NREQ) :: WK

  ! Check input parameter values

  FN_VAL = Zero
  IFAULT = 0
  if (NREQ < 1 .or. NREQ > This%NParam) then
    IFAULT = IFAULT + 4
  end if
  if (This%NOBS <= NREQ) then
    IFAULT = IFAULT + 8
  end if
  if (IFAULT /= 0) then
    write(unit=*, fmt="(1x, a, i4)") "Error in function VARPRD: ifault =", IFAULT
    return
  end if

  ! Calculate the residual variance estimate.

  VAR = This%SSERR / (This%NOBS - NREQ)

  ! Variance of x'b = var.x'(inv This%R)(inv This%D)(inv This%R')x
  ! First call BKSUB2 to calculate (inv This%R')x by back-substitution.

  call BKSUB2(This,X, WK, NREQ)
  do ROW = 1,NREQ
    if(This%D(ROW) > This%TOL(ROW)) then
      FN_VAL = FN_VAL + WK(ROW)**2 / This%D(ROW)
    end if
  end do

  FN_VAL = FN_VAL * VAR

End Procedure


! Solve x = This%R'b for b given x, using only the first nreq rows and
! columns of This%R, and only the first nreq elements of This%R.
! Solve by back-substitution, starting from the top.
Module Procedure BKSUB2
  integer           :: POS, ROW, COL
  real(rkp)                                                             ::  TEMP
  do ROW = 1,NREQ
    POS     =   ROW - 1
    TEMP    =   X(ROW)
    do COL = 1,ROW-1
      TEMP  =   TEMP - This%R(POS)*B(COL)
      POS   =   POS + This%NParam - COL - 1
    end do
    B(ROW)  =   TEMP
  end do
End Procedure

End SubModule