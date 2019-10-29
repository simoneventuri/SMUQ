SubModule(Arithmetic_Module) Arithmetic_Interpolation_SubModule

  implicit none

  contains

Module Procedure cbrt
  cbrt = X * sqrt(X)
End Procedure

Module Procedure InterpLinear_0D
  real(REAL64)                                              ,parameter  ::  Zero = 0.0_REAL64
  integer                                                               ::  i                               ! Index of elements in the old dataset
  integer                                                               ::  iXOldMin                        ! Index of elements in the old dataset which has the minimum abscisse
  integer                                                               ::  iXOldMax                        ! Index of elements in the old dataset which has the maximum abscisse
  real(REAL64)                                                          ::  Slope                           ! Slope
  real(REAL64)                                                          ::  LHSBound                        ! Left bound
  real(REAL64)                                                          ::  RHSBound                        ! Right bound
  integer                                                               ::  i_Increasing                    ! Indicator whether the old dataset has increasing or decreasing abscisses
  logical                                                               ::  i_Extrapolation_Loc             ! Local extrapolation indicator
  i_Extrapolation_Loc   =       .false.                                                                         ! No extrapolation by default
  if ( present(i_Extrapolation) ) i_Extrapolation_Loc = i_Extrapolation                                         ! Setting the extrapolation indicator the input value if present
!<==============================================================================================================
!    FINDING THE INDEX OF ELEMENTS IN THE OLD DATASET WHICH HAVE THE MINIMUM/MAXIMUM ABSCISSES
!<==============================================================================================================
  iXOldMin      =       minloc(X_Old,1)                                                                         ! Getting the index of the minimum abscisse
  iXOldMax      =       maxloc(X_Old,1)                                                                         ! Getting the index of the maximum abscisse
!<==============================================================================================================
!    SETTING WHETHER THE OLD DATASET HAS INCREASING OR DECREASING ABSCISSES
!<==============================================================================================================
  if      ( iXOldMin < iXOldMax ) then                                                                          ! If the index of minimum value is lower than the index of maximum value
    i_Increasing =   1                                                                                          ! Setting the dataset as an increasing dataset
  else if ( iXOldMin > iXOldMax ) then                                                                          ! If the index of minimum value is higher than the index of maximum value
    i_Increasing = - 1                                                                                          ! Setting the dataset as an decreasing dataset
  end if                                                                                                        ! End if case
!<==============================================================================================================
!    INTERPOLATING THE DATA
!<==============================================================================================================
  if ( X_New <= X_Old(iXOldMin) ) then                                                                          ! If abscisse smaller than the lower abscisse in old dataset
    if (i_Extrapolation_Loc) then                                                                                   ! If extrapolation
      Slope     =       ( Y_Old(iXOldMin+i_Increasing) - Y_Old(iXOldMin) ) / ( X_Old(iXOldMin+i_Increasing) - X_Old(iXOldMin) )
      Y_New     =       Y_Old(iXOldMin) - Slope * ( X_Old(iXOldMin) - X_New )
    else                                                                                                        ! If no extrapolation
      Y_New     =       Y_Old(iXOldMin)                                                                         ! Imposition to lower bound
    end if                                                                                                      ! End if case on extrapolation indicator
  else if ( X_New >= X_Old(iXOldMax) ) then                                                                     ! If abscisse greater than the upper abscisse in old dataset
    if (i_Extrapolation_Loc) then                                                                                   ! If extrapolation
      Slope     =       ( Y_Old(iXOldMax) - Y_Old(iXOldMax-i_Increasing) ) / ( X_Old(iXOldMax) - X_Old(iXOldMax-i_Increasing) )
      Y_New     =       Y_Old(iXOldMax) + Slope * ( X_New - X_Old(iXOldMax) )
    else                                                                                                        ! If no extrapolation
      Y_New     =         Y_Old(iXOldMax)                                                                       ! Imposition to upper bound
    end if                                                                                                      ! End if case on extrapolation indicator
  else                                                                                                          ! If abscisse within the lower and upper bound
    do i = 1,size(X_Old)-1                                                                                      ! Loop on all points of the old dataset
      LHSBound  =       i_Increasing * ( X_New - X_Old(i) )                                                     ! Getting the left-bound
      RHSBound  =       i_Increasing * ( X_New - X_Old(i+1) )                                                   ! Getting the right-bound
      if ( (LHSBound>=Zero) .and. (RHSBound<=Zero) ) then                                                 ! If the searched abscisse is within the i and i+1 a&bscisses
        Slope   =       ( Y_Old(i+1) - Y_Old(i) ) / ( X_Old(i+1) - X_Old(i) )                                   ! Computing the slope
        Y_New   =       Y_Old(i) + Slope * ( X_New - X_Old(i) )                                                 ! Linear interpolation
        exit                                                                                                    ! Exiting the loop
      end if                                                                                                    ! End if case of abscisse value
    end do                                                                                                      ! End loop on old mesh points
  end if                                                                                                        ! End of if case
End Procedure

Module Procedure InterpLinear_1D
  integer                                                               ::  i_New                           ! Index of elements in the new dataset
  do i_New = 1,size(X_New,1)                                                                                    ! Loop on all elements to be interpolated
    Y_New(i_New) = InterpLinear( X_Old, Y_Old, X_New(i_New), i_Extrapolation )                                      ! Interpolating current element
  end do                                                                                                        ! End loop on elements to be interpolated
End Procedure





! ! REMARK:
! ! Consider a polygonal line connecting the vertices (x(i),y(i)) with i = 1,...,n taken in this order.
! ! It is assumed that the polygonal path is a loop, where (x(n),y(n)) = (x(1),y(1)) or there is an arc from (x(n),y(n)) to (x(1),y(1)).
! ! The polygon may cross itself any number of times.
! ! (x0,y0) is an arbitrary point and iPos and m are variables.
! ! On output, iPos and m are assigned the following values:
! !    iPos = -1   if (x0,y0) is outside the polygonal path
! !    iPos =  0   if (x0,y0) lies on the polygonal path
! !    iPos =  1   if (x0,y0) is inside the polygonal path
! ! m = 0 if (x0,y0) is on or outside the path.  if (x0,y0) is inside the
! ! path then m is the winding number of the path around the point (x0,y0).
! ! Original authors:
! !   - fortran 66 version by a.h. morris
! !   - converted to elf90 compatibility by alan miller, 15 february 1997
!
! Subroutine Point_In_Polygon( x0, y0, x, y, iPos, m, i_Debug )
!
!   real(REAL64)                                          ,intent(in)     ::  x0
!   real(REAL64)                                          ,intent(in)     ::  y0
!   real(REAL64)  ,dimension( : )                         ,intent(in)     ::  x
!   real(REAL64)  ,dimension( : )                         ,intent(in)     ::  y
!   integer                                               ,intent(out)    ::  iPos
!   integer                                               ,intent(out)    ::  m
!   logical                                     ,optional ,intent(in)     ::  i_Debug
!
!   logical                                                     ::  i_Debug_Loc=.true.
!
!   integer                                                               ::  NVertex                         ! Number of vertex, ie number of points making-up the polynom
!   integer                                                               ::  i
!
!   real(REAL64)                                                          ::  Tol
!   real(REAL64)                                                          ::  SumAngle
!   real(REAL64)                                                          ::  dx
!   real(REAL64)                                                          ::  dy
!   real(REAL64)                                                          ::  Angle
!   real(REAL64)                                                          ::  Theta
!   real(REAL64)                                                          ::  Theta_1
!   real(REAL64)                                                          ::  Theta_i
!
!   real(REAL64)                                              ,parameter  ::  Eps=epsilon(One)            ! Machine dependent constant corresponds to the smallest number such that 1.0 + eps > 1.0
!   real(REAL64)                                              ,parameter  ::  K_Pi=acos(-One)
!   real(REAL64)                                              ,parameter  ::  K_Pi2=2*K_Pi
!
!   i_Debug_Loc   =       .true.                                                                                  ! Initializing local debugging indicator
!   if ( present(i_Debug) ) i_Debug_Loc = i_Debug                                                                 ! Setting local debugging indicator to optional input value
!   if (i_Debug_Loc) write(LogUnit,"(6x,'[Point_In_Polygon]: Entering')")                                        ! Debugging
!
!   if (i_Debug_Loc) write(LogUnit,"(6x,'[Point_In_Polygon]: x = ',*(es15.8,3x))") x
!   if (i_Debug_Loc) write(LogUnit,"(6x,'[Point_In_Polygon]: y = ',*(es15.8,3x))") y
!   if (i_Debug_Loc) write(LogUnit,"(6x,'[Point_In_Polygon]: x0= ',*(es15.8,3x))") x0
!   if (i_Debug_Loc) write(LogUnit,"(6x,'[Point_In_Polygon]: y0= ',*(es15.8,3x))") y0
!
!   if (size(X) /= size(Y)) then
! !     ERROR
!   end if
!   NVertex       =       size(X)                                                                                 ! Setting the number of points in the polygon
!   IF ( ( x(1)==x(NVertex)) .and. (y(1)==y(NVertex)) ) NVertex = NVertex - 1                                     ! If the first and last points are identical, adjusting the number of points
!   if (NVertex < 2) return                                                                                       ! If the polynom is composed of only two points, then it is a segment and so exiting the procedure
!
!
!
!   Tol   =       4 * Eps * K_Pi
!
!   Tol   =       1.0E-02_rkp
!
!   iPos     =       -1
!   m     =       0
!
!   i     =       1
!   dx    =       x(i) - x0
!   dy    =       y(i) - y0
!   if ( (dx==Zero) .and. (dy==Zero) ) then         ! If the points is on current vertex
!     if (i_Debug_Loc) write(LogUnit,"(6x,'[Point_In_Polygon]: Flag 1')")
!     iPos = 0
!     return
!   end if
!
!
!   Theta_1       =       atan2(dy, dx)
!   SumAngle      =       Zero
!   Theta         =       Theta_1
!
!
!     if (i_Debug_Loc) write(LogUnit,"(6x,'[Point_In_Polygon]: i = ',i3,3x,'dx = ',es15.8,3x,'dy = ',es15.8,3x,'Theta_i = ',es15.8,3x,'Angle = ',es15.8)") &
!     i, dx, dy, Theta_1, Zero
!
!
!   do i = 2,NVertex
!     dx          =       x(i) - x0
!     dy          =       y(i) - y0
!     if ( (dx==Zero) .and. (dy==Zero) ) then         ! If the points is on current vertex
!       if (i_Debug_Loc) write(LogUnit,"(6x,'[Point_In_Polygon]: Flag 2')")
!       iPos         =       0
!       return
!     end if
!
!
!     Theta_i     =       atan2(dy,dx)
!     Angle       =       abs(Theta_i - Theta)
!
!     if (i_Debug_Loc) write(LogUnit,"(6x,'[Point_In_Polygon]: i = ',i3,3x,'dx = ',es15.8,3x,'dy = ',es15.8,3x,'Theta_i = ',es15.8,3x,'Angle = ',es15.8)") &
!     i, dx, dy, Theta_i, Angle
!
!     if ( abs(Angle-K_Pi) < Tol ) then
!       iPos         =       0
!       return
!     end if
!
!     if ( Angle > K_Pi    )      Angle   =         Angle - K_Pi2
!     IF ( Theta > Theta_i )      Angle   =       - Angle
!
!     SumAngle    =       SumAngle + Angle
!     Theta       =       Theta_i
!   end do
!
!   Angle = ABS(Theta_1 - Theta)
!   IF (ABS(Angle - K_Pi) < Tol) GO TO 20
!   IF (Angle > K_Pi) Angle = Angle - K_Pi2
!   IF (Theta > Theta_1) Angle = -Angle
!   SumAngle = SumAngle + Angle
!
!   !     SUM = 2*PI*M WHERE M IS THE WINDING NUMBER
!
!   m = int( ABS(SumAngle)/K_Pi2 + 0.2_rkp )
!   IF (m == 0) RETURN
!   iPos = 1
!   IF (SumAngle < 0.0) m = -m
!   RETURN
!
!   !     (X0, Y0) IS ON THE BOUNDARY OF THE PATH
!
!   20 iPos = 0
!   RETURN
!
! End Subroutine
!
! !-----------------------------------------------------------------------
!
! !                 INTERSECTION OF A STRAIGHT LINE
! !                       AND POLYGONAL PATH
!
! !-----------------------------------------------------------------------
!
! ! The polygon is defined by the set of points (xi, yi), i = 1, 2, ..., n.
! ! The straight line is from (a1,a2) to (b1,b2).
! ! On exit, the arrays U and V contain the num points at which the line
! ! crosses the polygon in order, provided that num <= m.
! ! Error indicator:
! ! ierr = 0 no error detected
! !      = 1 if a = b
! !      = 2 U and V require more storage, i.e. num > m.
! !      = -i if the ith segment of the polygon is coincident with part of the
! !           line.
!
! ! Based upon routine PFIND from the NSWC Mathematics Library.
!
! ! Code converted using TO_F90 by Alan Miller
! ! Date: 2000-07-04  Time: 12:24:01
! !
! ! Subroutine Poly_Intercept (a, b, x, y, u, v, ierr)
! !
! ! !
! !   real(REAL64)  ,dimension( 2 )                         ,intent(in)     ::  A
! !   real(REAL64)  ,dimension( 2 )                         ,intent(in)     ::  B
! !   real(REAL64)  ,dimension( : )                         ,intent(in)     ::  x
! !   real(REAL64)  ,dimension( : )                         ,intent(in)     ::  y
! !   real(REAL64)  ,dimension( : ) ,allocatable            ,intent(out)    ::  u
! !   real(REAL64)  ,dimension( : ) ,allocatable            ,intent(out)    ::  v
! !   integer                                               ,intent(out)    ::  ierr
! !
! !   real(REAL64)  :: d, diff, diff1, p, q, s, t, tmax, tmin
! !
! !
! !   real(REAL64)                                              ,parameter  ::  Eps=epsilon(One)            ! Machine dependent constant corresponds to the smallest number such that 1.0 + eps > 1.0
! !   real(REAL64)                                              ,parameter  ::  Zero=Zero
! !
! !   integer                                                               ::  NVertex                         ! Number of vertex, ie of points making-up the polynom
! !   integer                                                               ::  NPtsInt                         ! Number of points at which the segment AB crosses the polygon
! !   integer                                                               ::  iVertex                         ! Index of vertex, ie of points making-up the polynom
! !   integer                                                               ::  ind
! !
! !   real(REAL64)                                                          ::  dx, dy
! !   real(REAL64)                                                          ::  dxi, dyi
! !   real(REAL64)                                                          ::  Tol, Tol0, onem, onep           ! Tolerence
! !
! !   if (size(X) /= size(Y)) then  ! ERROR
! !   NVertex       =       size(X)                                                                                 ! Setting the number of points in the polygon
! !
! !   dx            =       b(1) - a(1)
! !   dy            =       b(2) - a(2)
! !
! ! ! ==============================================================================================================
! ! !    CHECKING INPUT VALIDITY
! ! ! ==============================================================================================================
! !   if (NVertex < 2) iErr = 1                                                                                     ! If the polygon has only 2 points, then setting the error indicator
! !   if ( (dx==Zero) .and. (dy==Zero) ) iErr = 1                                                                   ! If the segment corresponds to a point, then setting the error indicator
! !   if ( iErr == 1 ) return                                                                                       ! If erronious input, then exiting the procedure
! !
! !
! !   NPtsInt       =       0                                                                                       ! Initializing the number of intersection points
! !   iErr          =       0                                                                                       ! Initializing the error indicator
! !
! !   Tol           =       4.0_rkp * Eps
! !   Tol0          =       2.0_rkp * Eps
! !   onep          =       One + Tol
! !   onem          =       0.5_rkp + (0.5_rkp - Tol0)
! !
! !   ind           =       0
! !
! !   do  iVertex = 1, NVertex-1                                                                                          ! Loop on all the polynom vertex except the last one
! !
! !     dxi         =       x(iVertex+1) - x(iVertex)                                                                           ! Computing the increment in the x direction between current and next vertex
! !     dyi         =       y(iVertex+1) - y(iVertex)                                                                           ! Computing the increment in the y direction between current and next vertex
! !
! !     if ( (dxi==Zero) .and. (dyi==Zero) ) cycle                                                                  ! If current and next vertex are at the same position, then going to the next vertex
! !
! !     ind         =       1
! !
! !
! ! ! ==============================================================================================================
! ! !    CHECKING IF THE AB SEGMENT AND THE I-TH LINE IN THE PATH ARE PARALLEL
! ! ! ==============================================================================================================
! !     s           =       dxi * dy
! !     t           =       dx  * dyi
! !     d           =       s - t
! !     if (abs(d) <= Tol*max( abs(s),abs(t)) ) then ! go to 40     ! The lines are parallel
! !
! !      if (abs(dxi) > abs(dyi)) go to 50
! !
! !
! !     d = a(2) - y(iVertex)
! !     if (abs(d) <= Tol0*max(abs(a(2)),abs(y(iVertex)))) d = Zero
! !       s = d/dyi
! !
! !     p = x(iVertex) + s*dxi
! !     if (abs(a(1) - p) > Tol*max(abs(a(1)),abs(p))) cycle
! !
! !     d = b(2) - y(iVertex)
! !       if (abs(d) <= Tol0*max(abs(b(2)),abs(y(iVertex)))) d = Zero
! !     t = d/dyi
! !     go to 60
! !
! !     50 d = a(1) - x(iVertex)
! !     if (abs(d) <= Tol0*max(abs(a(1)),abs(x(iVertex)))) d = Zero
! !       s = d/dxi
! !
! !     p = y(iVertex) + s*dyi
! !     if (abs(p - a(2)) > Tol*max(abs(p),abs(a(2)))) cycle
! !
! !     d = b(1) - x(iVertex)
! !       if (abs(d) <= Tol0*max(abs(b(1)),abs(x(iVertex)))) d = Zero
! !     t = d/dxi
! !
! !   !              the 2 lines are portions of the same
! !   !                     straight infinite line
! !
! !       60 if (s > Zero .and. s < onem) go to 220
! !       if (t > Zero .and. t < onem) go to 220
! !       tmin = min(s,t)
! !       tmax = max(s,t)
! !       if (tmax <= Zero) go to 70
! !       if (tmin >= onem) go to 80
! !         go to 220
! !
! !       70 if (tmax < Zero) cycle
! !       if (NPtsInt > 0) cycle
! !       if (tmax == s) go to 10
! !       go to 21
! !
! !       80 if (tmin > 1.0) cycle
! !       if (tmin == s) go to 10
! !       go to 21
! !
! !
! !     else
! !
! !     end if
! !
! !
! !   !-----------------------------------------------------------------------
! !   !                   the lines are not parallel
! !   !-----------------------------------------------------------------------
! !       p = x(iVertex) - a(1)
! !     q = y(iVertex) - a(2)
! !     s = dxi*q
! !     t = dyi*p
! !     diff = s - t
! !     if (abs(diff) <= Tol*max(abs(s),abs(t))) diff = Zero
! !       s = dx*q
! !     t = dy*p
! !     diff1 = s - t
! !     if (abs(diff1) <= Tol*max(abs(s),abs(t))) diff1 = Zero
! !
! !     s = diff/d
! !       t = diff1/d
! !     if (s < Zero .or. s > onep) cycle
! !     if (t < Zero .or. t > onep) cycle
! !     if (NPtsInt > 0 .and. t == Zero) cycle
! !     if (s > Zero) go to 20
! !
! !   !                   point a is on the iVertex-th line
! !
! !     10 NPtsInt = NPtsInt + 1
! !     if (NPtsInt > m) go to 210
! !     u(NPtsInt) = a(1)
! !     v(NPtsInt) = a(2)
! !     cycle
! !
! !     !                   point b is on the iVertex-th line
! !
! !     20 if (s < onem) go to 30
! !     21 NPtsInt = NPtsInt + 1
! !     if (NPtsInt > m) go to 210
! !     u(NPtsInt) = b(1)
! !       v(NPtsInt) = b(2)
! !     cycle
! !
! !   !              the interior of the line from a to b
! !   !                 intersects with the iVertex-th line
! !
! !       30 NPtsInt = NPtsInt + 1
! !     if (NPtsInt > m) go to 210
! !     u(NPtsInt) = a(1) + s*dx
! !     v(NPtsInt) = a(2) + s*dy
! !     cycle
! !
! !   !-----------------------------------------------------------------------
! !   !                     the lines are parallel
! !   !-----------------------------------------------------------------------
! !
! !
! !     d = a(2) - y(iVertex)
! !     if (abs(d) <= Tol0*max(abs(a(2)),abs(y(iVertex)))) d = Zero
! !       s = d/dyi
! !
! !     p = x(iVertex) + s*dxi
! !     if (abs(a(1) - p) > Tol*max(abs(a(1)),abs(p))) cycle
! !
! !     d = b(2) - y(iVertex)
! !       if (abs(d) <= Tol0*max(abs(b(2)),abs(y(iVertex)))) d = Zero
! !     t = d/dyi
! !     go to 60
! !
! !     50 d = a(1) - x(iVertex)
! !     if (abs(d) <= Tol0*max(abs(a(1)),abs(x(iVertex)))) d = Zero
! !       s = d/dxi
! !
! !     p = y(iVertex) + s*dyi
! !     if (abs(p - a(2)) > Tol*max(abs(p),abs(a(2)))) cycle
! !
! !     d = b(1) - x(iVertex)
! !       if (abs(d) <= Tol0*max(abs(b(1)),abs(x(iVertex)))) d = Zero
! !     t = d/dxi
! !
! !   !              the 2 lines are portions of the same
! !   !                     straight infinite line
! !
! !       60 if (s > Zero .and. s < onem) go to 220
! !       if (t > Zero .and. t < onem) go to 220
! !       tmin = min(s,t)
! !       tmax = max(s,t)
! !       if (tmax <= Zero) go to 70
! !       if (tmin >= onem) go to 80
! !         go to 220
! !
! !       70 if (tmax < Zero) cycle
! !       if (NPtsInt > 0) cycle
! !       if (tmax == s) go to 10
! !       go to 21
! !
! !       80 if (tmin > 1.0) cycle
! !       if (tmin == s) go to 10
! !       go to 21
! !
! !     end do
! !     if (ind == 0) go to 200
! !
! !   if (NPtsInt < 2) return
! !   if (u(NPtsInt) == x(1) .and. v(NPtsInt) == y(1)) NPtsInt = NPtsInt - 1
! !   return
! !
! !   !                          error return
! !
! !   200 iErr = 1
! !   return
! !
! !   210 iErr = 2
! !   NPtsInt = NPtsInt - 1
! !     return
! !
! !   220 iErr = -iVertex
! !   return
! !
! ! End Subroutine

End SubModule