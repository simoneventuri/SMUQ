function r8_error_f ( x )

!*****************************************************************************80
!
!! R8_ERROR_F evaluates the error function ERF.
!
!  Discussion:
!
!    Since some compilers already supply a routine named ERF which evaluates
!    the error function, this routine has been given a distinct, if
!    somewhat unnatural, name.
!
!    The function is defined by:
!
!      ERF(X) = ( 2 / sqrt ( PI ) ) * Integral ( 0 <= T <= X ) EXP ( - T^2 ) dT.
!
!    Properties of the function include:
!
!      Limit ( X -> -Infinity ) ERF(X) =          -1.0;
!                               ERF(0) =           0.0;
!                               ERF(0.476936...) = 0.5;
!      Limit ( X -> +Infinity ) ERF(X) =          +1.0.
!
!      0.5D+00 * ( ERF(X/sqrt(2)) + 1 ) = Normal_01_CDF(X)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 November 2006
!
!  Author:
!
!    Original FORTRAN77 version by William Cody.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    William Cody,
!    Rational Chebyshev Approximations for the Error Function,
!    Mathematics of Computation,
!    1969, pages 631-638.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the error function.
!
!    Output, real ( kind = 8 ) R8_ERROR_F, the value of the error function.
!
  implicit none

  real ( kind = 8 ), parameter, dimension ( 5 ) :: a = (/ &
    3.16112374387056560D+00, &
    1.13864154151050156D+02, &
    3.77485237685302021D+02, &
    3.20937758913846947D+03, &
    1.85777706184603153D-01 /)
  real ( kind = 8 ), parameter, dimension ( 4 ) :: b = (/ &
    2.36012909523441209D+01, &
    2.44024637934444173D+02, &
    1.28261652607737228D+03, &
    2.84423683343917062D+03 /)
  real ( kind = 8 ), parameter, dimension ( 9 ) :: c = (/ &
    5.64188496988670089D-01, &
    8.88314979438837594D+00, &
    6.61191906371416295D+01, &
    2.98635138197400131D+02, &
    8.81952221241769090D+02, &
    1.71204761263407058D+03, &
    2.05107837782607147D+03, &
    1.23033935479799725D+03, &
    2.15311535474403846D-08 /)
  real ( kind = 8 ), parameter, dimension ( 8 ) :: d = (/ &
    1.57449261107098347D+01, &
    1.17693950891312499D+02, &
    5.37181101862009858D+02, &
    1.62138957456669019D+03, &
    3.29079923573345963D+03, &
    4.36261909014324716D+03, &
    3.43936767414372164D+03, &
    1.23033935480374942D+03 /)
  real ( kind = 8 ) del
  integer ( kind = 4 ) i
  real ( kind = 8 ), parameter, dimension ( 6 ) :: p = (/ &
    3.05326634961232344D-01, &
    3.60344899949804439D-01, &
    1.25781726111229246D-01, &
    1.60837851487422766D-02, &
    6.58749161529837803D-04, &
    1.63153871373020978D-02 /)
  real ( kind = 8 ), parameter, dimension ( 5 ) :: q = (/ &
    2.56852019228982242D+00, &
    1.87295284992346047D+00, &
    5.27905102951428412D-01, &
    6.05183413124413191D-02, &
    2.33520497626869185D-03 /)
  real ( kind = 8 ) r8_error_f
  real ( kind = 8 ), parameter :: sqrpi = 0.56418958354775628695D+00
  real ( kind = 8 ), parameter :: thresh = 0.46875D+00
  real ( kind = 8 ) x
  real ( kind = 8 ) xabs
  real ( kind = 8 ), parameter :: xbig = 26.543D+00
  real ( kind = 8 ) xden
  real ( kind = 8 ) xnum
  real ( kind = 8 ) xsq

  xabs = abs ( ( x ) )
!
!  Evaluate ERF(X) for |X| <= 0.46875.
!
  if ( xabs <= thresh ) then

    if ( epsilon ( xabs ) < xabs ) then
      xsq = xabs * xabs
    else
      xsq = 0.0D+00
    end if

    xnum = a(5) * xsq
    xden = xsq
    do i = 1, 3
      xnum = ( xnum + a(i) ) * xsq
      xden = ( xden + b(i) ) * xsq
    end do

    r8_error_f = x * ( xnum + a(4) ) / ( xden + b(4) )
!
!  Evaluate ERFC(X) for 0.46875 <= |X| <= 4.0.
!
  else if ( xabs <= 4.0D+00 ) then

    xnum = c(9) * xabs
    xden = xabs
    do i = 1, 7
      xnum = ( xnum + c(i) ) * xabs
      xden = ( xden + d(i) ) * xabs
    end do

    r8_error_f = ( xnum + c(8) ) / ( xden + d(8) )
    xsq = real ( int ( xabs * 16.0D+00 ), kind = 8 ) / 16.0D+00
    del = ( xabs - xsq ) * ( xabs + xsq )
    r8_error_f = exp ( - xsq * xsq ) * exp ( - del ) * r8_error_f

    r8_error_f = ( 0.5D+00 - r8_error_f ) + 0.5D+00

    if ( x < 0.0D+00 ) then
      r8_error_f = - r8_error_f
    end if
!
!  Evaluate ERFC(X) for 4.0D+00 < |X|.
!
  else

    if ( xbig <= xabs ) then

      if ( 0.0D+00 < x ) then
        r8_error_f = 1.0D+00
      else
        r8_error_f = - 1.0D+00
      end if

    else

      xsq = 1.0D+00 / ( xabs * xabs )

      xnum = p(6) * xsq
      xden = xsq
      do i = 1, 4
        xnum = ( xnum + p(i) ) * xsq
        xden = ( xden + q(i) ) * xsq
      end do

      r8_error_f = xsq * ( xnum + p(5) ) / ( xden + q(5) )
      r8_error_f = ( sqrpi - r8_error_f ) / xabs
      xsq = real ( int ( xabs * 16.0D+00 ), kind = 8 ) / 16.0D+00
      del = ( xabs - xsq ) * ( xabs + xsq )
      r8_error_f = exp ( - xsq * xsq ) * exp ( - del ) * r8_error_f

      r8_error_f = ( 0.5D+00 - r8_error_f ) + 0.5D+00

      if ( x < 0.0D+00 ) then
        r8_error_f = - r8_error_f
      end if

    end if

  end if

  return
end
