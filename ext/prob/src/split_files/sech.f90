function sech ( x )

!*****************************************************************************80
!
!! SECH returns the hyperbolic secant.
!
!  Discussion:
!
!    SECH ( X ) = 1.0D+00 / COSH ( X ) = 2.0D+00 / ( EXP ( X ) + EXP ( - X ) )
!
!    SECH is not a built-in function in FORTRAN, and occasionally it
!    is handier, or more concise, to be able to refer to it directly
!    rather than through its definition in terms of the sine function.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument.
!
!    Output, real ( kind = 8 ) SECH, the hyperbolic secant of X.
!
  implicit none

  real ( kind = 8 ) sech
  real ( kind = 8 ) x

  sech = 1.0D+00 / cosh ( x )

  return
end
