subroutine reciprocal_variance ( a, b, variance )

!*****************************************************************************80
!
!! RECIPROCAL_VARIANCE returns the variance of the Reciprocal PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A <= B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) d
  real ( kind = 8 ) variance

  d = log ( a / b )

  variance = ( a - b )* ( a * ( d - 2.0D+00 ) &
    + b * ( d + 2.0D+00 ) ) / ( 2.0D+00 * d ** 2 )

  return
end
