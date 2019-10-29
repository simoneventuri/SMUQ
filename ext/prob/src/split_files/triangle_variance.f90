subroutine triangle_variance ( a, b, c, variance )

!*****************************************************************************80
!
!! TRIANGLE_VARIANCE returns the variance of the Triangle PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A <= B <= C and A < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) variance

  variance = ( ( c - a ) * ( c - a ) &
             - ( c - a ) * ( b - a ) &
             + ( b - a ) * ( b - a ) ) / 18.0D+00

  return
end
