subroutine triangle_mean ( a, b, c, mean )

!*****************************************************************************80
!
!! TRIANGLE_MEAN returns the mean of the Triangle PDF.
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
!    Output, real ( kind = 8 ) MEAN, the mean of the discrete uniform PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) mean

  mean = a + ( c + b - 2.0D+00 * a ) / 3.0D+00

  return
end
