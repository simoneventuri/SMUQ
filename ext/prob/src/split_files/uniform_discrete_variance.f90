subroutine uniform_discrete_variance ( a, b, variance )

!*****************************************************************************80
!
!! UNIFORM_DISCRETE_VARIANCE returns the variance of the Uniform discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, B, the parameters of the PDF.
!    A <= B.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  real ( kind = 8 ) variance

  variance = real ( ( b + 1 - a ) ** 2 - 1, kind = 8 ) / 12.0D+00

  return
end
