subroutine uniform_discrete_mean ( a, b, mean )

!*****************************************************************************80
!
!! UNIFORM_DISCRETE_MEAN returns the mean of the Uniform discrete PDF.
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
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  real ( kind = 8 ) mean

  mean = 0.5D+00 * real ( a + b, kind = 8 )

  return
end
