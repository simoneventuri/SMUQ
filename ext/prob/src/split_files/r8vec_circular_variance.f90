subroutine r8vec_circular_variance ( n, x, circular_variance )

!*****************************************************************************80
!
!! R8VEC_CIRCULAR_VARIANCE returns the circular variance of an R8VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) X(N), the vector whose variance is desired.
!
!    Output, real ( kind = 8 ) CIRCULAR VARIANCE, the circular variance
!    of the vector entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) circular_variance
  real ( kind = 8 ) mean
  real ( kind = 8 ) x(n)

  call r8vec_mean ( n, x, mean )

  circular_variance = &
      ( sum ( cos ( x(1:n) - mean ) ) ) ** 2 &
    + ( sum ( sin ( x(1:n) - mean ) ) ) ** 2

  circular_variance = sqrt ( circular_variance ) / real ( n, kind = 8 )

  circular_variance = 1.0D+00 - circular_variance

  return
end
