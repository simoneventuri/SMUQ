subroutine i4vec_variance ( n, x, variance )

!*****************************************************************************80
!
!! I4VEC_VARIANCE returns the variance of an I4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, integer ( kind = 4 ) X(N), the vector whose variance is desired.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the vector entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) mean
  real ( kind = 8 ) variance
  integer ( kind = 4 ) x(n)

  call i4vec_mean ( n, x, mean )

  variance = sum ( ( real ( x(1:n), kind = 8 ) - mean ) ** 2 )

  if ( 1 < n ) then
    variance = variance / real ( n - 1, kind = 8 )
  else
    variance = 0.0D+00
  end if

  return
end
