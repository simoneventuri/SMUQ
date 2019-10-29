subroutine i4vec_mean ( n, x, mean )

!*****************************************************************************80
!
!! I4VEC_MEAN returns the mean of an I4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, integer ( kind = 4 ) X(N), the vector whose mean is desired.
!
!    Output, real ( kind = 8 ) MEAN, the mean, or average, of
!    the vector entries.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) mean
  integer ( kind = 4 ) x(n)

  mean = real ( sum ( x(1:n) ), kind = 8 ) / real ( n, kind = 8 )

  return
end
