subroutine dirichlet_variance ( n, a, variance )

!*****************************************************************************80
!
!! DIRICHLET_VARIANCE returns the variances of the Dirichlet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be nonnegative, and at least one should be positive.
!
!    Output, real ( kind = 8 ) VARIANCE(N), the variances of the PDF.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_sum
  integer ( kind = 4 ) i
  real ( kind = 8 ) variance(n)

  a_sum = sum ( a(1:n) )

  do i = 1, n
    variance(i) = a(i) * ( a_sum - a(i) ) / ( a_sum ** 2 * ( a_sum + 1.0D+00 ) )
  end do

  return
end
