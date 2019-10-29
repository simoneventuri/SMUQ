subroutine discrete_mean ( a, b, mean )

!*****************************************************************************80
!
!! DISCRETE_MEAN evaluates the mean of the Discrete PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of probabilities assigned.
!
!    Input, real ( kind = 8 ) B(A), the relative probabilities of
!    outcomes 1 through A.  Each entry must be nonnegative.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  integer ( kind = 4 ) a

  real ( kind = 8 ) b(a)
  real ( kind = 8 ) b_sum
  integer ( kind = 4 ) j
  real ( kind = 8 ) mean

  b_sum = sum ( b(1:a) )

  mean = 0.0D+00
  do j = 1, a
    mean = mean + real ( j, kind = 8 ) * b(j)
  end do

  mean = mean / b_sum

  return
end
