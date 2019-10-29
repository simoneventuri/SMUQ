subroutine deranged_variance ( a, variance )

!*****************************************************************************80
!
!! DERANGED_VARIANCE returns the variance of the Deranged CDF.
!
!  Discussion:
!
!    The variance is computed by straightforward summation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, the number of items.
!    1 <= A.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) mean
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x
  real ( kind = 8 ) variance

  call deranged_mean ( a, mean )

  variance = 0.0D+00
  do x = 0, a
    call deranged_pdf ( x, a, pdf )
    variance = variance + pdf * ( x - mean ) ** 2
  end do

  return
end
