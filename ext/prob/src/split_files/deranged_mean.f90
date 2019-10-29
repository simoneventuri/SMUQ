subroutine deranged_mean ( a, mean )

!*****************************************************************************80
!
!! DERANGED_MEAN returns the mean of the Deranged CDF.
!
!  Discussion:
!
!    The mean is computed by straightforward summation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 January 2000
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
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) mean
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x

  mean = 0.0D+00
  do x = 0, a
    call deranged_pdf ( x, a, pdf )
    mean = mean + pdf * x
  end do

  return
end
