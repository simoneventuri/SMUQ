subroutine pearson_05_mean ( a, b, c, mean )

!*****************************************************************************80
!
!! PEARSON_05_MEAN evaluates the mean of the Pearson 5 PDF.
!
!  Discussion:
!
!    The mean is undefined for B <= 1.
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
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < A, 0.0 < B.
!
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) mean

  if ( b <= 1.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PEARSON_05_MEAN - Warning!'
    write ( *, '(a)' ) '  MEAN undefined for B <= 1.'
    mean = c
    return
  end if

  mean = c + a / ( b - 1.0D+00 )

  return
end
