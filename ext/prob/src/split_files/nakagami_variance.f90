subroutine nakagami_variance ( a, b, c, variance )

!*****************************************************************************80
!
!! NAKAGAMI_VARIANCE returns the variance of the Nakagami PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B
!    0.0 < C
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) t1
  real ( kind = 8 ) t2
  real ( kind = 8 ) variance

  t1 = gamma ( c + 0.5D+00 )
  t2 = gamma ( c )

  variance = b * b * ( 1.0D+00 - t1 * t1 / ( c * t2 * t2 ) )

  return
end
