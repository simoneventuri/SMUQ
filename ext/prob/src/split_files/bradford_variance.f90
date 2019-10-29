subroutine bradford_variance ( a, b, c, variance )

!*****************************************************************************80
!
!! BRADFORD_VARIANCE returns the variance of the Bradford PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0 < C.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) variance

  variance = ( b - a ) ** 2 * &
    ( c * ( log ( c + 1.0D+00 ) - 2.0D+00 ) + 2.0D+00 * log ( c + 1.0D+00 ) ) &
    / ( 2.0D+00 * c * ( log ( c + 1.0D+00 ) ) ** 2 )

  return
end
