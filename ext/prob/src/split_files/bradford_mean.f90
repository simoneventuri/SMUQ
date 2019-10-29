subroutine bradford_mean ( a, b, c, mean )

!*****************************************************************************80
!
!! BRADFORD_MEAN returns the mean of the Bradford PDF.
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
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) mean

  mean = ( c * ( b - a ) + log ( c + 1.0D+00 ) * ( a * ( c + 1.0D+00 ) - b ) ) &
    / ( c * log ( c + 1.0D+00 ) )

  return
end
