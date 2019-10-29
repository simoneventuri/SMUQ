subroutine beta_binomial_cdf ( x, a, b, c, cdf )

!*****************************************************************************80
!
!! BETA_BINOMIAL_CDF evaluates the Beta Binomial CDF.
!
!  Discussion:
!
!    A simple summing approach is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Input, integer ( kind = 4 ) C, a parameter of the PDF.
!    0 <= C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) pdf
  real ( kind = 8 ) r8_beta
  integer ( kind = 4 ) x
  integer ( kind = 4 ) y

  if ( x < 0 ) then

    cdf = 0.0D+00

  else if ( x < c ) then

    cdf = 0.0D+00
    do y = 0, x
      pdf = r8_beta ( a + real ( y, kind = 8 ), &
        b + real ( c - y, kind = 8 ) ) / ( real ( c + 1, kind = 8 ) &
        * r8_beta ( real ( y + 1, kind = 8 ), &
        real ( c - y + 1, kind = 8 ) ) * r8_beta ( a, b ) )
      cdf = cdf + pdf
    end do

  else if ( c <= x ) then

    cdf = 1.0D+00

  end if

  return
end
