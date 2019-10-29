subroutine erlang_cdf ( x, a, b, c, cdf )

!*****************************************************************************80
!
!! ERLANG_CDF evaluates the Erlang CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, real ( kind = 8 ) A, B, integer C, the parameters of the PDF.
!    0.0 < B.
!    0 < C.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) p2
  real ( kind = 8 ) r8_gamma_inc
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  if ( x < a ) then

    cdf = 0.0D+00

  else

    x2 = ( x - a ) / b
    p2 = real ( c, kind = 8 )

    cdf = r8_gamma_inc ( p2, x2 )

  end if

  return
end
