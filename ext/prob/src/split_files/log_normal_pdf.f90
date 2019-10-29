subroutine log_normal_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! LOG_NORMAL_PDF evaluates the Lognormal PDF.
!
!  Discussion:
!
!    PDF(A,B;X)
!      = exp ( - 0.5 * ( ( log ( X ) - A ) / B )^2 )
!        / ( B * X * sqrt ( 2 * PI ) )
!
!    The Lognormal PDF is also known as the Cobb-Douglas PDF,
!    and as the Antilog_normal PDF.
!
!    The Lognormal PDF describes a variable X whose logarithm
!    is normally distributed.
!
!    The special case A = 0, B = 1 is known as Gilbrat's PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 < X
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  if ( x <= 0.0D+00 ) then
    pdf = 0.0D+00
  else
    pdf = exp ( - 0.5D+00 * ( ( log ( x ) - a ) / b ) ** 2 ) &
      / ( b * x * sqrt ( 2.0D+00 * r8_pi ) )
  end if

  return
end
