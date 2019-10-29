subroutine burr_pdf ( x, a, b, c, d, pdf )

!*****************************************************************************80
!
!! BURR_PDF evaluates the Burr PDF.
!
!  Discussion:
!
!    Y = ( X - A ) / B;
!
!    PDF(X)(A,B,C,D) = ( C * D / B ) * Y ^ ( C - 1 ) / ( 1 + Y ^ C ) ^ ( D + 1 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Mark Johnson,
!    Multivariate Statistical Simulation,
!    Wiley, 1987.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, D, the parameters of the PDF.
!    0 < B,
!    0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( x <= a ) then
    pdf = 0.0D+00
  else

    y = ( x - a ) / b

    pdf = ( c * d / b ) * y ** ( c - 1.0D+00 ) &
      / ( 1.0D+00 + y ** c ) ** ( d + 1.0D+00 )

  end if

  return
end
