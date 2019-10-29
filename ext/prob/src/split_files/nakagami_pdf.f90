subroutine nakagami_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! NAKAGAMI_PDF evaluates the Nakagami PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B
!    0.0 < C.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x
  real ( kind = 8 ) y

  if ( x <= 0.0D+00 ) then

    pdf = 0.0D+00

  else if ( 0.0D+00 < x ) then

    y = ( x - a ) / b

    pdf = 2.0D+00 * c ** c / ( b * gamma ( c ) ) &
      * y ** ( 2.0D+00 * c - 1.0D+00 ) &
      * exp ( - c * y * y )

  end if

  return
end
