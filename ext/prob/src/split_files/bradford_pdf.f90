subroutine bradford_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! BRADFORD_PDF evaluates the Bradford PDF.
!
!  Discussion:
!
!    The formula is:
!
!      PDF(A,B,C;X) =
!        C / ( ( C * ( X - A ) + B - A ) * log ( C + 1 ) )
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
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    A <= X
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
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

  if ( x <= a ) then
    pdf = 0.0D+00
  else if ( x <= b ) then
    pdf = c / ( ( c * ( x - a ) + b - a ) * log ( c + 1.0D+00 ) )
  else if ( b < x ) then
    pdf = 0.0D+00
  end if

  return
end
