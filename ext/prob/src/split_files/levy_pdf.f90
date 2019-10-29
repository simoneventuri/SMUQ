subroutine levy_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! LEVY_PDF evaluates the Levy PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = sqrt ( B / ( 2 * PI ) )
!               * exp ( - B / ( 2 * ( X - A ) )
!               / ( X - A )^(3/2)
!
!    for A <= X.
!
!    Note that the Levy PDF does not have a finite mean or variance.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 April 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    Normally, A <= X.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0 < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) x

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LEVY_PDF - Fatal error!'
    write ( *, '(a)' ) '  Input parameter B <= 0.0'
    stop 1
  end if

  if ( x <= a ) then
    pdf = 0.0D+00
  else
    pdf = sqrt ( b / ( 2.0D+00 * r8_pi ) ) &
        * exp ( - b / ( 2.0D+00 * ( x - a ) ) ) &
        / sqrt ( ( x - a ) ** 3 )
  end if

  return
end
