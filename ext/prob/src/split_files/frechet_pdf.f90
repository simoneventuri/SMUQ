subroutine frechet_pdf ( x, alpha, pdf )

!*****************************************************************************80
!
!! FRECHET_PDF evaluates the Frechet PDF.
!
!  Discussion:
!
!    PDF(X) = ALPHA * exp ( -1 / X^ALPHA ) / X^(ALPHA+1)
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 September 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) ALPHA, the parameter.
!    It is required that 0.0 < ALPHA.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) alpha
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  if ( alpha <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'FRECHET_PDF - Fatal error!'
    write ( *, '(a)' ) '  ALPHA <= 0.0.'
    stop 1
  end if

  pdf = alpha * exp ( - 1.0D+00 / x ** alpha ) / x ** ( alpha + 1.0D+00 )

  return
end
