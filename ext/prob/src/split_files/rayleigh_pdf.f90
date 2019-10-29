subroutine rayleigh_pdf ( x, a, pdf )

!*****************************************************************************80
!
!! RAYLEIGH_PDF evaluates the Rayleigh PDF.
!
!  Discussion:
!
!    PDF(A;X) = ( X / A^2 ) * EXP ( - X^2 / ( 2 * A^2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!    0.0 <= X
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    0 < A.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    pdf = 0.0D+00
  else
    pdf = ( x / a ** 2 ) * exp ( - x ** 2 / ( 2.0D+00 * a ** 2 ) )
  end if

  return
end
