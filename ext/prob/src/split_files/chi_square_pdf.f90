subroutine chi_square_pdf ( x, a, pdf )

!*****************************************************************************80
!
!! CHI_SQUARE_PDF evaluates the central Chi squared PDF.
!
!  Discussion:
!
!    PDF(A;X) =
!      EXP ( - X / 2 ) * X^((A-2)/2) / ( 2^(A/2) * GAMMA ( A/2 ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 1999
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
!    1 <= A.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  if ( x < 0.0D+00 ) then
    pdf = 0.0D+00
  else
    b = a / 2.0D+00
    pdf = exp ( - 0.5D+00 * x ) * x ** ( b - 1.0D+00 ) &
      / ( 2.0D+00 ** b * gamma ( b ) )
  end if

  return
end
