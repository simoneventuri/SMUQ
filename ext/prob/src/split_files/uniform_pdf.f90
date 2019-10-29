subroutine uniform_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! UNIFORM_PDF evaluates the Uniform PDF.
!
!  Discussion:
!
!    The Uniform PDF is also known as the "Rectangular" or "de Moivre" PDF.
!
!    PDF(A,B;X) = 1 / ( B - A ) for A <= X <= B
!               = 0 otherwise
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    A < B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  if ( x < a .or. b < x ) then
    pdf = 0.0D+00
  else
    pdf = 1.0D+00 / ( b - a )
  end if

  return
end
