subroutine gumbel_pdf ( x, pdf )

!*****************************************************************************80
!
!! GUMBEL_PDF evaluates the Gumbel PDF.
!
!  Discussion:
!
!    PDF(X) = exp ( -X ) * exp ( - exp ( -X  ) ).
!
!    GUMBEL_PDF(X) = EXTREME_PDF(0,1;X)
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
!  Reference:
!
!    Eric Weisstein, editor,
!    CRC Concise Encylopedia of Mathematics,
!    CRC Press, 1998.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  pdf = exp ( - x - exp ( - x ) )

  return
end
