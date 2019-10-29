subroutine uniform_discrete_pdf ( x, a, b, pdf )

!*****************************************************************************80
!
!! UNIFORM_DISCRETE_PDF evaluates the Uniform discrete PDF.
!
!  Discussion:
!
!    The Uniform Discrete PDF is also known as the "Rectangular"
!    Discrete PDF.
!
!    PDF(A,B;X) = 1 / ( B + 1 - A ) for A <= X <= B.
!
!    The parameters define the interval of integers
!    for which the PDF is nonzero.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!
!    Input, integer ( kind = 4 ) A, B, the parameters of the PDF.
!    A <= B.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) b
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x

  if ( x < a .or. b < x ) then
    pdf = 0.0D+00
  else
    pdf = 1.0D+00 / real ( b + 1 - a, kind = 8 )
  end if

  return
end
