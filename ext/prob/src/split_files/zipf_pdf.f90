subroutine zipf_pdf ( x, a, pdf )

!*****************************************************************************80
!
!! ZIPF_PDF evaluates the Zipf PDF.
!
!  Discussion:
!
!    PDF(A;X) = ( 1 / X^A ) / C
!
!    where the normalizing constant is chosen so that
!
!    C = Sum ( 1 <= I < Infinity ) 1 / I^A.
!
!    From observation, the frequency of different words in long
!    sequences of text seems to follow the Zipf PDF, with
!    parameter A slightly greater than 1.  The Zipf PDF is sometimes
!    known as the "discrete Pareto" PDF.
!
!    Lotka's law is a version of the Zipf PDF in which A is 2 or approximately
!    2.  Lotka's law describes the frequency of publications by authors in a
!    given field, and estimates that the number of authors with X papers is
!    about 1/X^A of the number of authors with 1 paper.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Alfred Lotka,
!    The frequency distribution of scientific productivity,
!    Journal of the Washington Academy of Sciences,
!    Volume 16, Number 12, 1926, pages 317-324.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!    1 <= N
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1.0D+00 < A.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), save :: asave = 0.0D+00
  real ( kind = 8 ), save :: c = 0.0D+00
  real ( kind = 8 ) pdf
  real ( kind = 8 ) r8_zeta
  integer ( kind = 4 ) x

  if ( x < 1 ) then

    pdf = 0.0D+00

  else

    if ( a /= asave ) then

      c = r8_zeta ( a )
      asave = a

    end if

    pdf = ( 1.0D+00 / real ( x, kind = 8 ) ** a ) / c

  end if

  return
end
