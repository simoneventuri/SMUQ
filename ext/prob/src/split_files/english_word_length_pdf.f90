subroutine english_word_length_pdf ( x, pdf )

!*****************************************************************************80
!
!! ENGLISH_WORD_LENGTH_PDF evaluates the English Word Length PDF.
!
!  Discussion:
!
!    PDF(A,B;X) = B(X) if 1 <= X <= A
!                = 0    otherwise
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Henry Kucera, Winthrop Francis,
!    Computational Analysis of Present-Day American English,
!    Brown University Press, 1967.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the word length whose probability
!    is desired.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ), parameter :: word_length_max = 27

  real ( kind = 8 ) pdf
  real ( kind = 8 ), dimension ( word_length_max ) :: pdf_vec = (/ &
    0.03160D+00, &
    0.16975D+00, &
    0.21192D+00, &
    0.15678D+00, &
    0.10852D+00, &
    0.08524D+00, &
    0.07724D+00, &
    0.05623D+00, &
    0.04032D+00, &
    0.02766D+00, &
    0.01582D+00, &
    0.00917D+00, &
    0.00483D+00, &
    0.00262D+00, &
    0.00099D+00, &
    0.00050D+00, &
    0.00027D+00, &
    0.00022D+00, &
    0.00011D+00, &
    0.00006D+00, &
    0.00005D+00, &
    0.00002D+00, &
    0.00001D+00, &
    0.00001D+00, &
    0.00001D+00, &
    0.00001D+00, &
    0.00001D+00 /)
  real ( kind = 8 ), parameter :: pdf_sum = 0.99997D+00
  integer ( kind = 4 ) x

  if ( 1 <= x .and. x <= word_length_max ) then
    pdf = pdf_vec(x) / pdf_sum
  else
    pdf = 0.0D+00
  end if

  return
end
