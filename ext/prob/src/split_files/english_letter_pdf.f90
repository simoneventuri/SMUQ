subroutine english_letter_pdf ( c, pdf )

!*****************************************************************************80
!
!! ENGLISH_LETTER_PDF evaluates the English Letter PDF.
!
!  Discussion:
!
!    PDF('c') = 0.02782
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Robert Lewand,
!    Cryptological Mathematics,
!    Mathematics Association of America, 2000,
!    ISBN13: 978-0883857199
!
!  Parameters:
!
!    Input, character C, the letter whose probability is desired.
!    'a' <= c <= 'z', but case is ignored.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  character c
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  real ( kind = 8 ), save :: pdf_vec(26) = (/ &
    0.08167, 0.01492, 0.02782, 0.04253, 0.12702, &
    0.02228, 0.02015, 0.06094, 0.06966, 0.00153, &
    0.00772, 0.04025, 0.02406, 0.06749, 0.07507, &
    0.01929, 0.00095, 0.05987, 0.06327, 0.09056, &
    0.02758, 0.00978, 0.02361, 0.00150, 0.01974, &
    0.00074 /)

  if ( 'a' <= c .and. c <= 'z' ) then
    i = iachar ( c ) - iachar ( 'a' ) + 1
    pdf = pdf_vec(i)
  else if ( 'A' <= c .and. c <= 'Z' ) then
    i = iachar ( c ) - iachar ( 'A' ) + 1
    pdf = pdf_vec(i)
  else
    pdf = 0.0D+00
  end if

  return
end
