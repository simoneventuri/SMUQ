subroutine english_word_length_cdf_inv ( cdf, x )

!*****************************************************************************80
!
!! ENGLISH_WORD_LENGTH_CDF_INV inverts the English Word Length CDF.
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
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!    0.0 <= CDF <= 1.0.
!
!    Output, integer ( kind = 4 ) X, the corresponding word length for which
!    CDF(X-1) < CDF <= CDF(X)
!
  implicit none

  integer ( kind = 4 ), parameter :: word_length_max = 27

  real ( kind = 8 ) cdf
  real ( kind = 8 ) cum
  integer ( kind = 4 ) j
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

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ENGLISH_WORD_LENGTH_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  cum = 0.0D+00

  do j = 1, word_length_max

    cum = cum + pdf_vec(j)

    if ( cdf <= cum / pdf_sum ) then
      x = j
      return
    end if

  end do

  x = word_length_max

  return
end
