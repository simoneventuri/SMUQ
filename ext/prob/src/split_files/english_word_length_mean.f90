subroutine english_word_length_mean ( mean )

!*****************************************************************************80
!
!! ENGLISH_WORD_LENGTH_MEAN evaluates the mean of the English Word Length PDF.
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
!    Output, real ( kind = 8 ) MEAN, the mean of the PDF.
!
  implicit none

  integer ( kind = 4 ), parameter :: word_length_max = 27

  integer ( kind = 4 ) j
  real ( kind = 8 ) mean
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

  mean = 0.0D+00
  do j = 1, word_length_max
    mean = mean + real ( j, kind = 8 ) * pdf_vec(j)
  end do

  mean = mean / pdf_sum

  return
end
