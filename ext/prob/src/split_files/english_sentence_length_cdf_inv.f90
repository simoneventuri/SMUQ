subroutine english_sentence_length_cdf_inv ( cdf, x )

!*****************************************************************************80
!
!! ENGLISH_SENTENCE_LENGTH_CDF_INV inverts the English Sentence Length CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 July 2006
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
!    Output, integer ( kind = 4 ) X, the corresponding sentence length for which
!    CDF(X-1) < CDF <= CDF(X)
!
  implicit none

  integer ( kind = 4 ), parameter :: sentence_length_max = 79

  real ( kind = 8 ) cdf
  real ( kind = 8 ) cum
  integer ( kind = 4 ) j
  real ( kind = 8 ), dimension ( sentence_length_max ) :: pdf_vec = (/ &
    0.00806D+00, &
    0.01370D+00, &
    0.01862D+00, &
    0.02547D+00, &
    0.03043D+00, &
    0.03189D+00, &
    0.03516D+00, &
    0.03545D+00, &
    0.03286D+00, &
    0.03533D+00, &
    0.03562D+00, &
    0.03788D+00, &
    0.03669D+00, &
    0.03751D+00, &
    0.03518D+00, &
    0.03541D+00, &
    0.03434D+00, &
    0.03305D+00, &
    0.03329D+00, &
    0.03103D+00, &
    0.02867D+00, &
    0.02724D+00, &
    0.02647D+00, &
    0.02526D+00, &
    0.02086D+00, &
    0.02178D+00, &
    0.02128D+00, &
    0.01801D+00, &
    0.01690D+00, &
    0.01556D+00, &
    0.01512D+00, &
    0.01326D+00, &
    0.01277D+00, &
    0.01062D+00, &
    0.01051D+00, &
    0.00901D+00, &
    0.00838D+00, &
    0.00764D+00, &
    0.00683D+00, &
    0.00589D+00, &
    0.00624D+00, &
    0.00488D+00, &
    0.00477D+00, &
    0.00406D+00, &
    0.00390D+00, &
    0.00350D+00, &
    0.00318D+00, &
    0.00241D+00, &
    0.00224D+00, &
    0.00220D+00, &
    0.00262D+00, &
    0.00207D+00, &
    0.00174D+00, &
    0.00174D+00, &
    0.00128D+00, &
    0.00121D+00, &
    0.00103D+00, &
    0.00117D+00, &
    0.00124D+00, &
    0.00082D+00, &
    0.00088D+00, &
    0.00061D+00, &
    0.00061D+00, &
    0.00075D+00, &
    0.00063D+00, &
    0.00056D+00, &
    0.00052D+00, &
    0.00057D+00, &
    0.00031D+00, &
    0.00029D+00, &
    0.00021D+00, &
    0.00017D+00, &
    0.00021D+00, &
    0.00034D+00, &
    0.00031D+00, &
    0.00011D+00, &
    0.00011D+00, &
    0.00008D+00, &
    0.00006D+00 /)
  real ( kind = 8 ), parameter :: pdf_sum = 0.99768D+00
  integer ( kind = 4 ) x

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ENGLISH_SENTENCE_LENGTH_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    stop 1
  end if

  cum = 0.0D+00

  do j = 1, sentence_length_max

    cum = cum + pdf_vec(j)

    if ( cdf <= cum / pdf_sum ) then
      x = j
      return
    end if

  end do

  x = sentence_length_max

  return
end
