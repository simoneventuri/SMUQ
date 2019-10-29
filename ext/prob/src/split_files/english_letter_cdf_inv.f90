subroutine english_letter_cdf_inv ( cdf, c )

!*****************************************************************************80
!
!! ENGLISH_LETTER_CDF_INV inverts the English Letter CDF.
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
!    Input, real ( kind = 8 ) CDF, a cumulative probability between 0 and 1.
!
!    Input, character C, the corresponding letter.
!
  implicit none

  character c
  real ( kind = 8 ) cdf
  real ( kind = 8 ), save :: cdf_vec(27) = (/ &
    0.00000, &
    0.08167, 0.09659, 0.12441, 0.16694, 0.29396, &
    0.31624, 0.33639, 0.39733, 0.46699, 0.46852, &
    0.47624, 0.51649, 0.54055, 0.60804, 0.68311, &
    0.70240, 0.70335, 0.76322, 0.82649, 0.91705, &
    0.94463, 0.95441, 0.97802, 0.97952, 0.99926, &
    1.00000 /)
  integer ( kind = 4 ) i

  c = ' '

  do i = 2, 27
    if ( cdf <= cdf_vec(i) ) then
      c = achar ( iachar ( 'a' ) + i - 2 )
      exit
    end if
  end do

  return
end
