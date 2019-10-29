subroutine english_letter_cdf ( c, cdf )

!*****************************************************************************80
!
!! ENGLISH_LETTER_CDF evaluates the English Letter CDF.
!
!  Discussion:
!
!    CDF('c') = 0.12441
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
!    Output, real ( kind = 8 ) CDF, the probability that a random letter is less
!    than or equal to C.
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

  if ( 'a' <= c .and. c <= 'z' ) then
    i = iachar ( c ) - iachar ( 'a' ) + 2
    cdf = cdf_vec(i)
  else if ( 'A' <= c .and. c <= 'Z' ) then
    i = iachar ( c ) - iachar ( 'A' ) + 2
    cdf = cdf_vec(i)
  else
    cdf = 0.0D+00
  end if

  return
end
