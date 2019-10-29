function r8_zeta ( p )

!*****************************************************************************80
!
!! R8_ZETA estimates the Riemann Zeta function.
!
!  Discussion:
!
!    For 1 < P, the Riemann Zeta function is defined as:
!
!      ZETA ( P ) = Sum ( 1 <= N < Infinity ) 1 / N^P
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
!    Daniel Zwillinger, editor,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition,
!    CRC Press, 1996.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) P, the power to which the integers are raised.
!    P must be greater than 1.  For integral P up to 20, a
!    precomputed value of ZETA is returned; otherwise the infinite
!    sum is approximated.
!
!    Output, real ( kind = 8 ) R8_ZETA, an approximation to the Riemann
!    Zeta function.
!
  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) p
  real ( kind = 8 ), parameter :: r8_huge = 1.0D+30
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) r8_zeta
  real ( kind = 8 ) zsum
  real ( kind = 8 ) zsum_old

  if ( p <= 1.0D+00 ) then
    r8_zeta = r8_huge
  else if ( p == 2.0D+00 ) then
    r8_zeta = r8_pi ** 2 / 6.0D+00
  else if ( p == 3.0D+00 ) then
    r8_zeta = 1.2020569032D+00
  else if ( p == 4.0D+00 ) then
    r8_zeta = r8_pi ** 4 / 90.0D+00
  else if ( p == 5.0D+00 ) then
    r8_zeta = 1.0369277551D+00
  else if ( p == 6.0D+00 ) then
    r8_zeta = r8_pi ** 6 / 945.0D+00
  else if ( p == 7.0D+00 ) then
    r8_zeta = 1.0083492774D+00
  else if ( p == 8.0D+00 ) then
    r8_zeta = r8_pi ** 8 / 9450.0D+00
  else if ( p == 9.0D+00 ) then
    r8_zeta = 1.0020083928D+00
  else if ( p == 10.0D+00 ) then
    r8_zeta = r8_pi ** 10 / 93555.0D+00
  else if ( p == 11.0D+00 ) then
    r8_zeta = 1.0004941886D+00
  else if ( p == 12.0D+00 ) then
    r8_zeta = 1.0002460866D+00
  else if ( p == 13.0D+00 ) then
    r8_zeta = 1.0001227133D+00
  else if ( p == 14.0D+00 ) then
    r8_zeta = 1.0000612482D+00
  else if ( p == 15.0D+00 ) then
    r8_zeta = 1.0000305882D+00
  else if ( p == 16.0D+00 ) then
    r8_zeta = 1.0000152823D+00
  else if ( p == 17.0D+00 ) then
    r8_zeta = 1.0000076372D+00
  else if ( p == 18.0D+00 ) then
    r8_zeta = 1.0000038173D+00
  else if ( p == 19.0D+00 ) then
    r8_zeta = 1.0000019082D+00
  else if ( p == 20.0D+00 ) then
    r8_zeta = 1.0000009540D+00
  else

    zsum = 0.0D+00
    n = 0

    do

      n = n + 1
      zsum_old = zsum
      zsum = zsum + 1.0D+00 / ( real ( n, kind = 8 ) ) ** p
      if ( zsum <= zsum_old ) then
        exit
      end if

    end do

    r8_zeta = zsum

  end if

  return
end
