function uniform_01_sample ( seed )

!*****************************************************************************80
!
!! UNIFORM_01_SAMPLE is a portable random number generator.
!
!  Discussion:
!
!    SEED = SEED * (7^5) mod ( 2^31 - 1 )
!    UNIFORM_01_SAMPLE = SEED * / ( 2^31 - 1 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED, the integer "seed" used to
!    generate the output random number, and updated in preparation for the
!    next one.  SEED should not be zero.
!
!    Output, real ( kind = 8 ) UNIFORM_01_SAMPLE, a random value between 0
!    and 1.
!
!  Local parameters:
!
!    IA = 7^5
!    IB = 2^15
!    IB16 = 2^16
!    IP = 2^31-1
!
  implicit none

  integer ( kind = 4 ), parameter :: ia = 16807
  integer ( kind = 4 ), parameter :: ib15 = 32768
  integer ( kind = 4 ), parameter :: ib16 = 65536
  integer ( kind = 4 ), parameter :: ip = 2147483647
  integer ( kind = 4 ) iprhi
  integer ( kind = 4 ) ixhi
  integer ( kind = 4 ) k
  integer ( kind = 4 ) leftlo
  integer ( kind = 4 ) loxa
  integer ( kind = 4 ) seed
  real ( kind = 8 ) uniform_01_sample
!
!  Don't let SEED be 0 or IP
!
  if ( seed == 0 .or. seed == ip ) then
    seed = ip / 2
  end if
!
!  Get the 15 high order bits of SEED.
!
  ixhi = seed / ib16
!
!  Get the 16 low bits of SEED and form the low product.
!
  loxa = ( seed - ixhi * ib16 ) * ia
!
!  Get the 15 high order bits of the low product.
!
  leftlo = loxa / ib16
!
!  Form the 31 highest bits of the full product.
!
  iprhi = ixhi * ia + leftlo
!
!  Get overflow past the 31st bit of full product.
!
  k = iprhi / ib15
!
!  Assemble all the parts and presubtract IP.  The parentheses are essential.
!
  seed = ( ( ( loxa - leftlo * ib16 ) - ip ) + ( iprhi - k * ib15 ) * ib16 ) &
    + k
!
!  Add IP back in if necessary.
!
  if ( seed < 0 ) then
    seed = seed + ip
  end if
!
!  Multiply by 1 / (2^31-1).
!
  uniform_01_sample = real ( seed, kind = 8 ) * 4.656612875D-10

  return
end
