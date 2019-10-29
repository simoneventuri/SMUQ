subroutine fermi_dirac_sample ( u, v, seed, z )

!*****************************************************************************80
!
!! FERMI_DIRAC_SAMPLE samples a (continuous) Fermi-Dirac distribution.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 January 2008
!
!  Author:
!
!    Original BASIC version by Frederick Ruckdeschel.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Frederick Ruckdeschel,
!    BASIC Scientific Subroutines,
!    Volume I,
!    McGraw Hill, 1980,
!    ISBN: 0-07-054201-5,
!    LC: QA76.95.R82.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) U, V, the parameters of the distribution.
!    The value of U represents the halfway point for the distribution.
!    Half the probability is to the left, and half to the right, of
!    the value U.  The value of V controls the shape of the distribution.
!    The ratio U/V determines the relative shape of the distribution.
!    Values of U/V in excess of 100 will risk overflow.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) Z, a sample from the Fermi-Dirac distribution.
!    Output values will be nonnegative, and roughly half of them should
!    be less than or equal to U.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ), parameter :: iter_max = 1000
  integer ( kind = 4 ) iter_num
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) x
  real ( kind = 8 ) y
  real ( kind = 8 ) y1
  real ( kind = 8 ) z

  x = r8_uniform_01 ( seed )
  y = 1.0D+00
  a = exp ( 4.0D+00 * u / v )
  b = ( x - 1.0D+00 ) * log ( 1.0D+00 + a )

  iter_num = 0

  do

    y1 = b + log ( a + exp ( y ) )

    if ( abs ( y - y1 ) < 0.001D+00 ) then
      exit
    end if

    y = y1

    iter_num = iter_num + 1

    if ( iter_max < iter_num ) then
      exit
    end if

  end do

  z = v * y1 / 4.0D+00

  return
end
