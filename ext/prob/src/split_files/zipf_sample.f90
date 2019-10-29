subroutine zipf_sample ( a, seed, x )

!*****************************************************************************80
!
!! ZIPF_SAMPLE samples the Zipf PDF.
!
!  Discussion:
!
!    I am concerned that there may be a discrepancy in the results
!    of this routine, which do not seem to have the predicted variances.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Luc Devroye,
!    Non-Uniform Random Variate Generation,
!    Springer Verlag, 1986, pages 550-551.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1.0D+00 < A.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i4_huge
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  real ( kind = 8 ) t
  real ( kind = 8 ) test
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  integer ( kind = 4 ) x

  test = real ( i4_huge ( ), kind = 8 )

  b = 2.0D+00 ** ( a - 1.0D+00 )

  do

    u = r8_uniform_01 ( seed )
    v = r8_uniform_01 ( seed )
    w = aint ( 1.0D+00 / u ** ( 1.0D+00 / ( a - 1.0D+00 ) ) )
!
!  Very small values of U can cause W to be very large,
!  bigger than the largest integer...
!
    if ( test < w ) then
      cycle
    end if

    t = ( ( w + 1.0D+00 ) / w ) ** ( a - 1.0D+00 )

    if ( v * w * ( t - 1.0D+00 ) * b <= t * ( b - 1.0D+00 ) ) then
      exit
    end if

  end do

  x = int ( w )

  return
end
