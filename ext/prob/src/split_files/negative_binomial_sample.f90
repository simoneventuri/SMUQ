subroutine negative_binomial_sample ( a, b, seed, x )

!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_SAMPLE samples the Negative Binomial PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) A, a parameter of the PDF.
!    0 <= A.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    0 < B <= 1.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, integer ( kind = 4 ) X, a sample of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i4_huge
  integer ( kind = 4 ) num_success
  real ( kind = 8 ) r
  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) x

  if ( b == 1.0D+00 ) then
    x = a
    return
  else if ( b == 0.0D+00 ) then
    x = i4_huge ( )
    return
  end if

  x = 0
  num_success = 0

  do while ( num_success < a )

    x = x + 1
    r = r8_uniform_01 ( seed )

    if ( r <= b ) then
      num_success = num_success + 1
    end if

  end do

  return
end
