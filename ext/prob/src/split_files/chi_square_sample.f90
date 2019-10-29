subroutine chi_square_sample ( a, seed, x )

!*****************************************************************************80
!
!! CHI_SQUARE_SAMPLE samples the central Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1 <= A.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  real ( kind = 8 ) b2
  real ( kind = 8 ) c2
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: it_max = 100
  integer ( kind = 4 ) n
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x
  real ( kind = 8 ) x2

  n = int ( a )

  if ( real ( n, kind = 8 ) == a .and. n <= it_max ) then

    x = 0.0D+00
    do i = 1, n
      call normal_01_sample ( seed, x2 )
      x = x + x2 * x2
    end do

  else

    a2 = 0.0D+00
    b2 = 1.0D+00
    c2 = a / 2.0D+00

    call gamma_sample ( a2, b2, c2, seed, x )

    x = 2.0D+00 * x

  end if

  return
end
