subroutine zipf_cdf ( x, a, cdf )

!*****************************************************************************80
!
!! ZIPF_CDF evaluates the Zipf CDF.
!
!  Discussion:
!
!    Simple summation is used.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the argument of the PDF.
!    1 <= N
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1.0D+00 < A.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), save :: asave = 0.0D+00
  real ( kind = 8 ), save :: c = 0.0D+00
  real ( kind = 8 ) cdf
  real ( kind = 8 ) pdf
  real ( kind = 8 ) r8_zeta
  integer ( kind = 4 ) x
  integer ( kind = 4 ) y

  if ( x < 1 ) then

    cdf = 0.0D+00

  else

    if ( a /= asave ) then

      c = r8_zeta ( a )
      asave = a

    end if

    cdf = 0.0D+00
    do y = 1, x
      pdf = ( 1.0D+00 / real ( y, kind = 8 ) ** a ) / c
      cdf = cdf + pdf
    end do

  end if

  return
end
