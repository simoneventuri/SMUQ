subroutine negative_binomial_cdf ( x, a, b, cdf )

!*****************************************************************************80
!
!! NEGATIVE_BINOMIAL_CDF evaluates the Negative Binomial CDF.
!
!  Discussion:
!
!    A simple summing approach is used.
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
!    Input, integer ( kind = 4 ) X, the argument of the CDF.
!
!    Input, integer ( kind = 4 ) A, a parameter of the PDF.
!    0 <= A.
!
!    Input, real ( kind = 8 ) B, a parameter of the PDF.
!    0 < B <= 1.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  integer ( kind = 4 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  integer ( kind = 4 ) cnk
  integer ( kind = 4 ) i4_choose
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x
  integer ( kind = 4 ) y

  cdf = 0.0D+00

  do y = a, x

    cnk = i4_choose ( y - 1, a - 1 )

    pdf = real ( cnk, kind = 8 ) * b ** a * ( 1.0D+00 - b ) ** ( y - a )

    cdf = cdf + pdf

  end do

  return
end
