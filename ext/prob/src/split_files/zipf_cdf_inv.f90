subroutine zipf_cdf_inv ( a, cdf, x )

!*****************************************************************************80
!
!! ZIPF_CDF_INV inverts the Zipf CDF.
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
!    09 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, the value of the CDF.
!
!    Input, real ( kind = 8 ) A, the parameter of the PDF.
!    1.0 < A.
!
!    Output, integer ( kind = 4 ) X, the argument such that
!    CDF(X-1) < CDF <= CDF(X)
!    1 <= X <= 1000
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) c
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf2
  real ( kind = 8 ) pdf
  real ( kind = 8 ) r8_zeta
  integer ( kind = 4 ) x
  integer ( kind = 4 ) y

  if ( cdf <= 0.0 ) then

    x = 1

  else

    c = r8_zeta ( a )
    cdf2 = 0.0D+00

    x = 1000

    do y = 1, 1000
      pdf = ( 1.0D+00 / y ** a ) / c
      cdf2 = cdf2 + pdf
      if ( cdf <= cdf2 ) then
        x = y
        exit
      end if
    end do

  end if

  return
end
