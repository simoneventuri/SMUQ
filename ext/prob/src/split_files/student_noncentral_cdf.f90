subroutine student_noncentral_cdf ( x, idf, d, cdf )

!*****************************************************************************80
!
!! STUDENT_NONCENTRAL_CDF evaluates the noncentral Student T CDF.
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
!    Original FORTRAN77 version by B E Cooper.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    BE Cooper,
!    Algorithm AS 5:
!    The Integral of the Non-Central T-Distribution,
!    Applied Statistics,
!    Volume 17, 1968, page 193.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the CDF.
!
!    Input, integer ( kind = 4 ) IDF, the number of degrees of freedom.
!
!    Input, real ( kind = 8 ) D, the noncentrality parameter.
!
!    Output, real ( kind = 8 ) CDF, the value of the CDF.
!
  implicit none

  real ( kind = 8 ) a
  integer ( kind = 4 ), parameter :: a_max = 100
  real ( kind = 8 ) ak
  real ( kind = 8 ) b
  real ( kind = 8 ) cdf
  real ( kind = 8 ) cdf2
  real ( kind = 8 ) d
  real ( kind = 8 ) drb
  real ( kind = 8 ), parameter :: emin = 12.5D+00
  real ( kind = 8 ) f
  real ( kind = 8 ) fk
  real ( kind = 8 ) fmkm1
  real ( kind = 8 ) fmkm2
  integer ( kind = 4 ) idf
  integer ( kind = 4 ) k
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
  real ( kind = 8 ) sum2
  real ( kind = 8 ) temp
  real ( kind = 8 ) tfn
  real ( kind = 8 ) x

  f = real ( idf, kind = 8 )

  if ( idf == 1 ) then

    a = x / sqrt ( f )
    b = f / ( f + x ** 2 )
    drb = d * sqrt ( b )

    call normal_01_cdf ( drb, cdf2 )
    cdf = 1.0D+00 - cdf2 + 2.0D+00 * tfn ( drb, a )

  else if ( idf <= a_max ) then

    a = x / sqrt ( f )
    b = f / ( f + x * x )
    drb = d * sqrt ( b )
    sum2 = 0.0D+00

    fmkm2 = 0.0D+00
    if ( abs ( drb ) < emin ) then
      call normal_01_cdf ( a * drb, cdf2 )
      fmkm2 = a * sqrt ( b ) * exp ( - 0.5D+00 * drb ** 2 ) * cdf2 &
        / sqrt ( 2.0D+00 * r8_pi )
    end if

    fmkm1 = b * d * a * fmkm2
    if ( abs ( d ) < emin ) then
      fmkm1 = fmkm1 + 0.5D+00 * b * a * exp ( - 0.5D+00 * d ** 2 ) / r8_pi
    end if

    if ( mod ( idf, 2 ) == 0 ) then
      sum2 = fmkm2
    else
      sum2 = fmkm1
    end if

    ak = 1.0D+00

    do k = 2, idf - 2, 2

      fk = real ( k, kind = 8 )

      fmkm2 = b * ( d * a * ak * fmkm1 + fmkm2 ) * ( fk - 1.0D+00 ) / fk

      ak = 1.0D+00 / ( ak * ( fk - 1.0D+00 ) )
      fmkm1 = b * ( d * a * ak * fmkm2 + fmkm1 ) * fk / ( fk + 1.0D+00 )

      if ( mod ( idf, 2 ) == 0 ) then
        sum2 = sum2 + fmkm2
      else
        sum2 = sum2 + fmkm1
      end if

      ak = 1.0D+00 / ( ak * fk )

    end do

    if ( mod ( idf, 2 ) == 0 ) then
      call normal_01_cdf ( d, cdf2 )
      cdf = 1.0D+00 - cdf2 + sum2 * sqrt ( 2.0D+00 * r8_pi )
    else
      call normal_01_cdf ( drb, cdf2 )
      cdf = 1.0D+00 - cdf2 + 2.0D+00 * ( sum2 + tfn ( drb, a ) )
    end if
!
!  Normal approximation.
!
  else

    a = sqrt ( 0.5D+00 * f ) * exp ( lgamma ( 0.5D+00 * ( f - 1.0D+00 ) ) &
      - lgamma ( 0.5D+00 * f ) ) * d

    temp = ( x - a ) &
      / sqrt ( f * ( 1.0D+00 + d ** 2 ) / ( f - 2.0D+00 ) - a ** 2 )

    call normal_01_cdf ( temp, cdf2 )
    cdf = cdf2

  end if

  return
end
