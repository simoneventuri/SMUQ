subroutine chi_square_cdf_inv ( cdf, a, x )

!*****************************************************************************80
!
!! CHI_SQUARE_CDF_INV inverts the Chi squared PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 October 2004
!
!  Author:
!
!    Original FORTAN77 version by Donald Best, Roberts.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Donald Best, Roberts,
!    The Percentage Points of the Chi-Squared Distribution,
!    Algorithm AS 91,
!    Applied Statistics,
!    Volume 24, Number ?, pages 385-390, 1975.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) CDF, a value of the chi-squared cumulative
!    probability density function.
!    0.000002 <= CDF <= 0.999998.
!
!    Input, real ( kind = 8 ) A, the parameter of the chi-squared
!    probability density function.  0 < A.
!
!    Output, real ( kind = 8 ) X, the value of the chi-squared random deviate
!    with the property that the probability that a chi-squared random
!    deviate with parameter A is less than or equal to X is CDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a2
  real ( kind = 8 ), parameter :: aa = 0.6931471806D+00
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ), parameter :: c1 = 0.01D+00
  real ( kind = 8 ), parameter :: c2 = 0.222222D+00
  real ( kind = 8 ), parameter :: c3 = 0.32D+00
  real ( kind = 8 ), parameter :: c4 = 0.4D+00
  real ( kind = 8 ), parameter :: c5 = 1.24D+00
  real ( kind = 8 ), parameter :: c6 = 2.2D+00
  real ( kind = 8 ), parameter :: c7 = 4.67D+00
  real ( kind = 8 ), parameter :: c8 = 6.66D+00
  real ( kind = 8 ), parameter :: c9 = 6.73D+00
  real ( kind = 8 ), parameter :: c10 = 13.32D+00
  real ( kind = 8 ), parameter :: c11 = 60.0D+00
  real ( kind = 8 ), parameter :: c12 = 70.0D+00
  real ( kind = 8 ), parameter :: c13 = 84.0D+00
  real ( kind = 8 ), parameter :: c14 = 105.0D+00
  real ( kind = 8 ), parameter :: c15 = 120.0D+00
  real ( kind = 8 ), parameter :: c16 = 127.0D+00
  real ( kind = 8 ), parameter :: c17 = 140.0D+00
  real ( kind = 8 ), parameter :: c18 = 175.0D+00
  real ( kind = 8 ), parameter :: c19 = 210.0D+00
  real ( kind = 8 ), parameter :: c20 = 252.0D+00
  real ( kind = 8 ), parameter :: c21 = 264.0D+00
  real ( kind = 8 ), parameter :: c22 = 294.0D+00
  real ( kind = 8 ), parameter :: c23 = 346.0D+00
  real ( kind = 8 ), parameter :: c24 = 420.0D+00
  real ( kind = 8 ), parameter :: c25 = 462.0D+00
  real ( kind = 8 ), parameter :: c26 = 606.0D+00
  real ( kind = 8 ), parameter :: c27 = 672.0D+00
  real ( kind = 8 ), parameter :: c28 = 707.0D+00
  real ( kind = 8 ), parameter :: c29 = 735.0D+00
  real ( kind = 8 ), parameter :: c30 = 889.0D+00
  real ( kind = 8 ), parameter :: c31 = 932.0D+00
  real ( kind = 8 ), parameter :: c32 = 966.0D+00
  real ( kind = 8 ), parameter :: c33 = 1141.0D+00
  real ( kind = 8 ), parameter :: c34 = 1182.0D+00
  real ( kind = 8 ), parameter :: c35 = 1278.0D+00
  real ( kind = 8 ), parameter :: c36 = 1740.0D+00
  real ( kind = 8 ), parameter :: c37 = 2520.0D+00
  real ( kind = 8 ), parameter :: c38 = 5040.0D+00
  real ( kind = 8 ) cdf
  real ( kind = 8 ), parameter :: cdf_max = 0.999998D+00
  real ( kind = 8 ), parameter :: cdf_min = 0.000002D+00
  real ( kind = 8 ) ch
  real ( kind = 8 ), parameter :: e = 0.0000005D+00
  real ( kind = 8 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ), parameter :: it_max = 20
  real ( kind = 8 ) p1
  real ( kind = 8 ) p2
  real ( kind = 8 ) q
  real ( kind = 8 ) r8_gamma_inc
  real ( kind = 8 ) s1
  real ( kind = 8 ) s2
  real ( kind = 8 ) s3
  real ( kind = 8 ) s4
  real ( kind = 8 ) s5
  real ( kind = 8 ) s6
  real ( kind = 8 ) t
  real ( kind = 8 ) x
  real ( kind = 8 ) x2
  real ( kind = 8 ) xx

  if ( cdf < 0.0D+00 .or. 1.0D+00 < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CHI_SQUARE_CDF_INV - Fatal error!'
    write ( *, '(a)' ) '  CDF < 0 or 1 < CDF.'
    write ( *, '(a,g14.6)' ) '  CDF = ', cdf
    stop 1
  end if

  if ( cdf < cdf_min ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CHI_SQUARE_CDF_INV - Warning!'
    write ( *, '(a)' ) '  CDF < CDF_MIN.'
    write ( *, '(a,g14.6)' ) '  CDF = ', cdf
    write ( *, '(a,g14.6)' ) '  CDF_MIN = ', cdf_min
  end if

  if ( cdf_max < cdf ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CHI_SQUARE_CDF_INV - Warning!'
    write ( *, '(a)' ) '  CDF_MAX < CDF.'
    write ( *, '(a,g14.6)' ) '  CDF = ', cdf
    write ( *, '(a,g14.6)' ) '  CDF_MAX = ', cdf_max
  end if

  xx = 0.5D+00 * a
  c = xx - 1.0D+00
!
!  Compute Log ( Gamma ( A/2 ) ).
!
  g = lgamma ( a / 2.0D+00 )
!
!  Starting approximation for small chi-squared.
!
  if ( a < - c5 * log ( cdf ) ) then

    ch = ( cdf * xx * exp ( g + xx * aa ) ) ** ( 1.0D+00 / xx )

    if ( ch < e ) then
      x = ch
      return
    end if
!
!  Starting approximation for A less than or equal to 0.32.
!
  else if ( a <= c3 ) then

    ch = c4
    a2 = log ( 1.0D+00 - cdf )

    do

      q = ch
      p1 = 1.0D+00 + ch * ( c7 + ch )
      p2 = ch * ( c9 + ch * ( c8 + ch ) )

      t = - 0.5D+00 + ( c7 + 2.0D+00 * ch ) / p1 &
        - ( c9 + ch * ( c10 + 3.0D+00 * ch ) ) / p2

      ch = ch - ( 1.0D+00 - exp ( a2 + g + 0.5D+00 * ch + c * aa ) &
        * p2 / p1 ) / t

      if ( abs ( q / ch - 1.0D+00 ) <= c1 ) then
        exit
      end if

    end do
!
!  Call to algorithm AS 111.
!  Note that P has been tested above.
!  AS 241 could be used as an alternative.
!
  else

    call normal_01_cdf_inv ( cdf, x2 )
!
!  Starting approximation using Wilson and Hilferty estimate.
!
    p1 = c2 / a
    ch = a * ( x2 * sqrt ( p1 ) + 1.0D+00 - p1 ) ** 3
!
!  Starting approximation for P tending to 1.
!
    if ( c6 * a + 6.0D+00 < ch ) then
      ch = - 2.0D+00 * ( log ( 1.0D+00 - cdf ) - c * log ( 0.5D+00 * ch ) + g )
    end if

  end if
!
!  Call to algorithm AS 239 and calculation of seven term Taylor series.
!
  do i = 1, it_max

    q = ch
    p1 = 0.5D+00 * ch
    p2 = cdf - r8_gamma_inc ( xx, p1 )
    t = p2 * exp ( xx * aa + g + p1 - c * log ( ch ) )
    b = t / ch
    a2 = 0.5D+00 * t - b * c

    s1 = ( c19 + a2 &
       * ( c17 + a2 &
       * ( c14 + a2 &
       * ( c13 + a2 &
       * ( c12 + a2 &
       *   c11 ) ) ) ) ) / c24

    s2 = ( c24 + a2 &
       * ( c29 + a2 &
       * ( c32 + a2 &
       * ( c33 + a2 &
       *   c35 ) ) ) ) / c37

    s3 = ( c19 + a2 &
       * ( c25 + a2 &
       * ( c28 + a2 &
       *   c31 ) ) ) / c37

    s4 = ( c20 + a2 &
       * ( c27 + a2 &
       *   c34 ) + c &
       * ( c22 + a2 &
       * ( c30 + a2 &
       *   c36 ) ) ) / c38

    s5 = ( c13 + c21 * a2 + c * ( c18 + c26 * a2 ) ) / c37

    s6 = ( c15 + c * ( c23 + c16 * c ) ) / c38

    ch = ch + t * ( 1.0D+00 + 0.5D+00 * t * s1 - b * c &
      * ( s1 - b &
      * ( s2 - b &
      * ( s3 - b &
      * ( s4 - b &
      * ( s5 - b &
      *   s6 ) ) ) ) ) )

    if ( e < abs ( q / ch - 1.0D+00 ) ) then
      x = ch
      return
    end if

  end do

  x = ch
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CHI_SQUARE_CDF_INV - Warning!'
  write ( *, '(a)' ) '  Convergence not reached.'

  return
end
