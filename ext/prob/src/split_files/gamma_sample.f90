subroutine gamma_sample ( a, b, c, seed, x )

!*****************************************************************************80
!
!! GAMMA_SAMPLE samples the Gamma PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 September 2000
!
!  Author:
!
!    Original FORTRAN77 version by Joachim Ahrens, Ulrich Dieter.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Joachim Ahrens, Ulrich Dieter,
!    Generating Gamma Variates by a Modified Rejection Technique,
!    Communications of the ACM,
!    Volume 25, Number 1, January 1982, pages 47 - 54.
!
!    Joachim Ahrens, Ulrich Dieter,
!    Computer Methods for Sampling from Gamma, Beta, Poisson and
!    Binomial Distributions.
!    Computing,
!    Volume 12, 1974, pages 223 - 246.
!
!    Joachim Ahrens, KD Kohrt, Ulrich Dieter,
!    Algorithm 599,
!    ACM Transactions on Mathematical Software,
!    Volume 9, Number 2, June 1983, pages 255-257.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    0.0 < B,
!    0.0 < C.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X, a sample of the PDF.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ), parameter :: a1 =   0.3333333D+00
  real ( kind = 8 ), parameter :: a2 = - 0.2500030D+00
  real ( kind = 8 ), parameter :: a3 =   0.2000062D+00
  real ( kind = 8 ), parameter :: a4 = - 0.1662921D+00
  real ( kind = 8 ), parameter :: a5 =   0.1423657D+00
  real ( kind = 8 ), parameter :: a6 = - 0.1367177D+00
  real ( kind = 8 ), parameter :: a7 =   0.1233795D+00
  real ( kind = 8 ) b
  real ( kind = 8 ) bcoef
  real ( kind = 8 ) c
  real ( kind = 8 ) co
  real ( kind = 8 ) d
  real ( kind = 8 ) r8_uniform_01
  real ( kind = 8 ) e
  real ( kind = 8 ), parameter :: e1 = 1.0D+00
  real ( kind = 8 ), parameter :: e2 = 0.4999897D+00
  real ( kind = 8 ), parameter :: e3 = 0.1668290D+00
  real ( kind = 8 ), parameter :: e4 = 0.0407753D+00
  real ( kind = 8 ), parameter :: e5 = 0.0102930D+00
  real ( kind = 8 ), parameter :: euler = 2.71828182845904D+00
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) q0
  real ( kind = 8 ), parameter :: q1 =   0.04166669D+00
  real ( kind = 8 ), parameter :: q2 =   0.02083148D+00
  real ( kind = 8 ), parameter :: q3 =   0.00801191D+00
  real ( kind = 8 ), parameter :: q4 =   0.00144121D+00
  real ( kind = 8 ), parameter :: q5 = - 0.00007388D+00
  real ( kind = 8 ), parameter :: q6 =   0.00024511D+00
  real ( kind = 8 ), parameter :: q7 =   0.00024240D+00
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  integer ( kind = 4 ) seed
  real ( kind = 8 ) si
  real ( kind = 8 ) s2
  real ( kind = 8 ) t
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) w
  real ( kind = 8 ) x
!
!  Allow C = 0.
!
  if ( c == 0.0D+00 ) then
    x = a
    return
  end if
!
!  C < 1.
!
  if ( c < 1.0D+00 ) then

    do

      u = r8_uniform_01 ( seed )
      t = 1.0D+00 + c / euler
      p = u * t

      call exponential_01_sample ( seed, s )

      if ( p < 1.0D+00 ) then
        x = exp ( log ( p ) / c )
        if ( x <= s ) then
          exit
        end if
      else
        x = - log ( ( t - p ) / c )
        if ( ( 1.0D+00 - c ) * log ( x ) <= s ) then
          exit
        end if
      end if

    end do

    x = a + b * x
    return
!
!  1 <= C.
!
  else

    s2 = c - 0.5D+00
    s = sqrt ( c - 0.5D+00 )
    d = sqrt ( 32.0D+00 ) - 12.0D+00 * sqrt ( c - 0.5D+00 )

    call normal_01_sample ( seed, t )
    x = ( sqrt ( c - 0.5D+00 ) + 0.5D+00 * t ) ** 2

    if ( 0.0D+00 <= t ) then
      x = a + b * x
      return
    end if

    u = r8_uniform_01 ( seed )

    if ( d * u <= t ** 3 ) then
      x = a + b * x
      return
    end if

    r = 1.0D+00 / c

    q0 = ( ( ( ( ( ( &
           q7   * r &
         + q6 ) * r &
         + q5 ) * r &
         + q4 ) * r &
         + q3 ) * r &
         + q2 ) * r &
         + q1 ) * r

    if ( c <= 3.686D+00 ) then
      bcoef = 0.463D+00 + s - 0.178D+00 * s2
      si = 1.235D+00
      co = 0.195D+00 / s - 0.079D+00 + 0.016D+00 * s
    else if ( c <= 13.022D+00 ) then
      bcoef = 1.654D+00 + 0.0076D+00 * s2
      si = 1.68D+00 / s + 0.275D+00
      co = 0.062D+00 / s + 0.024D+00
    else
      bcoef = 1.77D+00
      si = 0.75D+00
      co = 0.1515D+00 / s
    end if

    if ( 0.0D+00 < sqrt ( c - 0.5D+00 ) + 0.5D+00 * t ) then

      v = 0.5D+00 * t / s

      if ( 0.25D+00 < abs ( v ) ) then
        q = q0 - s * t + 0.25D+00 * t * t + 2.0D+00 * s2 * log ( 1.0D+00 + v )
      else
        q = q0 + 0.5D+00 * t ** 2 * ( ( ( ( ( ( &
               a7   * v &
             + a6 ) * v &
             + a5 ) * v &
             + a4 ) * v &
             + a3 ) * v &
             + a2 ) * v &
             + a1 ) * v
      end if

      if ( log ( 1.0D+00 - u ) <= q ) then
        x = a + b * x
        return
      end if

    end if

    do

      call exponential_01_sample ( seed, e )

      u = r8_uniform_01 ( seed )

      u = 2.0D+00 * u - 1.0D+00
      t = bcoef + sign ( si * e, u )

      if ( -0.7187449D+00 <= t ) then

        v = 0.5D+00 * t / s

        if ( 0.25D+00 < abs ( v ) ) then
          q = q0 - s * t + 0.25D+00 * t ** 2 &
            + 2.0D+00 * s2 * log ( 1.0D+00 + v )
        else
          q = q0 + 0.5D+00 * t ** 2 * ( ( ( ( ( ( &
               a7   * v &
             + a6 ) * v &
             + a5 ) * v &
             + a4 ) * v &
             + a3 ) * v &
             + a2 ) * v &
             + a1 ) * v
        end if

        if ( 0.0D+00 < q ) then

          if ( 0.5D+00 < q ) then
            w = exp ( q ) - 1.0D+00
          else
            w = ( ( ( ( &
                    e5   * q &
                  + e4 ) * q &
                  + e3 ) * q &
                  + e2 ) * q &
                  + e1 ) * q
          end if

          if ( co * abs ( u ) <= w * exp ( e - 0.5D+00 * t ** 2 ) ) then
            x = a + b * ( s + 0.5D+00 * t ) ** 2
            return
          end if

        end if

      end if

    end do

  end if

  return
end
