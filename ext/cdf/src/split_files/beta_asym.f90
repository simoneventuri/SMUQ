function beta_asym ( a, b, lambda, eps )

!*****************************************************************************80
!
!! BETA_ASYM computes an asymptotic expansion for IX(A,B), for large A and B.
!
!  Author:
!
!    Armido DiDinato, Alfred Morris
!
!  Reference:
!
!    Armido DiDinato, Alfred Morris,
!    Algorithm 708:
!    Significant Digit Computation of the Incomplete Beta Function Ratios,
!    ACM Transactions on Mathematical Software,
!    Volume 18, 1993, pages 360-373.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the function.
!    A and B should be nonnegative.  It is assumed that both A and B
!    are greater than or equal to 15.
!
!    Input, real ( kind = 8 ) LAMBDA, the value of ( A + B ) * Y - B.
!    It is assumed that 0 <= LAMBDA.
!
!    Input, real ( kind = 8 ) EPS, the tolerance.
!
  implicit none

  integer ( kind = 4 ), parameter :: num = 20

  real ( kind = 8 ) a
  real ( kind = 8 ) a0(num+1)
  real ( kind = 8 ) b
  real ( kind = 8 ) b0(num+1)
  real ( kind = 8 ) bcorr
  real ( kind = 8 ) beta_asym
  real ( kind = 8 ) bsum
  real ( kind = 8 ) c(num+1)
  real ( kind = 8 ) d(num+1)
  real ( kind = 8 ) dsum
  real ( kind = 8 ), parameter :: e0 = 1.12837916709551D+00
  real ( kind = 8 ), parameter :: e1 = 0.353553390593274D+00
  real ( kind = 8 ) eps
  real ( kind = 8 ) error_fc
  real ( kind = 8 ) f
  real ( kind = 8 ) h
  real ( kind = 8 ) h2
  real ( kind = 8 ) hn
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) j0
  real ( kind = 8 ) j1
  real ( kind = 8 ) lambda
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm1
  integer ( kind = 4 ) mmj
  integer ( kind = 4 ) n
  integer ( kind = 4 ) np1
  real ( kind = 8 ) r
  real ( kind = 8 ) r0
  real ( kind = 8 ) r1
  real ( kind = 8 ) rlog1
  real ( kind = 8 ) s
  real ( kind = 8 ) sum1
  real ( kind = 8 ) t
  real ( kind = 8 ) t0
  real ( kind = 8 ) t1
  real ( kind = 8 ) u
  real ( kind = 8 ) w
  real ( kind = 8 ) w0
  real ( kind = 8 ) z
  real ( kind = 8 ) z0
  real ( kind = 8 ) z2
  real ( kind = 8 ) zn
  real ( kind = 8 ) znm1

  beta_asym = 0.0D+00

  if ( a < b ) then
    h = a / b
    r0 = 1.0D+00 / ( 1.0D+00 + h )
    r1 = ( b - a ) / b
    w0 = 1.0D+00 / sqrt ( a * ( 1.0D+00 + h ))
  else
    h = b / a
    r0 = 1.0D+00 / ( 1.0D+00 + h )
    r1 = ( b - a ) / a
    w0 = 1.0D+00 / sqrt ( b * ( 1.0D+00 + h ))
  end if

  f = a * rlog1 ( - lambda / a ) + b * rlog1 ( lambda / b )
  t = exp ( - f )
  if ( t == 0.0D+00 ) then
    return
  end if

  z0 = sqrt ( f )
  z = 0.5D+00 * ( z0 / e1 )
  z2 = f + f

  a0(1) = ( 2.0D+00 / 3.0D+00 ) * r1
  c(1) = -0.5D+00 * a0(1)
  d(1) = -c(1)
  j0 = ( 0.5D+00 / e0 ) * error_fc ( 1, z0 )
  j1 = e1
  sum1 = j0 + d(1) * w0 * j1

  s = 1.0D+00
  h2 = h * h
  hn = 1.0D+00
  w = w0
  znm1 = z
  zn = z2

  do n = 2, num, 2

    hn = h2 * hn
    a0(n) = 2.0D+00 * r0 * ( 1.0D+00 + h * hn ) &
      / ( n + 2.0D+00 )
    np1 = n + 1
    s = s + hn
    a0(np1) = 2.0D+00 * r1 * s / ( n + 3.0D+00 )

    do i = n, np1

      r = -0.5D+00 * ( i + 1.0D+00 )
      b0(1) = r * a0(1)
      do m = 2, i
        bsum = 0.0D+00
        mm1 = m - 1
        do j = 1, mm1
          mmj = m - j
          bsum = bsum + ( j * r - mmj ) * a0(j) * b0(mmj)
        end do
        b0(m) = r * a0(m) + bsum / m
      end do

      c(i) = b0(i) / ( i + 1.0D+00 )

      dsum = 0.0
      do j = 1, i-1
        dsum = dsum + d(i-j) * c(j)
      end do
      d(i) = - ( dsum + c(i) )

    end do

    j0 = e1 * znm1 + ( n - 1.0D+00 ) * j0
    j1 = e1 * zn + n * j1
    znm1 = z2 * znm1
    zn = z2 * zn
    w = w0 * w
    t0 = d(n) * w * j0
    w = w0 * w
    t1 = d(np1) * w * j1
    sum1 = sum1 + ( t0 + t1 )

    if ( ( abs ( t0 ) + abs ( t1 )) <= eps * sum1 ) then
      u = exp ( - bcorr ( a, b ) )
      beta_asym = e0 * t * u * sum1
      return
    end if

  end do

  u = exp ( - bcorr ( a, b ) )
  beta_asym = e0 * t * u * sum1

  return
end
