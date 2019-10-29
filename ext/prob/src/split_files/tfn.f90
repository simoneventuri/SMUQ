function tfn ( h, a )

!*****************************************************************************80
!
!! TFN calculates the T function of Owen.
!
!  Discussion:
!
!    Owen's T function is useful for computation of the bivariate normal
!    distribution and the distribution of a skewed normal distribution.
!
!    Although it was originally formulated in terms of the bivariate
!    normal function, the function can be defined more directly as
!
!      T(H,A) = 1 / ( 2 * pi ) *
!        Integral ( 0 <= X <= A ) e^( -H^2 * (1+X^2) / 2 ) / (1+X^2) dX
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 December 2004
!
!  Author:
!
!    Original FORTRAN77 version by J C Young, C E Minder.
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Donald Owen,
!    Tables for computing the bivariate normal distribution,
!    Annals of Mathematical Statistics,
!    Volume 27, pages 1075-1090, 1956.
!
!    JC Young, CE Minder,
!    Algorithm AS 76,
!    An Algorithm Useful in Calculating Non-Central T and
!    Bivariate Normal Distributions,
!    Applied Statistics,
!    Volume 23, Number 3, 1974, pages 455-457.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) H, A, the arguments of the T function.
!
!    Output, real ( kind = 8 ) TFN, the value of the T function.
!
  implicit none

  integer ( kind = 4 ), parameter :: ngauss = 10

  real ( kind = 8 ) a
  real ( kind = 8 ) as
  real ( kind = 8 ) h
  real ( kind = 8 ) h1
  real ( kind = 8 ) h2
  real ( kind = 8 ) hs
  integer ( kind = 4 ) i
  real ( kind = 8 ) rt
  real ( kind = 8 ) tfn
  real ( kind = 8 ), parameter :: two_pi_inverse = 0.1591549430918953D+00
  real ( kind = 8 ), parameter :: tv1 = 1.0D-35
  real ( kind = 8 ), parameter :: tv2 = 15.0D+00
  real ( kind = 8 ), parameter :: tv3 = 15.0D+00
  real ( kind = 8 ), parameter :: tv4 = 1.0D-05
  real ( kind = 8 ), parameter, dimension ( ngauss ) :: weight = (/ &
    0.666713443086881375935688098933D-01, &
    0.149451349150580593145776339658D+00, &
    0.219086362515982043995534934228D+00, &
    0.269266719309996355091226921569D+00, &
    0.295524224714752870173892994651D+00, &
    0.295524224714752870173892994651D+00, &
    0.269266719309996355091226921569D+00, &
    0.219086362515982043995534934228D+00, &
    0.149451349150580593145776339658D+00, &
    0.666713443086881375935688098933D-01 /)
  real ( kind = 8 ) x
  real ( kind = 8 ), parameter, dimension ( ngauss ) :: xtab = (/ &
   -0.973906528517171720077964012084D+00, &
   -0.865063366688984510732096688423D+00, &
   -0.679409568299024406234327365115D+00, &
   -0.433395394129247190799265943166D+00, &
   -0.148874338981631210884826001130D+00, &
    0.148874338981631210884826001130D+00, &
    0.433395394129247190799265943166D+00, &
    0.679409568299024406234327365115D+00, &
    0.865063366688984510732096688423D+00, &
    0.973906528517171720077964012084D+00 /)
!
!  Test for H near zero.
!
  if ( abs ( h ) < tv1 ) then
    tfn = atan ( a ) * two_pi_inverse
!
!  Test for large values of abs(H).
!
  else if ( tv2 < abs ( h ) ) then
    tfn = 0.0D+00
!
!  Test for A near zero.
!
  else if ( abs ( a ) < tv1 ) then
    tfn = 0.0D+00
!
!  Test whether abs(A) is so large that it must be truncated.
!  If so, the truncated value of A is H2.
!
  else

    hs = - 0.5D+00 * h * h
    h2 = a
    as = a * a
!
!  Computation of truncation point by Newton iteration.
!
    if ( tv3 <= log ( 1.0D+00 + as ) - hs * as ) then

      h1 = 0.5D+00 * a
      as = 0.25D+00 * as

      do

        rt = as + 1.0D+00
        h2 = h1 + ( hs * as + tv3 - log ( rt ) ) &
          / ( 2.0D+00 * h1 * ( 1.0D+00 / rt - hs ) )
        as = h2 * h2

        if ( abs ( h2 - h1 ) < tv4 ) then
          exit
        end if

        h1 = h2

      end do

    end if
!
!  Gaussian quadrature on the interval [0,H2].
!
    rt = 0.0D+00
    do i = 1, ngauss
      x = 0.5D+00 * h2 * ( xtab(i) + 1.0D+00 )
      rt = rt + weight(i) * exp ( hs * ( 1.0D+00 + x * x ) ) &
        / ( 1.0D+00 + x * x )
    end do

    tfn = rt * ( 0.5D+00 * h2 ) * two_pi_inverse

  end if

  return
end
