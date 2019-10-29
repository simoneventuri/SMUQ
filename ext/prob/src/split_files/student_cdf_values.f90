subroutine student_cdf_values ( n_data, c, x, fx )

!*****************************************************************************80
!
!! STUDENT_CDF_VALUES returns some values of the Student CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      dist = StudentTDistribution [ c ]
!      CDF [ dist, x ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    National Bureau of Standards, 1964,
!    LC: QA47.A34,
!    ISBN: 0-486-61272-4.
!
!    Stephen Wolfram,
!    The Mathematica Book,
!    Fourth Edition,
!    Wolfram Media / Cambridge University Press, 1999.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) C, is usually called the number of
!    degrees of freedom of the distribution.  C is typically an
!    integer, but that is not essential.  It is required that
!    C be strictly positive.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 13

  real ( kind = 8 ) c
  real ( kind = 8 ), save, dimension ( n_max ) :: c_vec = (/ &
    1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, &
    5.0D+00, 2.0D+00, 5.0D+00, 2.0D+00, &
    5.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, &
    5.0D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.6000231200328521D+00, &
    0.6001080279134390D+00, &
    0.6001150934648930D+00, &
    0.6000995134721354D+00, &
    0.5999341989834830D+00, &
    0.7498859393137811D+00, &
    0.7500879487671045D+00, &
    0.9500004222186464D+00, &
    0.9499969138365968D+00, &
    0.9900012348724744D+00, &
    0.9900017619355059D+00, &
    0.9900004567580596D+00, &
    0.9900007637471291D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.325D+00, &
    0.289D+00, &
    0.277D+00, &
    0.271D+00, &
    0.267D+00, &
    0.816D+00, &
    0.727D+00, &
    2.920D+00, &
    2.015D+00, &
    6.965D+00, &
    4.541D+00, &
    3.747D+00, &
    3.365D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    c = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    c = c_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
