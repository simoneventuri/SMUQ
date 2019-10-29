subroutine chi_square_cdf_values ( n_data, a, x, fx )

!*****************************************************************************80
!
!! CHI_SQUARE_CDF_VALUES returns some values of the Chi-Square CDF.
!
!  Discussion:
!
!    The value of CHI_CDF ( DF, X ) can be evaluated in Mathematica by
!    commands like:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF[ChiSquareDistribution[DF], X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 June 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Milton Abramowitz, Irene Stegun,
!    Handbook of Mathematical Functions,
!    US Department of Commerce, 1964.
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
!    Output, integer ( kind = 4 ) A, real ( kind = 8 ) X, the arguments of
!    the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 21

  integer ( kind = 4 ) a
  integer ( kind = 4 ), save, dimension ( n_max ) :: a_vec = (/ &
     1,  2,  1,  2, &
     1,  2,  3,  4, &
     1,  2,  3,  4, &
     5,  3,  3,  3, &
     3,  3, 10, 10, &
    10 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.0796557D+00, 0.00498752D+00, 0.112463D+00,    0.00995017D+00, &
    0.472911D+00,  0.181269D+00,   0.0597575D+00,   0.0175231D+00, &
    0.682689D+00,  0.393469D+00,   0.198748D+00,    0.090204D+00, &
    0.0374342D+00, 0.427593D+00,   0.608375D+00,    0.738536D+00, &
    0.828203D+00,  0.88839D+00,    0.000172116D+00, 0.00365985D+00, &
    0.0185759D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.01D+00, 0.01D+00, 0.02D+00, 0.02D+00, &
    0.40D+00, 0.40D+00, 0.40D+00, 0.40D+00, &
    1.00D+00, 1.00D+00, 1.00D+00, 1.00D+00, &
    1.00D+00, 2.00D+00, 3.00D+00, 4.00D+00, &
    5.00D+00, 6.00D+00, 1.00D+00, 2.00D+00, &
    3.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
