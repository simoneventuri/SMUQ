subroutine f_noncentral_cdf_values ( n_data, a, b, lambda, x, fx )

!*****************************************************************************80
!
!! F_NONCENTRAL_CDF_VALUES returns some values of the F CDF test function.
!
!  Discussion:
!
!    The value of NONCENTRAL_F_CDF ( DFN, DFD, LAMDA, X ) can be evaluated
!    in Mathematica by commands like:
!
!      Needs["Statistics`ContinuousDistributions`"]
!      CDF[NoncentralFRatioDistribution[ DFN, DFD, LAMBDA ], X ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 June 2004
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
!    Output, integer ( kind = 4 ) A, B, real ( kind = 8 ) LAMBDA, the
!    parameters of the function.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 22

  integer ( kind = 4 ) a
  integer ( kind = 4 ), save, dimension ( n_max ) :: a_vec = (/ &
     1,  1,  1,  1, &
     1,  1,  1,  1, &
     1,  1,  2,  2, &
     3,  3,  4,  4, &
     5,  5,  6,  6, &
     8, 16 /)
  integer ( kind = 4 ) b
  integer ( kind = 4 ), save, dimension ( n_max ) :: b_vec = (/ &
     1,  5,  5,  5, &
     5,  5,  5,  5, &
     5,  5,  5, 10, &
     5,  5,  5,  5, &
     1,  5,  6, 12, &
    16,  8 /)
  real ( kind = 8 ) fx
  real, save, dimension ( n_max ) :: fx_vec = (/ &
    0.500000D+00, 0.636783D+00, 0.584092D+00, 0.323443D+00, &
    0.450119D+00, 0.607888D+00, 0.705928D+00, 0.772178D+00, &
    0.819105D+00, 0.317035D+00, 0.432722D+00, 0.450270D+00, &
    0.426188D+00, 0.337744D+00, 0.422911D+00, 0.692767D+00, &
    0.363217D+00, 0.421005D+00, 0.426667D+00, 0.446402D+00, &
    0.844589D+00, 0.816368D+00 /)
  real ( kind = 8 ) lambda
  real, save, dimension ( n_max ) :: lambda_vec = (/ &
    0.00D+00,  0.000D+00, 0.25D+00,  1.00D+00, &
    1.00D+00,  1.00D+00,  1.00D+00,  1.00D+00, &
    1.00D+00,  2.00D+00,  1.00D+00,  1.00D+00, &
    1.00D+00,  2.00D+00,  1.00D+00,  1.00D+00, &
    0.00D+00,  1.00D+00,  1.00D+00,  1.00D+00, &
    1.00D+00,  1.00D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real, save, dimension ( n_max ) :: x_vec = (/ &
    1.00D+00,  1.00D+00, 1.00D+00,  0.50D+00, &
    1.00D+00,  2.00D+00, 3.00D+00,  4.00D+00, &
    5.00D+00,  1.00D+00, 1.00D+00,  1.00D+00, &
    1.00D+00,  1.00D+00, 1.00D+00,  2.00D+00, &
    1.00D+00,  1.00D+00, 1.00D+00,  1.00D+00, &
    2.00D+00,  2.00D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0
    b = 0
    lambda = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    b = b_vec(n_data)
    lambda = lambda_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
