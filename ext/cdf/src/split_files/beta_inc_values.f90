subroutine beta_inc_values ( n_data, a, b, x, fx )

!*****************************************************************************80
!
!! BETA_INC_VALUES returns some values of the incomplete Beta function.
!
!  Discussion:
!
!    The incomplete Beta function may be written
!
!      BETA_INC(A,B,X) = Integral (0 to X) T^(A-1) * (1-T)^(B-1) dT
!                      / Integral (0 to 1) T^(A-1) * (1-T)^(B-1) dT
!
!    Thus,
!
!      BETA_INC(A,B,0.0) = 0.0
!      BETA_INC(A,B,1.0) = 1.0
!
!    Note that in Mathematica, the expressions:
!
!      BETA[A,B]   = Integral (0 to 1) T^(A-1) * (1-T)^(B-1) dT
!      BETA[X,A,B] = Integral (0 to X) T^(A-1) * (1-T)^(B-1) dT
!
!    and thus, to evaluate the incomplete Beta function requires:
!
!      BETA_INC(A,B,X) = BETA[X,A,B] / BETA[A,B]
!
!  Modified:
!
!    17 February 2004
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
!    Karl Pearson,
!    Tables of the Incomplete Beta Function,
!    Cambridge University Press, 1968.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, B, X, the arguments of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 30

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
     0.5D+00,  0.5D+00,  0.5D+00,  1.0D+00, &
     1.0D+00,  1.0D+00,  1.0D+00,  1.0D+00, &
     2.0D+00,  2.0D+00,  2.0D+00,  2.0D+00, &
     2.0D+00,  2.0D+00,  2.0D+00,  2.0D+00, &
     2.0D+00,  5.5D+00, 10.0D+00, 10.0D+00, &
    10.0D+00, 10.0D+00, 20.0D+00, 20.0D+00, &
    20.0D+00, 20.0D+00, 20.0D+00, 30.0D+00, &
    30.0D+00, 40.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), save, dimension ( n_max ) :: b_vec = (/ &
     0.5D+00,  0.5D+00,  0.5D+00,  0.5D+00, &
     0.5D+00,  0.5D+00,  0.5D+00,  1.0D+00, &
     2.0D+00,  2.0D+00,  2.0D+00,  2.0D+00, &
     2.0D+00,  2.0D+00,  2.0D+00,  2.0D+00, &
     2.0D+00,  5.0D+00,  0.5D+00,  5.0D+00, &
     5.0D+00, 10.0D+00,  5.0D+00, 10.0D+00, &
    10.0D+00, 20.0D+00, 20.0D+00, 10.0D+00, &
    10.0D+00, 20.0D+00 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.0637686D+00, 0.2048328D+00, 1.0000000D+00, 0.0D+00,       &
    0.0050126D+00, 0.0513167D+00, 0.2928932D+00, 0.5000000D+00, &
    0.028D+00,     0.104D+00,     0.216D+00,     0.352D+00,     &
    0.500D+00,     0.648D+00,     0.784D+00,     0.896D+00,     &
    0.972D+00,     0.4361909D+00, 0.1516409D+00, 0.0897827D+00, &
    1.0000000D+00, 0.5000000D+00, 0.4598773D+00, 0.2146816D+00, &
    0.9507365D+00, 0.5000000D+00, 0.8979414D+00, 0.2241297D+00, &
    0.7586405D+00, 0.7001783D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    0.01D+00, 0.10D+00, 1.00D+00, 0.0D+00,  &
    0.01D+00, 0.10D+00, 0.50D+00, 0.50D+00, &
    0.1D+00,  0.2D+00,  0.3D+00,  0.4D+00,  &
    0.5D+00,  0.6D+00,  0.7D+00,  0.8D+00,  &
    0.9D+00,  0.50D+00, 0.90D+00, 0.50D+00, &
    1.00D+00, 0.50D+00, 0.80D+00, 0.60D+00, &
    0.80D+00, 0.50D+00, 0.60D+00, 0.70D+00, &
    0.80D+00, 0.70D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    a = 0.0D+00
    b = 0.0D+00
    x = 0.0D+00
    fx = 0.0D+00
  else
    a = a_vec(n_data)
    b = b_vec(n_data)
    x = x_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
