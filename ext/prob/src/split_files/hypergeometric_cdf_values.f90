subroutine hypergeometric_cdf_values ( n_data, sam, suc, pop, n, fx )

!*****************************************************************************80
!
!! HYPERGEOMETRIC_CDF_VALUES returns some values of the hypergeometric CDF.
!
!  Discussion:
!
!    CDF(X)(A,B) is the probability of at most X successes in A trials,
!    given that the probability of success on a single trial is B.
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`DiscreteDistributions`]
!      dist = HypergeometricDistribution [ sam, suc, pop ]
!      CDF [ dist, n ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 September 2004
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
!    Daniel Zwillinger,
!    CRC Standard Mathematical Tables and Formulae,
!    30th Edition, CRC Press, 1996, pages 651-652.
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) SAM, integer SUC, integer POP, the sample
!    size, success size, and population parameters of the function.
!
!    Output, integer ( kind = 4 ) N, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 16

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.6001858177500578D-01, &
    0.2615284665839845D+00, &
    0.6695237889132748D+00, &
    0.1000000000000000D+01, &
    0.1000000000000000D+01, &
    0.5332595856827856D+00, &
    0.1819495964117640D+00, &
    0.4448047017527730D-01, &
    0.9999991751316731D+00, &
    0.9926860896560750D+00, &
    0.8410799901444538D+00, &
    0.3459800113391901D+00, &
    0.0000000000000000D+00, &
    0.2088888139634505D-02, &
    0.3876752992448843D+00, &
    0.9135215248834896D+00 /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ), save, dimension ( n_max ) :: n_vec = (/ &
     7,  8,  9, 10, &
     6,  6,  6,  6, &
     6,  6,  6,  6, &
     0,  0,  0,  0 /)
  integer ( kind = 4 ) pop
  integer ( kind = 4 ), save, dimension ( n_max ) :: pop_vec = (/ &
    100, 100, 100, 100, &
    100, 100, 100, 100, &
    100, 100, 100, 100, &
    90,  200, 1000, 10000 /)
  integer ( kind = 4 ) sam
  integer ( kind = 4 ), save, dimension ( n_max ) :: sam_vec = (/ &
    10, 10, 10, 10, &
     6,  7,  8,  9, &
    10, 10, 10, 10, &
    10, 10, 10, 10 /)
  integer ( kind = 4 ) suc
  integer ( kind = 4 ), save, dimension ( n_max ) :: suc_vec = (/ &
    90, 90, 90, 90, &
    90, 90, 90, 90, &
    10, 30, 50, 70, &
    90, 90, 90, 90 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    sam = 0
    suc = 0
    pop = 0
    n = 0
    fx = 0.0D+00
  else
    sam = sam_vec(n_data)
    suc = suc_vec(n_data)
    pop = pop_vec(n_data)
    n = n_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
