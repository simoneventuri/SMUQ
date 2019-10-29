subroutine log_series_cdf_values ( n_data, t, n, fx )

!*****************************************************************************80
!
!! LOG_SERIES_CDF_VALUES returns some values of the log series CDF.
!
!  Discussion:
!
!    In Mathematica, the function can be evaluated by:
!
!      Needs["Statistics`DiscreteDistributions`]
!      dist = LogSeriesDistribution [ t ]
!      CDF [ dist, n ]
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 August 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
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
!    Output, real ( kind = 8 ) T, the parameter of the function.
!
!    Output, integer ( kind = 4 ) N, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 29

  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.9491221581029903D+00, &
    0.9433541128559735D+00, &
    0.9361094611773272D+00, &
    0.9267370278044118D+00, &
    0.9141358246245129D+00, &
    0.8962840235449100D+00, &
    0.8690148741955517D+00, &
    0.8221011541254772D+00, &
    0.7213475204444817D+00, &
    0.6068261510845583D+00, &
    0.5410106403333613D+00, &
    0.4970679476476894D+00, &
    0.4650921887927060D+00, &
    0.4404842934597863D+00, &
    0.4207860535926143D+00, &
    0.4045507673897055D+00, &
    0.3908650337129266D+00, &
    0.2149757685421097D+00, &
    0.0000000000000000D+00, &
    0.2149757685421097D+00, &
    0.3213887739704539D+00, &
    0.3916213575531612D+00, &
    0.4437690508633213D+00, &
    0.4850700239649681D+00, &
    0.5191433267738267D+00, &
    0.5480569580144867D+00, &
    0.5731033910767085D+00, &
    0.5951442521714636D+00, &
    0.6147826594068904D+00 /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_data
  integer ( kind = 4 ), save, dimension ( n_max ) :: n_vec = (/ &
     1, 1, 1, 1, 1, &
     1, 1, 1, 1, 1, &
     1, 1, 1, 1, 1, &
     1, 1, 1, 0, 1, &
     2, 3, 4, 5, 6, &
     7, 8, 9, 10 /)
  real ( kind = 8 ) t
  real ( kind = 8 ), save, dimension ( n_max ) :: t_vec = (/ &
    0.1000000000000000D+00, &
    0.1111111111111111D+00, &
    0.1250000000000000D+00, &
    0.1428571428571429D+00, &
    0.1666666666666667D+00, &
    0.2000000000000000D+00, &
    0.2500000000000000D+00, &
    0.3333333333333333D+00, &
    0.5000000000000000D+00, &
    0.6666666666666667D+00, &
    0.7500000000000000D+00, &
    0.8000000000000000D+00, &
    0.8333333333333333D+00, &
    0.8571485714857149D+00, &
    0.8750000000000000D+00, &
    0.8888888888888889D+00, &
    0.9000000000000000D+00, &
    0.9900000000000000D+00, &
    0.9900000000000000D+00, &
    0.9900000000000000D+00, &
    0.9900000000000000D+00, &
    0.9900000000000000D+00, &
    0.9900000000000000D+00, &
    0.9900000000000000D+00, &
    0.9900000000000000D+00, &
    0.9900000000000000D+00, &
    0.9900000000000000D+00, &
    0.9900000000000000D+00, &
    0.9900000000000000D+00 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    t = 0.0D+00
    n = 0
    fx = 0.0D+00
  else
    t = t_vec(n_data)
    n = n_vec(n_data)
    fx = fx_vec(n_data)
  end if

  return
end
