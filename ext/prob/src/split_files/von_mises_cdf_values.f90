subroutine von_mises_cdf_values ( n_data, a, b, x, fx )

!*****************************************************************************80
!
!! VON_MISES_CDF_VALUES returns some values of the von Mises CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Kanti Mardia, Peter Jupp,
!    Directional Statistics,
!    Wiley, 2000,
!    QA276.M335
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0
!    before the first call.  On each call, the routine increments N_DATA by 1,
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, real ( kind = 8 ) A, a parameter of the PDF.
!    A is the preferred direction, in radians.
!    -PI <= A <= PI.
!
!    Output, real ( kind = 8 ) B, a parameter of the PDF.
!    B measures the "concentration" of the distribution around the
!    angle A.  B = 0 corresponds to a uniform distribution
!    (no concentration).  Higher values of B cause greater concentration
!    of probability near A.
!    0.0 <= B.
!
!    Output, real ( kind = 8 ) X, the argument of the function.
!
!    Output, real ( kind = 8 ) FX, the value of the function.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 23

  real ( kind = 8 ) a
  real ( kind = 8 ), save, dimension ( n_max ) :: a_vec = (/ &
     0.0D+00, &
     0.0D+00, &
     0.0D+00, &
     0.0D+00, &
     0.0D+00, &
     0.1D+01, &
     0.1D+01, &
     0.1D+01, &
     0.1D+01, &
     0.1D+01, &
     0.1D+01, &
    -0.2D+01, &
    -0.1D+01, &
     0.0D+01, &
     0.1D+01, &
     0.2D+01, &
     0.3D+01, &
     0.0D+00, &
     0.0D+00, &
     0.0D+00, &
     0.0D+00, &
     0.0D+00, &
     0.0D+00 /)
  real ( kind = 8 ) b
  real ( kind = 8 ), save, dimension ( n_max ) :: b_vec = (/ &
     0.1D+01, &
     0.1D+01, &
     0.1D+01, &
     0.1D+01, &
     0.1D+01, &
     0.2D+01, &
     0.2D+01, &
     0.2D+01, &
     0.2D+01, &
     0.2D+01, &
     0.2D+01, &
     0.3D+01, &
     0.3D+01, &
     0.3D+01, &
     0.3D+01, &
     0.3D+01, &
     0.3D+01, &
     0.0D+00, &
     0.1D+01, &
     0.2D+01, &
     0.3D+01, &
     0.4D+01, &
     0.5D+01 /)
  real ( kind = 8 ) fx
  real ( kind = 8 ), save, dimension ( n_max ) :: fx_vec = (/ &
    0.2535089956281180D-01, &
    0.1097539041177346D+00, &
    0.5000000000000000D+00, &
    0.8043381312498558D+00, &
    0.9417460124555197D+00, &
    0.5000000000000000D+00, &
    0.6018204118446155D+00, &
    0.6959356933122230D+00, &
    0.7765935901304593D+00, &
    0.8410725934916615D+00, &
    0.8895777369550366D+00, &
    0.9960322705517925D+00, &
    0.9404336090170247D+00, &
    0.5000000000000000D+00, &
    0.5956639098297530D-01, &
    0.3967729448207649D-02, &
    0.2321953958111930D-03, &
    0.6250000000000000D+00, &
    0.7438406999109122D+00, &
    0.8369224904294019D+00, &
    0.8941711407897124D+00, &
    0.9291058600568743D+00, &
    0.9514289900655436D+00 /)
  integer ( kind = 4 ) n_data
  real ( kind = 8 ) x
  real ( kind = 8 ), save, dimension ( n_max ) :: x_vec = (/ &
    -0.2617993977991494D+01, &
    -0.1570796326794897D+01, &
     0.0000000000000000D+00, &
     0.1047197551196598D+01, &
     0.2094395102393195D+01, &
     0.1000000000000000D+01, &
     0.1200000000000000D+01, &
     0.1400000000000000D+01, &
     0.1600000000000000D+01, &
     0.1800000000000000D+01, &
     0.2000000000000000D+01, &
     0.0000000000000000D+00, &
     0.0000000000000000D+00, &
     0.0000000000000000D+00, &
     0.0000000000000000D+00, &
     0.0000000000000000D+00, &
     0.0000000000000000D+00, &
     0.7853981633974483D+00, &
     0.7853981633974483D+00, &
     0.7853981633974483D+00, &
     0.7853981633974483D+00, &
     0.7853981633974483D+00, &
     0.7853981633974483D+00 /)

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
