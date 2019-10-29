subroutine poisson_kernel ( r, n, c, x, y, p )

!*****************************************************************************80
!
!! POISSON_KERNEL evaluates the Poisson kernel.
!
!  Discussion:
!
!    P(X,Y) = ( R^2 - |X-C|^2 ) / ( R * A * |X-Y|^N )
!
!    where the N-dimensional ball has radius R and center C,
!    and A is the area of the unit sphere.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the radius of the ball.
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!
!    Input, real ( kind = 8 ) C(N), the center of the ball.
!
!    Input, real ( kind = 8 ) X(N), a point inside the ball.
!
!    Input, real ( kind = 8 ) Y(N), a point on the surface of the ball.
!
!    Output, real ( kind = 8 ) P, the Poisson kernel function P(X,Y).
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) area
  real ( kind = 8 ) b
  real ( kind = 8 ) c(n)
  real ( kind = 8 ) p
  real ( kind = 8 ) r
  real ( kind = 8 ) r8vec_diff_norm
  real ( kind = 8 ) sphere_unit_area_nd
  real ( kind = 8 ) t
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xc_diff_norm
  real ( kind = 8 ) xy_diff_norm
  real ( kind = 8 ) y(n)

  xc_diff_norm = r8vec_diff_norm ( n, x, c )
  xy_diff_norm = r8vec_diff_norm ( n, x, y )
  area = sphere_unit_area_nd ( n )

  t = ( r + xc_diff_norm ) * ( r - xc_diff_norm )
  b = r * area * ( xy_diff_norm ) ** n
  p = t / b

  return
end
