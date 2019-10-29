subroutine uniform_01_order_sample ( n, seed, x )

!*****************************************************************************80
!
!! UNIFORM_01_ORDER_SAMPLE samples the Uniform 01 Order PDF.
!
!  Discussion:
!
!    In effect, this routine simply generates N samples of the
!    Uniform 01 PDF; but it generates them in order.  (Actually,
!    it generates them in descending order, but stores them in
!    the array in ascending order).  This saves the work of
!    sorting the results.  Moreover, if the order statistics
!    for another PDF are desired, and the inverse CDF is available,
!    then the desired values may be generated, presorted, by
!    calling this routine and using the results as input to the
!    inverse CDF routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jerry Banks, editor,
!    Handbook of Simulation,
!    Engineering and Management Press Books, 1998, page 168.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements in the sample.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(N), N samples of the Uniform 01 PDF, in
!    ascending order.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) r8_uniform_01
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u
  real ( kind = 8 ) v
  real ( kind = 8 ) x(n)

  v = 1.0D+00
  do i = n, 1, -1
    u = r8_uniform_01 ( seed )
    v = v * u ** ( 1.0D+00 / real ( i, kind = 8 ) )
    x(i) = v
  end do

  return
end
