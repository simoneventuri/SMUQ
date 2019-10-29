function buffon_sample ( a, l, trial_num )

!*****************************************************************************80
!
!! BUFFON_SAMPLE samples the Buffon PDF.
!
!  Discussion:
!
!    In the Buffon needle experiment, we suppose that the plane has been
!    ruled by vertical lines with a spacing of A units, and that a
!    needle of length L is dropped "at random" onto this grid.
!
!    Because of the various symmetries, we may assume that this eye of
!    this needle lands in the first infinite strip, and we may further
!    assume that its Y coordinate is 0.  Thus, we have
!    the eye as (X1,Y1) with 0 <= X1 <= A and Y1 = 0.
!
!    ANGLE, the angle that the needle makes is taken to be uniformly random.
!    The point of the needle, (X2,Y2), therefore lies at
!
!      (X2,Y2) = ( X1+L*cos(ANGLE), Y1+L*sin(ANGLE) )
!
!    The needle will have crossed at least one grid line if any of the
!    following are true:
!
!      X2 <= 0, A <= X2.
!
!    The probability of a crossing on a single trial is
!
!      P(A,L) = ( 2 * L ) / ( PI * A )
!
!    and therefore, a record of the number of hits for a given number of
!    trials can be used as a very roundabout way of estimating PI.
!
!    Note that this routine will try to generate 4 * TRIAL_NUM random
!    double precision values at one time, using automatic arrays.
!    When I tried this with TRIAL_NUM = 1,000,000, the program failed,
!    because of internal system limits on such arrays.
!
!    Such a problem could be avoided by using a DO loop running through
!    each trial individually, but this tend to run much more slowly than
!    necessary.
!
!    Since this routine invokes the FORTRAN90 random number generator,
!    the user should initialize the random number generator, particularly
!    if it is desired to control whether the sequence is to be varied
!    or repeated.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2007
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, the horizontal spacing between the
!    vertical grid lines.  0 <= A.
!
!    Input, real ( kind = 8 ) L, the length of the needle.
!
!    Input, integer ( kind = 4 ) TRIAL_NUM, the number of times the needle is
!    to be dropped onto the grid.
!
!    Output, integer ( kind = 4 ) BUFFON_SAMPLE, the number of times the
!    needle crossed at least one line of the grid of cells.
!
!  Local Parameters:
!
!    Local, integer BATCH_SIZE, specifies the number of trials to be done
!    in a single batch.  Setting BATCH_SIZE to 1 will be very slow.
!    Replacing it by TRIAL_NUM would be fine except that your system
!    may have a limit on the size of automatic arrays.  We have set a default
!    value of 10,000 here which should be large enough to be efficient
!    but small enough not to annoy the system.
!
  implicit none

  integer ( kind = 4 ), parameter :: batch_size = 10000
  integer ( kind = 4 ) trial_num

  real ( kind = 8 ) a
  real ( kind = 8 ) angle(batch_size)
  integer ( kind = 4 ) batch
  integer ( kind = 4 ) buffon_sample
  integer ( kind = 4 ) hits
  real ( kind = 8 ) l
  integer ( kind = 4 ) n
  real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793238462643D+00
  real ( kind = 8 ) x1(batch_size)
  real ( kind = 8 ) x2(batch_size)

  hits = 0

  do batch = 1, trial_num, batch_size

    n = min ( batch_size, trial_num + 1 - batch )
!
!  Randomly choose the location (X1,Y1) of the eye of the needle
!  in [0,0]x[A,0], and the angle the needle makes.
!
    call random_number ( harvest = x1(1:n) )
    call random_number ( harvest = angle(1:n) )

    x1(1:n) = a * x1(1:n)
    angle(1:n) = 2.0D+00 * r8_pi * angle(1:n)
!
!  Compute the location of the point of the needle.
!  We only need to know the value of X2, not Y2!
!
    x2(1:n) = x1(1:n) + l * cos ( angle(1:n) )
!
!  Count the end locations that lie outside the cell.
!
    hits = hits + count (      x2(1:n) <= 0.0 .or. &
                          a <= x2(1:n) )

  end do

  buffon_sample = hits

  return
end
