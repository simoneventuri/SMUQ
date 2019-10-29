subroutine random_initialize ( seed_input )

!*****************************************************************************80
!
!! RANDOM_INITIALIZE initializes the FORTRAN90 random number seed.
!
!  Discussion:
!
!    If you don't initialize the FORTRAN90 random number generator
!    routine RANDOM_NUMBER, which is used by calls like
!
!      call random_number ( harvest = x )
!
!    then its behavior is not specified.  That may be OK for you.  But
!    you may want to be able to force the same sequence to be generated
!    each time, or to force a different sequence.
!
!    To control the sequence of random numbers, you need to set the seed.
!    In FORTRAN90, this is done by calling the RANDOM+SEED routine.
!    You can call it with no arguments, in fact.  But if you call
!    it with no arguments:
!
!      call random_seed ( )
!
!    then its behavior (or more particularly, the behavior of RANDOM_NUMBER)
!    is still not specified.  You might hope that the system will go out
!    and pick a nice random seed for you, but there's no guarantee.
!
!
!    For example, on the DEC ALPHA, if you compile a program that calls
!    RANDOM_NUMBER, then every time you run it, you get the same sequence
!    of "random" values.  If you compile a program that calls RANDOM_SEED
!    with no arguments, and then calls RANDOM_NUMBER, you still get the
!    same sequence each time you run the program.
!
!    In order to actually try to scramble up the random number generator
!    a bit, this routine goes through the tedious process of getting the
!    size of the random number seed, making up values based on the current
!    time, and setting the random number seed.
!
!    Unfortunately, the RANDOM_SEED routine has a very elastic definition.
!    It does not use a single scalar integer SEED.  Instead, it communicates
!    with the user through an integer vector whose size is not specified.
!    You actually have to "ask" the routine to tell you the size of this
!    vector.  Then you can fill up the vector with values to be used to
!    influence the seeding of the random number routine.  The details of
!    how the seed affects the sequence are also unspecified, but the only
!    thing we are reasonably confident about is that specifying the same
!    seed should result in the same sequence, and specifying different
!    seeds should result in different sequences!
!
!    I assume this is far more than you wanted to know.  (It's certainly
!    more than I wanted to know!)
!
!    The argument SEED is an input quantity only, so it is legal
!    to type
!
!      call random_initialize ( 0 )
!
!    or
!
!      call random_initialize ( 18867 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 February 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) SEED_INPUT, the user's "suggestion" for a seed.
!    However, if the input value is 0, the routine will come up with
!    its own "suggestion", based on the system clock.
!
  implicit none

  integer ( kind = 4 ) count
  integer ( kind = 4 ) count_max
  integer ( kind = 4 ) count_rate
  logical, parameter :: debug = .false.
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) seed_input
  integer ( kind = 4 ), allocatable :: seed_vector(:)
  integer ( kind = 4 ) seed_size
  real ( kind = 8 ) t
  integer ( kind = 4 ), parameter :: warm_up = 100

  seed = seed_input
!
!  Initialize the random seed routine.
!
  call random_seed ( )
!
!  Determine the size of the random number seed vector.
!
  call random_seed ( size = seed_size )
!
!  Allocate a vector of the right size to be used as a random seed.
!
  allocate ( seed_vector(seed_size) )
!
!  If the user supplied a SEED value, use that.
!
!  Otherwise, use the system clock value to make up a value that is
!  likely to change based on when this routine is called.
!
  if ( seed /= 0 ) then

    if ( debug ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'RANDOM_INITIALIZE'
      write ( *, '(a,i20)' ) '  Initialize RANDOM_NUMBER, user SEED = ', seed
    end if

  else

    call system_clock ( count, count_rate, count_max )

    seed = count

    if ( debug ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'RANDOM_INITIALIZE'
      write ( *, '(a,i20)' ) '  Initialize RANDOM_NUMBER, arbitrary SEED = ', &
        seed
    end if

  end if
!
!  Set the seed vector.  We don't know the significance of the
!  individual entries of the internal seed vector, so we'll just set
!  all entries to SEED.
!
  seed_vector(1:seed_size) = seed
!
!  Now call RANDOM_SEED, and tell it to use this seed vector.
!
  call random_seed ( put = seed_vector(1:seed_size) )
!
!  Free up the seed space.
!
  deallocate ( seed_vector )
!
!  Call the random number routine a bunch of times just to "warm it up".
!
  do i = 1, warm_up
    call random_number ( harvest = t )
  end do

  return
end
