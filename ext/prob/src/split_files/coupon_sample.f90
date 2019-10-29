subroutine coupon_sample ( type_num, seed, coupon, box_num )

!*****************************************************************************80
!
!! COUPON_SAMPLE simulates the coupon collector's problem.
!
!  Discussion:
!
!    The coupon collector needs to collect one of each of TYPE_NUM
!    coupons.  The collector may draw one coupon (or, in some settings,
!    open one box) on each trial, and takes as many trials as necessary
!    to complete the task.  On each trial, the probability of picking
!    any particular type of coupon is always 1 / TYPE_NUM.
!
!    Interesting questions include;
!
!    * what is the expected number of drawings necessary to complete
!      the collection?
!
!    * How does the expected number of drawings necessary to complete
!      the collection vary as TYPE_NUM increases?
!
!    * What is the distribution of the numbers of each type of coupon
!      in a typical collection when it is just completed?
!
!    As TYPE_NUM increases, the number of coupons necessary to be
!    collected in order to get a complete set in any simulation
!    strongly tends to the value TYPE_NUM * LOG ( TYPE_NUM ).
!
!    If TYPE_NUM is 1, the simulation ends with a single drawing.
!
!    If TYPE_NUM is 2, then we may call the coupon taken on the first drawing
!    a "Head", say, and the process then is similar to the question of the
!    length, plus one, of a run of Heads or Tails in coin flipping.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 May 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) TYPE_NUM, the number of types of coupons.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random number
!    generator.
!
!    Output, integer ( kind = 4 ) COUPON(TYPE_NUM), the number of coupons
!    of each type that were collected during the simulation.
!
!    Output, integer ( kind = 4 ) BOX_NUM, the total number of boxes opened.
!
  implicit none

  integer ( kind = 4 ) type_num

  integer ( kind = 4 ), parameter :: box_max = 2000
  integer ( kind = 4 ) box_num
  integer ( kind = 4 ) coupon(type_num)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uniform_ab
  integer ( kind = 4 ) seed
  integer ( kind = 4 ) straight

  coupon(1:type_num) = 0

  straight = 0
  box_num = 0
!
!  Draw another coupon.
!
  do while ( box_num < box_max )

    i = i4_uniform_ab ( 1, type_num, seed )
!
!  Increment the number of I coupons.
!
    coupon(i) = coupon(i) + 1
    box_num = box_num + 1
!
!  If I is the next one we needed, increase STRAIGHT by 1.
!
    if ( i == straight + 1 ) then

      do

        straight = straight + 1
!
!  If STRAIGHT = TYPE_NUM, we have all of them.
!
        if ( type_num <= straight ) then
          return
        end if
!
!  If the next coupon has not been collected, our straight is over.
!
        if ( coupon(straight+1) <= 0 ) then
          exit
        end if

      end do

    end if

  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'COUPON_SAMPLE - Fatal error!'
  write ( *, '(a)' ) '  Maximum number of coupons drawn without success.'

  stop 1
end
