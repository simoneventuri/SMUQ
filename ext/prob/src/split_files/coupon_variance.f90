subroutine coupon_variance ( j, type_num, variance )

!*****************************************************************************80
!
!! COUPON_VARIANCE returns the variance of the Coupon PDF.
!
!  Discussion:
!
!    In this version of the coupon collector's problem, we assume
!    that each box contains 1 coupon, that there are TYPE_NUM distinct types
!    of coupon, uniformly distributed among an inexhaustible supply
!    of boxes, and that the collector's goal is to get J distinct
!    types of coupons by opening one box after another.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) J, the number of distinct coupons to be
!    collected.  1 <= J <= TYPE_NUM.
!
!    Input, integer ( kind = 4 ) TYPE_NUM, the number of types of coupons.
!
!    Output, real ( kind = 8 ) VARIANCE, the variance of the number of
!    boxes that must be opened in order to just get J distinct kinds
!    of coupons.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) type_num
  real ( kind = 8 ) variance

  if ( type_num < j ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COUPON_VARIANCE - Fatal error!'
    write ( *, '(a)' ) '  Number of distinct coupons desired must be no more'
    write ( *, '(a)' ) '  than the total number of distinct coupons.'
    stop 1
  end if

  variance = 0.0D+00
  do i = 1, j
    variance = variance + real ( i - 1, kind = 8 ) &
      / real ( type_num - i + 1, kind = 8 ) ** 2
  end do
  variance = variance * real ( type_num, kind = 8 )

  return
end
