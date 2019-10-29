subroutine coupon_mean ( j, type_num, mean )

!*****************************************************************************80
!
!! COUPON_MEAN returns the mean of the Coupon PDF.
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
!    collected.  J must be between 1 and TYPE_NUM.
!
!    Input, integer ( kind = 4 ) TYPE_NUM, the number of distinct coupons.
!
!    Output, real ( kind = 8 ) MEAN, the mean number of boxes that
!    must be opened in order to just get J distinct kinds.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  real ( kind = 8 ) mean
  integer ( kind = 4 ) type_num

  if ( type_num < j ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'COUPON_MEAN - Fatal error!'
    write ( *, '(a)' ) '  Number of distinct coupons desired must be no more'
    write ( *, '(a)' ) '  than the total number of distinct coupons.'
    stop 1
  end if

  mean = 0.0D+00
  do i = 1, j
    mean = mean + 1.0D+00 / real ( type_num - i + 1, kind = 8 )
  end do
  mean = mean * real ( type_num, kind = 8 )

  return
end
