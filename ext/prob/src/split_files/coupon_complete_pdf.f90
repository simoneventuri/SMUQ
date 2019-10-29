subroutine coupon_complete_pdf ( type_num, box_num, pdf )

!*****************************************************************************80
!
!! COUPON_COMPLETE_PDF evaluates the Complete Coupon Collection PDF.
!
!  Discussion:
!
!    PDF(TYPE_NUM;BOX_NUM) is the probability that, given an inexhaustible
!    supply of boxes, inside each of which there is one of TYPE_NUM distinct
!    coupons, which are uniformly distributed among the boxes, that it will
!    require opening exactly BOX_NUM boxes to achieve at least one of each
!    kind of coupon.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    25 August 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Herbert Wilf,
!    Some New Aspects of the Coupon Collector's Problem,
!    SIAM Review,
!    Volume 48, Number 3, September 2006, pages 549-565.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) BOX_NUM, the number of boxes that had to be
!    opened in order to just get at least one of each coupon.
!    0 <= BOX_NUM.  If BOX_NUM < TYPE_NUM, then PDF is surely 0.
!
!    Input, integer ( kind = 4 ) TYPE_NUM, the number of distinct coupons.
!    1 <= TYPE_NUM.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ) box_num
  real ( kind = 8 ) factor
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) stirling2_value
  integer ( kind = 4 ) type_num
!
!  Nonsense cases.
!
  if ( box_num < 0 ) then

    pdf = 0.0D+00

  else if ( type_num < 1 ) then

    pdf = 0.0D+00
!
!  Degenerate but meaningful case.
!
  else if ( type_num == 1 ) then

    if ( box_num == 1 ) then
      pdf = 1.0D+00
    else
      pdf = 0.0D+00
    end if
!
!  Easy cases.
!
  else if ( box_num < type_num ) then

    pdf = 0.0D+00
!
!  General case.
!
  else

    factor = 1.0D+00
    do i = 1, type_num
      factor = factor * real ( i, kind = 8 ) / real ( type_num, kind = 8 )
    end do
    do i = type_num + 1, box_num
      factor = factor / real ( type_num, kind = 8 )
    end do

    pdf = factor * real ( &
      stirling2_value ( box_num - 1, type_num - 1 ), kind = 8 )

  end if

  return
end
