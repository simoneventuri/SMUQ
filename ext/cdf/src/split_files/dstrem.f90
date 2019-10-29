function dstrem ( z )

!*****************************************************************************80
!
!! DSTREM computes the Sterling remainder ln ( Gamma ( Z ) ) - Sterling ( Z ).
!
!  Discussion:
!
!    This routine returns
!
!      ln ( Gamma ( Z ) ) - Sterling ( Z )
!
!    where Sterling(Z) is Sterling's approximation to ln ( Gamma ( Z ) ).
!
!    Sterling(Z) = ln ( sqrt ( 2 * PI ) ) + ( Z - 0.5 ) * ln ( Z ) - Z
!
!    If 6 <= Z, the routine uses 9 terms of a series in Bernoulli numbers,
!    with values calculated using Maple.
!
!    Otherwise, the difference is computed explicitly.
!
!  Modified:
!
!    14 June 2004
!
!  Parameters:
!
!    Input, real ( kind = 8 ) Z, the value at which the Sterling
!    remainder is to be calculated.  Z must be positive.
!
!    Output, real ( kind = 8 ) DSTREM, the Sterling remainder.
!
  implicit none

  integer ( kind = 4 ), parameter :: ncoef = 9

  real ( kind = 8 ), parameter, dimension ( 0:ncoef ) :: coef = (/ &
    0.0D+00, &
    0.0833333333333333333333333333333D+00, &
    -0.00277777777777777777777777777778D+00, &
    0.000793650793650793650793650793651D+00, &
    -0.000595238095238095238095238095238D+00, &
    0.000841750841750841750841750841751D+00, &
    -0.00191752691752691752691752691753D+00, &
    0.00641025641025641025641025641026D+00, &
    -0.0295506535947712418300653594771D+00, &
    0.179644372368830573164938490016D+00 /)
  real ( kind = 8 ) dstrem
  real ( kind = 8 ) eval_pol
  real ( kind = 8 ) gamma_log
  real ( kind = 8 ), parameter :: hln2pi = 0.91893853320467274178D+00
  real ( kind = 8 ) sterl
  real ( kind = 8 ) z

  if ( z <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DSTREM - Fatal error!'
    write ( *, '(a)' ) '  Zero or negative argument Z.'
    stop
  end if

  if ( 6.0D+00 < z ) then
    dstrem = eval_pol ( coef, ncoef, 1.0D+00 / z**2 ) * z
  else
    sterl = hln2pi + ( z - 0.5D+00 ) * log ( z ) - z
    dstrem = gamma_log ( z ) - sterl
  end if

  return
end
