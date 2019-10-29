function weibull_discrete_check ( a, b )

!*****************************************************************************80
!
!! WEIBULL_DISCRETE_CHECK checks the parameters of the discrete Weibull CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 <= A <= 1.0D+00,
!    0.0 < B.
!
!    Output, logical WEIBULL_DISCRETE_CHECK, is true if the parameters
!    are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical weibull_discrete_check

  if ( a < 0.0D+00 .or. 1.0D+00 < a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WEIBULL_DISCRETE_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A < 0 or 1 < A.'
    weibull_discrete_check = .false.
    return
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'WEIBULL_DISCRETE_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    weibull_discrete_check = .false.
    return
  end if

  weibull_discrete_check = .true.

  return
end
