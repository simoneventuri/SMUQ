function pareto_check ( a, b )

!*****************************************************************************80
!
!! PARETO_CHECK checks the parameters of the Pareto CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the parameters of the PDF.
!    0.0 < A,
!    0.0 < B.
!
!    Output, logical PARETO_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical pareto_check

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PARETO_CHECK - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.'
    pareto_check = .false.
    return
  end if

  if ( b <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'PARETO_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= 0.'
    pareto_check = .false.
    return
  end if

  pareto_check = .true.

  return
end
