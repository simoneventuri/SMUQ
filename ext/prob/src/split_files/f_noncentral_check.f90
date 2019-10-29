function f_noncentral_check ( a, m, n )

!*****************************************************************************80
!
!! F_NONCENTRAL_CHECK checks the parameters of the F noncentral PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, a parameter of the PDF.
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!    1 <= M,
!    1 <= N.
!
!    Output, logical F_NONCENTRAL_CHECK, is TRUE if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  logical f_noncentral_check
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  if ( a <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F_NONCENTRAL_CHECK - Warning!'
    write ( *, '(a)' ) '  A <= 0.'
    f_noncentral_check = .false.
    return
  end if

  if ( m < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F_NONCENTRAL_CHECK - Warning!'
    write ( *, '(a)' ) '  M < 1.'
    f_noncentral_check = .false.
    return
  end if

  if ( n < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'F_NONCENTRAL_CHECK - Warning!'
    write ( *, '(a)' ) '  N < 1.'
    f_noncentral_check = .false.
    return
  end if

  f_noncentral_check = .true.

  return
end
