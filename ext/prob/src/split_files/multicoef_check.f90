function multicoef_check ( nfactor, factor )

!*****************************************************************************80
!
!! MULTICOEF_CHECK checks the parameters of the multinomial coefficient.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) NFACTOR, the number of factors.
!    1 <= NFACTOR.
!
!    Input, integer ( kind = 4 ) FACTOR(NFACTOR), contains the factors.
!    0.0 <= FACTOR(I).
!
!    Output, logical MULTICOEF_CHECK, is true if the parameters are legal.
!
  implicit none

  integer ( kind = 4 ) nfactor

  integer ( kind = 4 ) factor(nfactor)
  integer ( kind = 4 ) i
  logical multicoef_check

  if ( nfactor < 1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MULTICOEF_CHECK - Fatal error!'
    write ( *, '(a)' ) '  NFACTOR < 1.'
    multicoef_check = .false.
    return
  end if

  do i = 1, nfactor

    if ( factor(i) < 0 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MULTICOEF_CHECK - Fatal error'
      write ( *, '(a,i8)' ) '  Factor ', I
      write ( *, '(a,i8)' ) '  = ', factor(i)
      write ( *, '(a)' ) '  But this value must be nonnegative.'
      multicoef_check = .false.
      return
    end if

  end do

  multicoef_check = .true.

  return
end
