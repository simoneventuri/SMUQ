subroutine multinomial_coef1 ( nfactor, factor, ncomb )

!*****************************************************************************80
!
!! MULTINOMIAL_COEF1 computes a Multinomial coefficient.
!
!  Discussion:
!
!    The multinomial coefficient is a generalization of the binomial
!    coefficient.  It may be interpreted as the number of combinations of
!    N objects, where FACTOR(1) objects are indistinguishable of type 1,
!    ... and FACTOR(NFACTOR) are indistinguishable of type NFACTOR,
!    and N is the sum of FACTOR(1) through FACTOR(NFACTOR).
!
!    NCOMB = N! / ( FACTOR(1)! FACTOR(2)! ... FACTOR(NFACTOR)! )
!
!    The log of the gamma function is used, to avoid overflow.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 February 1999
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
!    Output, integer ( kind = 4 ) NCOMB, the value of the multinomial
!    coefficient.
!
  implicit none

  integer ( kind = 4 ) nfactor

  logical check
  real ( kind = 8 ) facn
  integer ( kind = 4 ) factor(nfactor)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_huge
  logical multicoef_check
  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncomb

  check = multicoef_check ( nfactor, factor )

  if ( .not. check ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MULTINOMIAL_COEF1 - Fatal error!'
    write ( *, '(a)' ) '  MULTICOEF_CHECK failed.'
    ncomb = - i4_huge ( )
    return
  end if
!
!  The factors sum to N.
!
  n = sum ( factor(1:nfactor) )

  facn = lgamma ( real ( n + 1, kind = 8 ) )

  do i = 1, nfactor

    facn = facn - lgamma ( real ( factor(i) + 1, kind = 8 ) )

  end do

  ncomb = nint ( exp ( facn ) )

  return
end
