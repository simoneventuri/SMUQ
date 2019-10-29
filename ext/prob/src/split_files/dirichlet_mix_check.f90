function dirichlet_mix_check ( comp_num, elem_num, a, comp_weight )

!*****************************************************************************80
!
!! DIRICHLET_MIX_CHECK checks the parameters of a Dirichlet mixture PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) COMP_NUM, the number of components in the
!    Dirichlet
!    mixture density, that is, the number of distinct Dirichlet PDF's
!    that are mixed together.
!
!    Input, integer ( kind = 4 ) ELEM_NUM, the number of elements of an
!    observation.
!
!    Input, real ( kind = 8 ) A(ELEM_NUM,COMP_NUM), the probabilities
!    for element ELEM_NUM in component COMP_NUM.
!    Each A(I,J) should be positive.
!
!    Input, real ( kind = 8 ) COMP_WEIGHT(COMP_NUM), the mixture weights of
!    the densities.  These do not need to be normalized.  The weight of a
!    given component is the relative probability that that component will
!    be used to generate the sample.
!
!    Output, logical DIRICHLET_MIX_CHECK, is true if the parameters are legal.
!
  implicit none

  integer ( kind = 4 ) comp_num
  integer ( kind = 4 ) elem_num

  real ( kind = 8 ) a(elem_num,comp_num)
  integer ( kind = 4 ) comp_i
  real ( kind = 8 ) comp_weight(comp_num)
  logical dirichlet_mix_check
  integer ( kind = 4 ) elem_i
  logical positive

  do comp_i = 1, comp_num

    do elem_i = 1, elem_num
      if ( a(elem_i,comp_i) <= 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DIRICHLET_MIX_CHECK - Warning!'
        write ( *, '(a)' ) '  A(ELEM,COMP) <= 0.'
        write ( *, '(a,i8)' ) '  COMP = ', comp_i
        write ( *, '(a,i8)' ) '  ELEM = ', elem_i
        write ( *, '(a,g14.6)' ) '  A(COMP,ELEM) = ', a(elem_i,comp_i)
        dirichlet_mix_check = .false.
        return
      end if
    end do

  end do

  positive = .false.

  do comp_i = 1, comp_num

    if ( comp_weight(comp_i) < 0.0D+00 ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DIRICHLET_MIX_CHECK - Warning!'
      write ( *, '(a)' ) '  COMP_WEIGHT(COMP) < 0.'
      write ( *, '(a,i8)' ) '  COMP = ', comp_i
      write ( *, '(a,g14.6)' ) '  COMP_WEIGHT(COMP) = ', comp_weight(comp_i)
      dirichlet_mix_check = .false.
      return
    else if ( 0.0D+00 < comp_weight(comp_i) ) then
      positive = .true.
    end if

  end do

  if ( .not. positive ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'DIRICHLET_MIX_CHECK - Warning!'
    write ( *, '(a)' ) '  All component weights are zero.'
    dirichlet_mix_check = .false.
    return
  end if

  dirichlet_mix_check = .true.

  return
end
