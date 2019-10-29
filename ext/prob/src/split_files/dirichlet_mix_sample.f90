subroutine dirichlet_mix_sample ( comp_num, elem_num, a, &
  comp_weight, seed, comp, x )

!*****************************************************************************80
!
!! DIRICHLET_MIX_SAMPLE samples a Dirichlet mixture PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) COMP_NUM, the number of components in the
!    Dirichlet mixture density, that is, the number of distinct Dirichlet PDF's
!    that are mixed together.
!
!    Input, integer ( kind = 4 ) ELEM_NUM, the number of elements of
!    an observation.
!
!    Input, real ( kind = 8 ) A(ELEM_NUM,COMP_NUM), the probabilities for
!    element ELEM_NUM in component COMP_NUM.
!    Each A(I,J) should be positive.
!
!    Input, real ( kind = 8 ) COMP_WEIGHT(COMP_NUM), the mixture weights of
!    the densities.  These do not need to be normalized.  The weight of a
!    given component is the relative probability that that component will
!    be used to generate the sample.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, integer ( kind = 4 ) COMP, the index of the component of the
!    Dirichlet mixture that was chosen to generate the sample.
!
!    Output, real ( kind = 8 ) X(ELEM_NUM), a sample of the PDF.
!
  implicit none

  integer ( kind = 4 ) comp_num
  integer ( kind = 4 ) elem_num

  real ( kind = 8 ) a(elem_num,comp_num)
  integer ( kind = 4 ) comp
  real ( kind = 8 ) comp_weight(comp_num)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(elem_num)
!
!  Choose a particular density component COMP.
!
  call discrete_sample ( comp_num, comp_weight, seed, comp )
!
!  Sample the density number COMP.
!
  call dirichlet_sample ( elem_num, a(1,comp), seed, x )

  return
end
