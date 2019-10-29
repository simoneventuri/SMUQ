subroutine dirichlet_mix_mean ( comp_num, elem_num, a, comp_weight, &
  mean )

!*****************************************************************************80
!
!! DIRICHLET_MIX_MEAN returns the means of a Dirichlet mixture PDF.
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
!    Input, integer ( kind = 4 ) ELEM_NUM, the number of elements of an
!    observation.
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
!    Output, real ( kind = 8 ) MEAN(ELEM_NUM), the means for each element.
!
  implicit none

  integer ( kind = 4 ) comp_num
  integer ( kind = 4 ) elem_num

  real ( kind = 8 ) a(elem_num,comp_num)
  integer ( kind = 4 ) comp_i
  real ( kind = 8 ) comp_mean(elem_num)
  real ( kind = 8 ) comp_weight(comp_num)
  real ( kind = 8 ) comp_weight_sum
  real ( kind = 8 ) mean(elem_num)

  comp_weight_sum = sum ( comp_weight )

  mean(1:elem_num) = 0.0D+00

  do comp_i = 1, comp_num
    call dirichlet_mean ( elem_num, a(1,comp_i), comp_mean )
    mean(1:elem_num) = mean(1:elem_num) &
      + comp_weight(comp_i) * comp_mean(1:elem_num)
  end do

  mean(1:elem_num) = mean(1:elem_num) / comp_weight_sum

  return
end
