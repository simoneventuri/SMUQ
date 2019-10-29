subroutine dirichlet_mix_pdf ( x, comp_num, elem_num, a, &
  comp_weight, pdf )

!*****************************************************************************80
!
!! DIRICHLET_MIX_PDF evaluates a Dirichlet mixture PDF.
!
!  Discussion:
!
!    The PDF is a weighted sum of Dirichlet PDF's.  Each PDF is a
!    "component", with an associated weight.
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
!    Input, real ( kind = 8 ) X(ELEM_NUM), the argument of the PDF.
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
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ) comp_num
  integer ( kind = 4 ) elem_num

  real ( kind = 8 ) a(elem_num,comp_num)
  integer ( kind = 4 ) comp_i
  real ( kind = 8 ) comp_pdf
  real ( kind = 8 ) comp_weight(comp_num)
  real ( kind = 8 ) comp_weight_sum
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x(elem_num)

  comp_weight_sum = sum ( comp_weight )

  pdf = 0.0D+00
  do comp_i = 1, comp_num

    call dirichlet_pdf ( x, elem_num, a(1,comp_i), comp_pdf )

    pdf = pdf + comp_weight(comp_i) * comp_pdf / comp_weight_sum

  end do

  return
end
