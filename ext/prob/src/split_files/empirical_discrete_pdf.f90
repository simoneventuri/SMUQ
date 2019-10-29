subroutine empirical_discrete_pdf ( x, a, b, c, pdf )

!*****************************************************************************80
!
!! EMPIRICAL_DISCRETE_PDF evaluates the Empirical Discrete PDF.
!
!  Discussion:
!
!    A set of A values C(1:A) are assigned nonnegative weights B(1:A),
!    with at least one B nonzero.  The probability of C(I) is the
!    value of B(I) divided by the sum of the weights.
!
!    The C's must be distinct, and given in ascending order.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 June 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) X, the argument of the PDF.
!
!    Input, integer ( kind = 4 ) A, the number of values.
!    0 < A.
!
!    Input, real ( kind = 8 ) B(A), the weights of each value.
!    0 <= B(1:A) and at least one value is nonzero.
!
!    Input, real ( kind = 8 ) C(A), the values.
!    The values must be distinct and in ascending order.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ) a

  real ( kind = 8 ) b(a)
  real ( kind = 8 ) c(a)
  integer ( kind = 4 ) i
  real ( kind = 8 ) pdf
  real ( kind = 8 ) x

  do i = 1, a
    if ( x == c(i) ) then
      pdf = b(i) / sum ( b(1:a) )
      return
    end if
  end do

  pdf = 0.0D+00

  return
end
