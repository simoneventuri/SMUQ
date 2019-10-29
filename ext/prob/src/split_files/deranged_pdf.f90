subroutine deranged_pdf ( x, a, pdf )

!*****************************************************************************80
!
!! DERANGED_PDF evaluates the Deranged PDF.
!
!  Discussion:
!
!    PDF(A;X) is the probability that exactly X items will occur in
!    their proper place after a random permutation of A items.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the number of items in their
!    correct places.  0 <= X <= A.
!
!    Input, integer ( kind = 4 ) A, the total number of items.
!    1 <= A.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) cnk
  integer ( kind = 4 ) deranged_enum
  integer ( kind = 4 ) dnmk
  integer ( kind = 4 ) i4_choose
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) x

  if ( x < 0 .or. a < x ) then
    pdf = 0.0D+00
  else
    cnk = i4_choose ( a, x )
    dnmk = deranged_enum ( a - x )
    pdf = real ( cnk * dnmk, kind = 8 ) / lgamma ( real ( a + 1, kind = 8 ) )
  end if

  return
end
