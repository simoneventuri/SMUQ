subroutine hypergeometric_pdf ( x, n, m, l, pdf )

!*****************************************************************************80
!
!! HYPERGEOMETRIC_PDF evaluates the Hypergeometric PDF.
!
!  Discussion:
!
!    PDF(N,M,L;X) = C(M,X) * C(L-M,N-X) / C(L,N).
!
!    PDF(N,M,L;X) is the probability of drawing X white balls in a
!    single random sample of size N from a population containing
!    M white balls and a total of L balls.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) X, the desired number of white balls.
!    0 <= X <= N, usually, although any value of X can be given.
!
!    Input, integer ( kind = 4 ) N, the number of balls selected.
!    0 <= N <= L.
!
!    Input, integer ( kind = 4 ) M, the number of white balls.
!    0 <= M <= L.
!
!    Input, integer ( kind = 4 ) L, the number of balls to select from.
!    0 <= L.
!
!    Output, real ( kind = 8 ) PDF, the probability of exactly K white balls.
!
  implicit none

  real ( kind = 8 ) c1
  real ( kind = 8 ) c2
  real ( kind = 8 ) c3
  real ( kind = 8 ) i4_choose_log
  integer ( kind = 4 ) l
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) pdf
  real ( kind = 8 ) pdf_log
  integer ( kind = 4 ) x
!
!  Special cases.
!
  if ( x < 0 ) then

    pdf = 1.0D+00

  else if ( n < x ) then

    pdf = 0.0D+00

  else if ( m < x ) then

    pdf = 0.0D+00

  else if ( l < x ) then

    pdf = 0.0D+00

  else if ( n == 0 ) then

    if ( x == 0 ) then
      pdf = 1.0D+00
    else
      pdf = 0.0D+00
    end if

  else

    c1 = i4_choose_log ( m, x )
    c2 = i4_choose_log ( l - m, n - x )
    c3 = i4_choose_log ( l, n )

    pdf_log = c1 + c2 - c3

    pdf = exp ( pdf_log )

  end if

  return
end
