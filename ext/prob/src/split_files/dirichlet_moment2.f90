subroutine dirichlet_moment2 ( n, a, m2 )

!*****************************************************************************80
!
!! DIRICHLET_MOMENT2 returns the second moments of the Dirichlet PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 November 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components.
!
!    Input, real ( kind = 8 ) A(N), the probabilities for each component.
!    Each A(I) should be positive.
!
!    Output, real ( kind = 8 ) M2(N,N), the second moments of the PDF.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  real ( kind = 8 ) a_sum
  real ( kind = 8 ) m2(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  a_sum = sum ( a(1:n) )

  do i = 1, n
    do j = 1, n
      if ( i == j ) then
        m2(i,j) = a(i) * ( a(i) + 1.0D+00 ) / ( a_sum * ( a_sum + 1.0D+00 ) )
      else
        m2(i,j) = a(i) * a(j) / ( a_sum * ( a_sum + 1.0D+00 ) )
      end if
    end do
  end do

  return
end
