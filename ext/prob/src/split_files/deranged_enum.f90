function deranged_enum ( n )

!*****************************************************************************80
!
!! DERANGED_ENUM returns the number of derangements of N objects.
!
!  Discussion:
!
!    A derangement of N objects is a permutation with no fixed
!    points.  If we symbolize the permutation operation by "P",
!    then for a derangment, P(I) is never equal to I.
!
!      D(0) = 1
!      D(1) = 0
!      D(2) = 1
!      D(N) = (N-1) * ( D(N-1) + D(N-2) )
!
!    or
!
!      D(0) = 1
!      D(1) = 0
!      D(N) = N * D(N-1) + (-1)^N
!
!    D(N) = N! * ( 1 - 1/1! + 1/2! - 1/3! ... 1/N! )
!
!    Based on the inclusion/exclusion law.
!
!    D(N) is the number of ways of placing N non-attacking rooks on
!    an N by N chessboard with one diagonal deleted.
!
!    Limit ( N -> Infinity ) D(N)/N! = 1 / e.
!
!    The number of permutations with exactly K items in the right
!    place is COMB(N,K) * D(N-K).
!
!  First values:
!
!     N         D(N)
!     0           1
!     1           0
!     2           1
!     3           2
!     4           9
!     5          44
!     6         265
!     7        1854
!     8       14833
!     9      133496
!    10     1334961
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 February 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of objects to be permuted.
!
!    Output, integer ( kind = 4 ) DERANGED_ENUM, the number of derangements
!    of N objects.
!
  implicit none

  integer ( kind = 4 ) deranged_enum
  integer ( kind = 4 ) dn
  integer ( kind = 4 ) dnm1
  integer ( kind = 4 ) dnm2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n

  if ( n < 0 ) then

    dn = 0

  else if ( n == 0 ) then

    dn = 1

  else if ( n == 1 ) then

    dn = 0

  else if ( n == 2 ) then

    dn = 1

  else

    dnm1 = 0
    dn = 1

    do i = 3, n
      dnm2 = dnm1
      dnm1 = dn
      dn = ( i - 1 ) * ( dnm1 + dnm2 )
    end do

  end if

  deranged_enum = dn

  return
end
