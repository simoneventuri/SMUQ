function lerch ( a, b, c )

!*****************************************************************************80
!
!! LERCH estimates the Lerch transcendent function.
!
!  Discussion:
!
!    The Lerch transcendent function is defined as:
!
!      LERCH ( A, B, C ) = Sum ( 0 <= K < Infinity ) A^K / ( C + K )^B
!
!    excluding any term with ( C + K ) = 0.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Eric Weisstein, editor,
!    CRC Concise Encylopedia of Mathematics,
!    CRC Press, 1998.
!
!  Thanks:
!
!    Oscar van Vlijmen
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the function.
!
!    Output, real ( kind = 8 ) LERCH, an approximation to the Lerch
!    transcendent function.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a_k
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  integer ( kind = 4 ) k
  real ( kind = 8 ) lerch
  real ( kind = 8 ) sum2
  real ( kind = 8 ) sum2_old

  sum2 = 0.0D+00
  k = 0
  a_k = 1.0D+00

  do

    sum2_old = sum2

    if ( c + real ( k, kind = 8 ) == 0.0D+00 ) then
      k = k + 1
      a_k = a_k * a
      cycle
    end if

    sum2 = sum2 + a_k / ( c + real ( k, kind = 8 ) ) ** b

    if ( sum2 <= sum2_old ) then
      exit
    end if

    k = k + 1
    a_k = a_k * a

  end do

  lerch = sum2

  return
end
