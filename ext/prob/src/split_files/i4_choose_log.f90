function i4_choose_log ( n, k )

!*****************************************************************************80
!
!! I4_CHOOSE_LOG computes the logarithm of the Binomial coefficient.
!
!  Discussion:
!
!    The formula is:
!
!      LOG ( C(N,K) ) = LOG ( N! / ( K! * (N-K)! ) ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, K, are the values of N and K.
!
!    Output, real ( kind = 8 ) I4_CHOOSE_LOG, the logarithm of C(N,K).
!
  implicit none

  real ( kind = 8 ) i4_choose_log
  integer ( kind = 4 ) k
  integer ( kind = 4 ) n

  i4_choose_log = &
      lgamma ( real ( n + 1, kind = 8 ) ) &
    - lgamma ( real ( k + 1, kind = 8 ) ) &
    - lgamma ( real ( n - k + 1, kind = 8 ) )

  return
end
