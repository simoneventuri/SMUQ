subroutine multivariate_normal_sample ( n, mean, covar_factor, seed, x )

!*****************************************************************************80
!
!! MULTIVARIATE_NORMAL_SAMPLE samples the Multivariate Normal PDF.
!
!  Discussion:
!
!    PDF ( Mean(1:N), S(1:N,1:N); X(1:N) ) =
!      1 / ( 2 * pi ) ^ ( N / 2 ) * 1 / det ( S )
!      * exp ( - ( X - Mean )' * inverse ( S ) * ( X - Mean ) / 2 )
!
!    Here,
!
!      X is the argument vector of length N,
!      Mean is the mean vector of length N,
!      S is an N by N positive definite symmetric covariance matrix.
!
!    The properties of S guarantee that it has a lower triangular
!    matrix L, the Cholesky factor, such that S = L * L'.  It is the
!    matrix L, rather than S, that is required by this routine.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Jerry Banks, editor,
!    Handbook of Simulation,
!    Engineering and Management Press Books, 1998, page 167.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the spatial dimension.
!
!    Input, real ( kind = 8 ) MEAN(N), the mean vector.
!
!    Input, real ( kind = 8 ) COVAR_FACTOR(N,N), the lower triangular Cholesky
!    factor L of the covariance matrix S.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random
!    number generator.
!
!    Output, real ( kind = 8 ) X(N), a sample point of the distribution.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) covar_factor(n,n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) mean(n)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) z

  do i = 1, n

    call normal_01_sample ( seed, z )

    x(i) = mean(i)

    do j = 1, i
      x(i) = x(i) + covar_factor(i,j) * z
    end do

  end do

  return
end
