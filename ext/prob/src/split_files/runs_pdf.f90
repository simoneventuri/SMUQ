subroutine runs_pdf ( m, n, r, pdf )

!*****************************************************************************80
!
!! RUNS_PDF evaluates the Runs PDF.
!
!  Discussion:
!
!    Suppose we have M symbols of one type and N of another, and we consider
!    the various possible permutations of these symbols.
!
!    Let "R" be the number of runs in a given permutation.  By a "run", we
!    mean a maximal sequence of identical symbols.  Thus, for instance,
!    the permutation
!
!      ABBBAAAAAAAA
!
!    has three runs.
!
!    The probability that a permutation of M+N symbols, with M of one kind
!    and N of another, will have exactly R runs is:
!
!      PDF(M,N)(R) = 2 * C(M-1,R/2-1) * C(N-1,R/2-1)
!                    / C(M+N,N) for R even;
!
!                  = ( C(M-1,(R-1)/2) * C(N-1,(R-3)/2 )
!                    + C(M-1,(R-3)/2) * C(N-1,(R-1)/2 )
!                    ) / C(M+N,N) for R odd.
!
!    The minimum number of runs is:
!
!      1 if M or N is 0,
!      2 otherwise.
!
!    The maximum number of runs is:
!
!      M + N,                if M = N
!      2 * min ( M, N ) + 1  otherwise
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 March 2016
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Kalimutha Krishnamoorthy,
!    Handbook of Statistical Distributions with Applications,
!    Chapman and Hall, 2006,
!    ISBN: 1-58488-635-8,
!    LC: QA273.6.K75.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the parameters of the PDF.
!
!    Input, integer ( kind = 4 ) R, the number of runs.
!
!    Output, real ( kind = 8 ) PDF, the value of the PDF.
!
  implicit none

  integer ( kind = 4 ) i4_choose
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) pdf
  integer ( kind = 4 ) r

  if ( m < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'RUN_PDF - Fatal error!'
    write ( *, '(a)' ) '  M must be at least 0.'
    write ( *, '(a,i8)' ) '  The input value of M = ', m
    stop 1
  end if

  if ( n < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'RUN_PDF - Fatal error!'
    write ( *, '(a)' ) '  N must be at least 0.'
    write ( *, '(a,i8)' ) '  The input value of N = ', n
    stop 1
  end if

  if ( n + m <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'RUN_PDF - Fatal error!'
    write ( *, '(a)' ) '  M+N must be at least 1.'
    write ( *, '(a,i8)' ) '  The input value of M+N = ', m + n
    stop 1
  end if
!
!  If all the symbols are of one type, there is always 1 run.
!
  if ( m == 0 .or. n == 0 ) then
    if ( r == 1 ) then
      pdf = 1.0D+00
    else
      pdf = 0.0D+00
    end if
    return
  end if
!
!  Take care of extreme values of R.
!
  if ( r < 2 .or. m + n < r ) then
    pdf = 0.0D+00
    return
  end if
!
!  The normal cases.
!
  if ( mod ( r, 2 ) == 0 ) then

    pdf = real ( 2 * i4_choose ( m - 1, ( r / 2 ) - 1 ) &
                   * i4_choose ( n - 1, ( r / 2 ) - 1 ), kind = 8 ) &
        / real (     i4_choose ( m + n, n ), kind = 8 )

  else

    pdf = real (   i4_choose ( m - 1, ( r - 1 ) / 2 ) &
                 * i4_choose ( n - 1, ( r - 3 ) / 2 ) &
                 + i4_choose ( m - 1, ( r - 3 ) / 2 ) &
                 * i4_choose ( n - 1, ( r - 1 ) / 2 ), kind = 8 ) &
        / real (   i4_choose ( m + n, n ), kind = 8 )

  end if

  return
end
