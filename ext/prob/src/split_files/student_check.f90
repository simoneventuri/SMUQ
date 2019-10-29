function student_check ( a, b, c )

!*****************************************************************************80
!
!! STUDENT_CHECK checks the parameter of the central Student T CDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    02 November 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, shape parameters of the PDF,
!    used to transform the argument X to a shifted and scaled
!    value Y = ( X - A ) / B.  It is required that B be nonzero.
!    For the standard distribution, A = 0 and B = 1.
!
!    Input, real ( kind = 8 ) C, is usually called the number of
!    degrees of freedom of the distribution.  C is typically an
!    integer, but that is not essential.  It is required that
!    C be strictly positive.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  logical student_check

  if ( b == 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'STUDENT_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B must be nonzero.'
    student_check = .false.
    return
  end if

  if ( c <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'STUDENT_CHECK - Fatal error!'
    write ( *, '(a)' ) '  C must be greater than 0.'
    student_check = .false.
    return
  end if

  student_check = .true.

  return
end
