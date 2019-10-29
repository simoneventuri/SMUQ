function bradford_check ( a, b, c )

!*****************************************************************************80
!
!! BRADFORD_CHECK checks the parameters of the Bradford PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, C, the parameters of the PDF.
!    A < B,
!    0.0 < C.
!
!    Output, logical BRADFORD_CHECK, is true if the parameters are legal.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  logical bradford_check
  real ( kind = 8 ) c

  if ( b <= a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BRADFORD_CHECK - Fatal error!'
    write ( *, '(a)' ) '  B <= A.'
    bradford_check = .false.
    return
  end if

  if ( c <= 0.0D+00 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'BRADFORD_CHECK - Fatal error!'
    write ( *, '(a)' ) '  C <= 0.'
    bradford_check = .false.
    return
  end if

  bradford_check = .true.

  return
end
