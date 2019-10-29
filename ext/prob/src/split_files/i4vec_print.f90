subroutine i4vec_print ( n, a, title )

!*****************************************************************************80
!
!! I4VEC_PRINT prints an I4VEC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 November 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, integer ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title to be printed first.
!    TITLE may be blank.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(n)
  integer ( kind = 4 ) big
  integer ( kind = 4 ) i
  character ( len = * ) title

  if ( title /= ' ' ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  big = maxval ( abs ( a(1:n) ) )

  write ( *, '(a)' ) ' '
  if ( big < 1000 ) then
    do i = 1, n
      write ( *, '(i8,1x,i4)' ) i, a(i)
    end do
  else if ( big < 1000000 ) then
    do i = 1, n
      write ( *, '(i8,1x,i7)' ) i, a(i)
    end do
  else
    do i = 1, n
      write ( *, '(i8,i11)' ) i, a(i)
    end do
  end if

  return
end
