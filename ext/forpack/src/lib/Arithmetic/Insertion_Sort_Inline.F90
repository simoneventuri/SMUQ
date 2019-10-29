! ======================================================================
! This inline code is used in the procedures:
! * Insertion_Sort_INT8
! * Insertion_Sort_REAL64
! ======================================================================
  integer                                                               ::  i, j, n
  integer       ,dimension( size(a) )                                   ::  LocalIndex
  n             =       size(a)
  LocalIndex    =       [(i,i=1,n)]
  do i = 2,n
     j          =       i - 1
     temp       =       a(i)
     do
       if ( j < 1 ) exit
       if ( a(j) <= temp ) exit
       a(j+1)           =       a(j)
       LocalIndex(j+1)  =       LocalIndex(j)
       j                =       j - 1
     end do
     a(j+1)             =       temp
     LocalIndex(j+1)    =       i
  end do
  if (present(Sorted_Index)) Sorted_Index = LocalIndex
  if ( present(Reverse) ) then
  if ( Reverse ) then
    a   =   a(n:1:-1)
    if (present(Sorted_Index)) Sorted_Index = Sorted_Index(n:1:-1)
  end if
  end if
! ======================================================================