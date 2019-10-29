! This inlined code is used in the Fortran file "String_SubModule.f90",
! in the procedures:
! * Presence_Integer_0D
! * Presence_Real4_0D
! * Presence_Real8_0D
  integer   ::  i                                                                                               ! Index of elements in the list of variables
  Is_Present    =       .False.                                                                                 ! Initialization of the variable presence indicator to false
  do i = 1,size(List_Var)                                                                                       ! Loop on all elements
    if ( Var /= List_Var(i) ) cycle                                                                             ! If current variable from the list is different from the searched variable, then going to the next element
    Is_Present  =       .True.                                                                                  ! Setting presence indicator to true
    return                                                                                                      ! Exiting the procedure if no counting of occurence is required
  end do                                                                                                        ! End do loop on strings
