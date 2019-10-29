! This inlined code is used in the Fortran file "String_SubModule.f90",
! in the procedures:
! * CountPresence_Integer_0D
! * CountPresence_Real4_0D
! * CountPresence_Real8_0D
  integer   ::  i                                                                                               ! Index of elements in the list of variables
  NCounts       =       0                                                                                       ! Initialization of the count
  do i = 1,size(List_Var)                                                                                       ! Loop on all elements in the list of variables
    if ( Var == List_Var(i) ) NCounts = NCounts + 1                                                             ! If current variable from the list is identical to the searched variable, then incrementing of the counts
  end do                                                                                                        ! End do loop on elements
