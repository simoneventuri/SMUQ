SubModule(String_Module) String_SubModule

! Include file needed for '_ASSIGN_ALLOCATABLE_CHARACTER_' in RemoveDuplicate, ReplaceCharacter_C1d_From_C1d, ReplaceCharacter_C1d_From_C0d
# include "forpack-include.inc"

! TODO: Remove SetLength_Sub

  use, intrinsic :: ISO_Fortran_Env ,only: Output_Unit

  implicit none

  integer                       ::  LogUnit         =   Output_Unit                                     !< Unit used for logs
  integer       ,parameter      ::  i_ASCII_VertTab =   9
  integer       ,parameter      ::  i_ASCII_Space   =   32
  integer       ,parameter      ::  ierr_NO_ERROR   =   0

  contains

Module Procedure EmptyString_0d
  allocate( Character(0) :: String )
End Procedure

Module Procedure EmptyString_1d
  allocate( Character(0) :: String(0) )
End Procedure

Module Procedure EmptyString_2d
  allocate( Character(0) :: String(0,0) )
End Procedure

! This procedure retuns the length of the input scalar string w/o any trailing space.
! Its doing the same than the instrisinc function 'len_trim'
Module Procedure LenTrim_0D
  Length    =   len_trim(Strings)
End Procedure

! This procedure retuns the length of the input rank-1 array string w/o any trailing space.
! Internally, it is taking the maximum of thye 'len_trim' of all elements.
Module Procedure LenTrim_1D
  integer                                                               ::  i
  Length    =   0
  do i = 1,size(Strings,1)
    Length  =   max( Length, len_trim(Strings(i)) )
  end do
End Procedure

! This procedure retuns the length of the input rank-2 array string w/o any trailing space.
! Internally, it is taking the maximum of thye 'len_trim' of all elements.
Module Procedure LenTrim_2D
  integer                                                               ::  i, j
  Length        =   0
  do j = 1,size(Strings,2)
  do i = 1,size(Strings,1)
    Length      =   max( Length, len_trim(Strings(i,j)) )
  end do
  end do
End Procedure

! This procedure trims a scalar string. Its doing the same than the instrisinc function 'len_trim'

Module Procedure TrimCharacter_0D
  String    =   trim(String)
End Procedure

Module Procedure TrimCharacter_1D
  character(:)  ,dimension(:)           ,allocatable                    ::  TmpString
  integer                                                               ::  i
  allocate( character(LenTrim(String)) :: TmpString(size(String)) )
  do i = 1,size(String)
    TmpString(i)    =   trim( String(i) )
  end do
  call move_alloc( TmpString, String )
End Procedure

Module Procedure TrimCharacter_2D
  character(:)  ,dimension(:,:)         ,allocatable                    ::  TmpString
  integer                                                               ::  i1, i2
  allocate( character(LenTrim(String)) :: TmpString(size(String,1),size(String,2)) )
  do i2 = 1,size(String,2)
  do i1 = 1,size(String,1)
    TmpString(i1,i2)  =   trim( String(i1,i2) )
  end do
  end do
  call move_alloc( TmpString, String )
End Procedure






! This procedure sets the log unit used by all procedures in this module.
! The default log unit is the intrinsic Output_Unit.
! Note that no checking is done to ensure that the input Unit is actually connected to a opened file.
Module Subroutine Set_LogUnit( Unit )
  integer                                               ,intent(in)     ::  Unit
  LogUnit       =   Unit
End Subroutine

Module Procedure Is_Digit
  select case (String)
    case ( '0':'9' );   Indicator = .True.
    case default;       Indicator = .False.
  end select
End Procedure

Module Procedure Is_Letter
  select case (String)
    case ('A':'Z','a':'z'); Indicator = .True.
    case default;           Indicator = .False.
  end select
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
!       PROCEDURES FOR COUNTING THE NUMBER OF TIME A VARIABLE IS PRESENT IN A LIST OF VARIABLES
! **************************************************************************************************************
! **************************************************************************************************************

Module Procedure CountPresence_Character_0D
  integer                                                               ::  iVar                            ! Index of elements in the list of variables
  NCounts       =   0                                                                                       ! Initialization of the count
  do iVar = 1,size(List_Var)                                                                                    ! Loop on all elements in the list of variables
    if ( Equal( Var, List_Var(iVar), Trimed=Trimed, CaseSensitive=CaseSensitive ) ) NCounts = NCounts + 1      ! If current variable from the list is identical to the searched variable, then incrementing of the counts
  end do                                                                                                        ! End do loop on elements
End Procedure

Module Procedure CountPresence_Integer_0D
# include "String_CountPresence_Inline.F90"
End Procedure

Module Procedure CountPresence_Real4_0D
# include "String_CountPresence_Inline.F90"
End Procedure

Module Procedure CountPresence_Real8_0D
# include "String_CountPresence_Inline.F90"
End Procedure

Pure Module Function CountPresence_Character_1D( Var, List_Var, Trimed, CaseSensitive ) result(NCounts)
  character(*)          ,dimension(:)                   ,intent(in)     ::  Var                             !< Array of variables whose presence in the list of variable is to be counted
  character(*)          ,dimension(:)                   ,intent(in)     ::  List_Var                        !< List of variables used for counting
  logical                                     ,optional ,intent(in)     ::  Trimed                          !< Indicator whether
  logical                                     ,optional ,intent(in)     ::  CaseSensitive                  !< Indicator whether the search should be case sensitive
  integer               ,dimension( size(Var) )                         ::  NCounts                         !< Number of occurence of each element of the array of variable in the list of variables
  integer                                                               ::  i                               ! Index of element in the array of variable
  forall(i=1:size(Var)) NCounts(i) = CountPresence( Var(i), List_Var, Trimed, CaseSensitive )                 ! Counting the occurence of current element in the list of variable
End Function

Pure Module Function CountPresence_Integer_1D( Var, List_Var ) result(NCounts)
  integer               ,dimension(:)                   ,intent(in)     ::  Var                             !< Array of variables whose presence in the list of variable is to be counted
  integer               ,dimension(:)                   ,intent(in)     ::  List_Var                        !< List of variables used for counting
  integer               ,dimension( size(Var) )                         ::  NCounts                         !< Number of occurence of each element of the array of variable in the list of variables
  integer                                                               ::  i                               ! Index of element in the array of variable
  forall(i=1:size(Var)) NCounts(i) = CountPresence( Var(i), List_Var )                                         ! Counting the occurence of current element in the list of variable
End Function

Pure Module Function CountPresence_Real4_1D( Var, List_Var ) result(NCounts)
  real(4)               ,dimension(:)                   ,intent(in)     ::  Var                             !< Array of variables whose presence in the list of variable is to be counted
  real(4)               ,dimension(:)                   ,intent(in)     ::  List_Var                        !< List of variables used for counting
  integer               ,dimension( size(Var) )                         ::  NCounts                         !< Number of occurence of each element of the array of variable in the list of variables
  integer                                                               ::  i                               ! Index of element in the array of variable
  forall(i=1:size(Var)) NCounts(i) = CountPresence( Var(i), List_Var )                                         ! Counting the occurence of current element in the list of variable
End Function

Pure Module Function CountPresence_Real8_1D( Var, List_Var ) result(NCounts)
  real(8)               ,dimension(:)                   ,intent(in)     ::  Var                             !< Array of variables whose presence in the list of variable is to be counted
  real(8)               ,dimension(:)                   ,intent(in)     ::  List_Var                        !< List of variables used for counting
  integer               ,dimension( size(Var) )                         ::  NCounts                         !< Number of occurence of each element of the array of variable in the list of variables
  integer                                                               ::  i                               ! Index of element in the array of variable
  forall(i=1:size(Var)) NCounts(i) = CountPresence( Var(i), List_Var )                                         ! Counting the occurence of current element in the list of variable
End Function


! **************************************************************************************************************
!       PROCEDURES FOR GETTING THE POSITION OF A STRING IN A LIST OF STRINGS
! **************************************************************************************************************
Module Procedure GetPosition_0d
  integer                                                               ::  i                                   ! Index of string of the input string in the list of strings
  integer                                                               ::  iStart                              ! Starting index used to found the string position
  integer                                                               ::  iStop                               ! Stopping index used to found the string position
  integer                                                               ::  iStep                               ! Step in index incrementation used to found the string position
  logical                                                               ::  CaseSensitive_
  character(:)  ,allocatable                                            ::  TargetElement
  character(:)  ,allocatable                                            ::  CurrentElement
  CaseSensitive_ = .True.; if ( present(CaseSensitive) ) CaseSensitive_ = CaseSensitive
  iLoc          =   0                                                                                           ! Initialization of the position index
  iStart        =   1                                                                                           ! Setting the starting index to unity
  iStop         =   size(Array)                                                                                 ! Setting the stopping index to the dimension of the list of strings
  iStep         =   1                                                                                           ! Setting index step to one
  if ( present(Back) ) then                                                                                     ! If the "Back" optional argument is present
    if (Back) then                                                                                              ! If the "Back" argument is true
      iStart    =    size(Array)                                                                                ! Setting the starting index to the dimension of the list of strings
      iStop     =    1                                                                                          ! Setting the stopping index to unity
      iStep     =   -1                                                                                          ! Setting index step to -1 to go backward
    end if                                                                                                      ! End if case on the argument value
  end if                                                                                                        ! End if case on optional argument presence
  TargetElement =   trim(Element)
  if ( .Not. CaseSensitive_ ) TargetElement = UpperCase(TargetElement)
  do i = iStart,iStop,iStep                                                                                     ! Loop on all strings
    CurrentElement  =   trim(Array(i))
    if ( .Not. CaseSensitive_ ) CurrentElement = UpperCase(CurrentElement)
    if ( TargetElement /= CurrentElement ) cycle                                                                ! If current string from the list is different from the searched string, then going to the next element
    iLoc    =   i                                                                                               ! Setting the index if the string
    return                                                                                                      ! Exiting the procedure if no counting of occurence is required
  end do                                                                                                        ! End do loop on strings
End Procedure

Pure Module Function GetPosition_1D( Elements, Array, Back, CaseSensitive ) result(iLoc)
  character(*)                                          ,intent(in)     ::  Elements(:)                         !< String to be checked for presence
  character(*)                                          ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
  logical                                     ,optional ,intent(in)     ::  Back                                !< Indicator than the position index is counted from the end of the list of strings
  logical                                     ,optional ,intent(in)     ::  CaseSensitive                       !< Indicator whether the search should be case sensitive
  integer   ,dimension(size(Elements))                                  ::  iLoc                                !< Index of positions of input strings in the list of strings
  integer                                                               ::  i                                   ! Index of string of the input string in the list of strings
  do i = 1,size(Elements)                                                                                       ! Loop on all strings to be cheched for presence
     iLoc(i)  =   GetPosition( Elements(i), Array, Back, CaseSensitive )                                        ! Finding the position of the current string in the list of strings
  end do                                                                                                        ! End loop on strings to be cheched for presence
End Function

Pure Module Subroutine GetPositions_0d( Element, Array, Positions, CaseSensitive )
  use Utilities_Library  ,only:  AddElementToArray
  character(*)                                          ,intent(in)     ::  Element                             !< String to be checked for presence
  character(*)                                          ,intent(in)     ::  Array(:)                            !< List of string used for presence checking
  integer ,allocatable                                  ,intent(out)    ::  Positions(:)                        !< Index of all positions of the input element in the array
  logical                                     ,optional ,intent(in)     ::  CaseSensitive                       !< Indicator whether the search should be case sensitive
  logical                                                               ::  CaseSensitive_
  integer                                                               ::  i                                   ! Index of string of the input string in the list of strings
  character(:)  ,allocatable                                            ::  TargetElement
  character(:)  ,allocatable                                            ::  CurrentElement
  CaseSensitive_ = .True.; if ( present(CaseSensitive) ) CaseSensitive_ = CaseSensitive
  TargetElement =   trim(Element)
  if ( .Not. CaseSensitive_ ) TargetElement = UpperCase(TargetElement)
  do i = 1,size(Array)
    CurrentElement  =   trim(Array(i))
    if ( .Not. CaseSensitive_ ) CurrentElement = UpperCase(CurrentElement)
    if ( TargetElement == CurrentElement ) call AddElementToArray( i, Positions )                               ! If the two elements match, then adding the position of current element to the output list of positions
  end do                                                                                                        ! End do loop on strings
  if ( .Not. allocated(Positions) ) allocate(Positions(0))
End Subroutine



! **************************************************************************************************************
! **************************************************************************************************************
!       PROCEDURES FOR REORDERING AN ARRAY OF STRING ACCORDING TO A REFERENCE ARRAY OF STRING
! **************************************************************************************************************
! **************************************************************************************************************

!! This procedure orders string array elements according to a reference string array. \n
!! In the following, the string to be ordered is denoted "object string" whereas the string from which the
!! ordering is performed is called the "reference string". \n
!! It is assumed that lower dimension of both the object and reference strings start at 1. \n
!! If the two strings have different dimension, then the output optional error indicator is set to one and
!! the procedure is exited whitout any reordering. \n
!! An insertion sorting method is used to sort the array elements.
Module Procedure Reorder_Character
  logical                                                               ::  i_Debug_Loc                     ! Local debugging indicator
  logical                                                               ::  i_Value_Reordering              ! Indicator of value-based reordering
  logical                                                               ::  i_Index_Reordering              ! Indicator of index-based reordering
  integer                                                               ::  iNew, iOld, jOld                ! New/Old element index
  integer                       ,dimension( size(Array) )               ::  iOld_to_iNew                    ! Index mapping from old to new index
  integer                       ,dimension( size(Array) )               ::  Mapping_Loc                     ! Local index mapping from old to new order
  character(:)  ,dimension(:)   ,allocatable                            ::  Array_Loc                       ! Local copy of the Input-array
  character(:)  ,dimension(:)   ,allocatable                            ::  Array_Ref                       ! Local reference value array from which the ordering is performed (called Reference-array)
  character(*)                                              ,parameter  ::  ProcName='Reorder_Character'    ! Procedure name

  i_Debug_Loc = .False.; if ( present(i_Debug) ) i_Debug_Loc = i_Debug                                                                 ! Setting local debugging indicator
  if (i_Debug_Loc) write(LogUnit,"(2x,'[Reorder_Character]: Entering')")                                        ! Debugging

! ==============================================================================================================
!    TREATING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  i_Value_Reordering      =   .False.                                                                       ! Initialization of the value-based reordering indicator to false
  i_Index_Reordering      =   .False.                                                                       ! Initialization of the index-based reordering indicator to false
  if ( present(ValRef) ) i_Value_Reordering = .True.                                                            ! If the reference-value array is present in the list of arguments, then setting the value-based reordering indicator to true
  if ( present(IdxRef) ) i_Index_Reordering = .True.                                                            ! If the reference-index array is present in the list of arguments, then setting the index-based reordering indicator to true
  if (i_Debug_Loc) write(LogUnit,"(2x,'[Reorder_Character]: i_Value_Reordering = ',l3)") i_Value_Reordering
  if (i_Debug_Loc) write(LogUnit,"(2x,'[Reorder_Character]: i_Index_Reordering = ',l3)") i_Index_Reordering

! ==============================================================================================================
!    CHECKING DATA CONSISTENCY
! ==============================================================================================================
  if ( size(Array) == 0 ) then
    if (i_Debug_Loc) write(LogUnit,"(2x,'[Reorder_Character]: Input argument is a zero-dimension array => Exiting')")
    return
  end if

  if ( i_Value_Reordering .and. i_Index_Reordering ) &                                                          ! If both index-based and value-based reordering are considered, then ...
  call Error_Message( ErrMsg="Both the index-based and value-based reordering options cannot be specified together", ProcName=ProcName ) ! ... printing an error message and stopping the code

  if ( i_Index_Reordering ) then                                                                                ! If index-based reordering is considered, then checking that the array and index variable has the same dimension
    if ( size(Array) /= size(IdxRef) ) &                                                                        ! If size of the reference index variable and the array variables which need to be reordered does not match, then ...
    call Error_Message( ErrMsg="The size of the reference index variable and the array variables which need to be reordered does not match", ProcName=ProcName ) ! ... printing an error message and stopping the code
  end if                                                                                                        ! End if case on index-based reordering


! ==============================================================================================================
!    IF VALUE-BASED ORDERING, SETTING THE ARRAY OF ORDERED Input-array INDEX
! ==============================================================================================================
  if ( i_Value_Reordering ) then                                                                                ! If value-based reordering
    if (i_Debug_Loc) write(LogUnit,"(2x,'[Reorder_Character]: Value-based ordering')")                          ! Debugging
    allocate( Array_Ref, source=ValRef )                                                                        ! Allocating the local Reference-array
    do iOld = 1,size(Array)                                                                                     ! Loop on Input-array's elements
      do iNew = 1,size(Array_Ref)                                                                               ! Loop on Reference-array's elements
        if ( Array(iOld) == Array_Ref(iNew) ) iOld_to_iNew(iOld) = iNew                                         ! Setting index correspondance from Input-array to Reference-array
      end do                                                                                                    ! End loop on Reference-array's elements
    end do                                                                                                      ! End loop on Input-array's elements
    Mapping_Loc =   1                                                                                       ! Initialization of ordered Input-array index
    do iOld = 1,size(Array)                                                                                     ! Loop on Input-array's elements
      do jOld = 1,size(Array)                                                                                   ! Loop on Input-array's elements
        if ( (iOld/=jOld) .and. (iOld_to_iNew(iOld)>iOld_to_iNew(jOld)) ) Mapping_Loc(iOld) = Mapping_Loc(iOld) + 1   ! If the two Input-array elements are different and if the first index if greater than the second, then Setting the index of the ordered Input-array
      end do                                                                                                      ! End loop on Input-array's elements
    end do                                                                                                      ! End loop on Input-array's elements
  end if                                                                                                        ! End of value-based reordering

! ==============================================================================================================
!    IF INDEX-BASED ORDERING, COPYING THE ARRAY OF ORDERED Input-array INDEX FROM Input
! ==============================================================================================================
  if ( i_Index_Reordering ) then                                                                                ! If index-based reordering
    if (i_Debug_Loc) write(LogUnit,"(2x,'[Reorder_Character]: Index-based ordering')")                          ! Debugging
    Mapping_Loc =   IdxRef                                                                                  ! Copying the array of ordered Input-array index from Input
  end if                                                                                                        ! End of index-based reordering

! ==============================================================================================================
!    SETTING THE ORDERED VALUES INTO THE OUTPUT ARRAY
! ==============================================================================================================
  allocate( Array_Loc, source=Array )                                                                           ! Creating a temporary copy of the Input-array
  do iOld = 1,size(Array)                                                                                       ! Loop on Input-array's elements
    Array( Mapping_Loc(iOld) )  =   Array_Loc(iOld)                                                         ! Setting the ordered Input-array value
  end do                                                                                                        ! End loop on Input-array's elements

! ==============================================================================================================
!    SETTING OPTIONAL OUTPUT ARGUMENTS
! ==============================================================================================================
  if ( present(Mapping) ) Mapping = Mapping_Loc                                                                 ! Storing the array of ordered Input-array index in the output variable

  if (i_Debug_Loc) then
    do iOld = 1,size(Array)
      write(LogUnit,"(2x,'[Reorder_Character]: iOld=',i3,3x,'New = Mapping_Loc(iOld)=',i3,3x,'Array(iOld)=',a,3x,'Array(iNew)=',a)")    &
      iOld, Mapping_Loc(iOld), Array_Loc(iOld), Array(iOld)
    end do
    write(LogUnit,"(2x,'[Reorder_Character]: Exiting')")
  end if

End Procedure

Module Procedure Reorder_Real_8
  logical                                                               ::  i_Value_Reordering                        ! Indicator of value-based reordering
  logical                                                               ::  i_Index_Reordering                        ! Indicator of index-based reordering
  integer                                                               ::  iInp, jInp                      ! Index element in the Inp-array
  real(8)                       ,dimension(size(Inp))                   ::  Array_Loc                          ! Local copy of the Inp-array
  integer                       ,dimension(size(Inp))                   ::  Mapping_Loc                     ! Array of ordered Inp-array index

  if ( size(Inp) == 0 ) then
    write(LogUnit,"(2x,'[Reorder_Real_8]: Input argument is a zero-dimension array')")
    write(LogUnit,"(2x,'[Reorder_Real_8]: Exiting')")
    return
  end if


! ==============================================================================================================
!    TREATING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  i_Value_Reordering      =   .False.
  i_Index_Reordering      =   .False.
  if    ( present(ValRef) )     i_Value_Reordering        =   .True.                                                  ! If the reference value array is provided in input, then setting the corresponding indicator
  if    ( present(IdxRef) )     i_Index_Reordering        =   .True.                                                  ! If the reference index array is provided in input, then setting the corresponding indicator
  if    (i_Value_Reordering.and.i_Index_Reordering) then
    write(*,"(2x,'[Reorder_Real_8]: ERROR: Both value-based and index-based reordering is forbidden')")
    stop
  end if

! ==============================================================================================================
!    IF VALUE-BASED ORDERING, SETTING THE ARRAY OF ORDERED INP-ARRAY INDEX
! ==============================================================================================================
  if    ( i_Value_Reordering )    then                                                                                    ! If value-based reordering
    write(*,"(2x,'[Reorder_Real_8]: ERROR: Value-based ordering not implemented for real numbers')")
    stop
  end if                                                                                                        ! End of value-based reordering

! ==============================================================================================================
!    IF INDEX-BASED ORDERING, COPYING THE ARRAY OF ORDERED INP-ARRAY INDEX FROM INPUT
! ==============================================================================================================
  if    ( i_Index_Reordering )    then                                                                                    ! If index-based reordering
    if  (size(Inp) /= size(IdxRef))     then
      write(*,"(2x,'[Reorder_Real_8]: ERROR: size of Inp and IdxRef are different')")
      stop
    end if
    Mapping_Loc =   IdxRef                                                                                  ! Copying the array of ordered inp-array index from input
  end if                                                                                                        ! End of index-based reordering

! ==============================================================================================================
!    SETTING THE ORDERED VALUES INTO THE OUTPUT ARRAY
! ==============================================================================================================
  Array_Loc        =   Inp                                                                                     ! Creating a temporary copy of the Inp-array
  do iInp = 1,size(Inp)                                                                                         ! Loop on Inp-array's elements
    jInp        =   Mapping_Loc(iInp)                                                                       ! Getting the index of the ordered Inp-array element
    Inp(jInp)   =   Array_Loc(iInp)                                                                            ! Setting the ordered Inp-array value
  end do                                                                                                        ! End loop on Inp-array's elements

! ==============================================================================================================
!    SETTING OPTIONAL OUTPUT ARGUMENTS
! ==============================================================================================================
  if    (present(Mapping))          Mapping     =   Mapping_Loc                                                     ! Storing the array of ordered inp-array index in the output variable

End Procedure

Module Procedure Reorder_Real_4
  logical                                                               ::  i_Value_Reordering                        ! Indicator of value-based reordering
  logical                                                               ::  i_Index_Reordering                        ! Indicator of index-based reordering
  integer                                                               ::  iInp, jInp                      ! Index element in the Inp-array
  real(4)                       ,dimension(size(Inp))                   ::  Array_Loc                          ! Local copy of the Inp-array
  integer                       ,dimension(size(Inp))                   ::  Mapping_Loc                     ! Array of ordered Inp-array index


  if ( size(Inp) == 0 ) then
    write(LogUnit,"(2x,'[Reorder_Real_4]: Input argument is a zero-dimension array')")
    write(LogUnit,"(2x,'[Reorder_Real_4]: Exiting')")
    return
  end if


! ==============================================================================================================
!    TREATING OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  i_Value_Reordering      =   .False.
  i_Index_Reordering      =   .False.
  if    ( present(ValRef) )     i_Value_Reordering        =   .True.                                                  ! If the reference value array is provided in input, then setting the corresponding indicator
  if    ( present(IdxRef) )     i_Index_Reordering        =   .True.                                                  ! If the reference index array is provided in input, then setting the corresponding indicator
  if    (i_Value_Reordering.and.i_Index_Reordering) then
    write(*,"(2x,'[Reorder_Real_4]: ERROR: Both value-based and index-based reordering is forbidden')")
    stop
  end if

! ==============================================================================================================
!    IF VALUE-BASED ORDERING, SETTING THE ARRAY OF ORDERED INP-ARRAY INDEX
! ==============================================================================================================
  if    ( i_Value_Reordering )    then                                                                                    ! If value-based reordering
    write(*,"(2x,'[Reorder_Real_4]: ERROR: Value-based ordering not implemented for real numbers')")
    stop
  end if                                                                                                        ! End of value-based reordering

! ==============================================================================================================
!    IF INDEX-BASED ORDERING, COPYING THE ARRAY OF ORDERED INP-ARRAY INDEX FROM INPUT
! ==============================================================================================================
  if    ( i_Index_Reordering )    then                                                                                    ! If index-based reordering
    if  (size(Inp) /= size(IdxRef))     then
      write(*,"(2x,'[Reorder_Real_4]: ERROR: size of Inp and IdxRef are different')")
      stop
    end if
    Mapping_Loc =   IdxRef                                                                                  ! Copying the array of ordered inp-array index from input
  end if                                                                                                        ! End of index-based reordering

! ==============================================================================================================
!    SETTING THE ORDERED VALUES INTO THE OUTPUT ARRAY
! ==============================================================================================================
  Array_Loc        =   Inp                                                                                     ! Creating a temporary copy of the Inp-array
  do iInp = 1,size(Inp)                                                                                         ! Loop on Inp-array's elements
    jInp        =   Mapping_Loc(iInp)                                                                       ! Getting the index of the ordered Inp-array element
    Inp(jInp)   =   Array_Loc(iInp)                                                                            ! Setting the ordered Inp-array value
  end do                                                                                                        ! End loop on Inp-array's elements

! ==============================================================================================================
!    SETTING OPTIONAL OUTPUT ARGUMENTS
! ==============================================================================================================
  if    (present(Mapping))          Mapping     =   Mapping_Loc                                                     ! Storing the array of ordered inp-array index in the output variable

End Procedure

! Module Subroutine Reorder_Character( Str_Obj, Str_Ref, ierr )
! !   character(*)                        ,dimension(:)           ,intent(inout)  ::  Str_Obj                         !< Object string on which the ordering is performed
!   character(*)                        ,dimension(:)           ,intent(in)     ::  Str_Ref                         !< Reference string from which the ordering is performed
!   integer                                   ,optional ,intent(out)    ::  ierr                            !< Error indicator (on output ierr=0 if no error)
!   character(len(Str_Obj))     ,dimension(size(Str_Obj))               ::  Str_Tem                         ! Temorary string
!   integer                     ,dimension(size(Str_Obj))               ::  Ind_Obj                         ! Index order of components of the object string in the frame of the reference string
!   integer                                                             ::  iobj                            ! Index of components of the object string
!   integer                                                             ::  iNew                            ! Index of components of the reference string
!   integer                                                             ::  I_wrk                           ! Local index
!   character(len(Str_Obj))                                             ::  S_wrk                           ! Local string component
!   if  (size(Str_Obj) /= size(Str_Ref))        then
!     if        (present(ierr)) ierr    =   1
! !     return
!   end if
!   Str_Tem     =   Str_Obj                                                                                 ! Setting the temporary string to the object string values
!   Ind_Obj     =   0                                                                                       ! Initialization of the index order
!   do iobj = 1,size(Str_Obj)                                                                                   ! Loop on all object components
!   do iNew = 1,size(Str_Ref)                                                                                   ! Loop on all reference components
!     if        (Str_Obj(iobj) == Str_Ref(iNew))        Ind_Obj(iobj)   =   iNew                                    ! If the two components are identical then setting the index order
!   end do                                                                                                      ! End loop on reference components
!   end do                                                                                                      ! End loop on object components
!   do iobj = 2,size(Str_Obj)
!     I_wrk     =   Ind_Obj(iobj)
!     S_wrk     =   Str_Tem(iobj)
!     if        (I_wrk >= Ind_Obj(iobj-1))      cycle
!     Ind_Obj(iobj)     =       Ind_Obj(iobj-1)
!     Str_Tem(iobj)     =       Str_Tem(iobj-1)
!     do iNew = iobj-2,1,-1
!       if      (I_wrk >= Ind_Obj(iNew))        exit
!       Ind_Obj(iNew+1) =   Ind_Obj(iNew)
!       Str_Tem(iNew+1) =   Str_Tem(iNew)
!     end do
!     Ind_Obj(iNew+1)   =   I_wrk
!     Str_Tem(iNew+1)   =   S_wrk
!   end do
!   Str_Obj     =   Str_Tem                                                                                 ! Saving the temporary object into the final object
!   if  (present(ierr))         ierr    =   ierr_NO_ERROR
! End Subroutine




!               @TODO: To be removed
                          Module Procedure Get_Numbers_AtLeftOf_Characters_0D
                            integer                                                               ::  i
                            character(len(Input_String))                                          ::  StrNum
                            i             =   0
                            do
                              i           =   i + 1
                              if  (i>len_trim(Input_String))       exit
                              if  (Is_Letter(Input_String(i:i)))   exit
                            end do
                            if ( i == 1 ) then
                              if  (present(default))      then
                                StrNum    =   default
                              else
                                StrNum    =   '1'
                              end if
                            else
                              StrNum      =   Input_String(1:i-1)
                            end if
                            Output_Number       =   Convert_To_Real(StrNum)
                          End Procedure

                          Module Procedure Get_Numbers_AtLeftOf_Characters_1D
                            integer                                                               ::  i                               !
                            allocate( Output_Number(size(Input_String)) )
                            do i = 1,size(Input_String)
                              call Get_Numbers_AtLeftOf_Characters( Input_String(i), Output_Number(i), default )
                            end do
                          End Procedure


                          !! This procedure extracts all characters from a string which are located at the right of the first set of letters.
                          !! If the input string starts with a letter, then the ouput and input string are identical
                          !! if the input string has only numbers, then the ouput is an empty string.
                          ! String_NumbersLetters = "15N2"        =>      String_Letters = "N2"
                          ! String_NumbersLetters = "N2"          =>      String_Letters = "N2"
                          ! String_NumbersLetters = "154"         =>      String_Letters = ""
                          Module Procedure Get_Characters_AtRightOf_Numbers_1d
                            integer                                                               ::  i, k
                            integer                                                               ::  Length                          ! Length of the output string
                            integer                                                               ::  LenLoc                          ! Local length a element
                            integer                                                               ::  NElements                       ! Number of elements in the input string
                            character(:)  ,allocatable                                            ::  String
                            character(:)  ,allocatable                                            ::  String_Letters
                            character(:)  ,allocatable                                            ::  String_Numbers
                            NElements             =   size(Input_String)                                                              ! Getting the number of elements of the input string
                            Length                =   0                                                                               ! Initializing the length of the output string (Required because of iterative computation)
                            do i = 1,NElements                                                                                            ! Loop on all the elements to be processed
                              String              =   Input_String(i)                                                                 ! Copying current string in a new variable (Required in order to process each character one-by-one)
                              LenLoc              =   len_trim(String)                                                                ! Getting the length of current element (without trailing blancs)
                              do k = 1,LenLoc                                                                                             ! Loop on all character of current element in order to find the index of te first character corresponding to a letter
                                if ( Is_Letter(String(k:k)) ) exit                                                                        ! If current character corresponds to a letter, then exiting
                              end do                                                                                                      ! End loop on characters of current element
                              if ( k == 1 ) then                                                                                          ! If the first character corresponds to a letter, then the full string corresponds to the string we are looking for
                                String_Numbers    =   ""                                                                              ! Extracting the string of numbers
                                String_Letters    =   trim(String)                                                                    ! Extracting the string of letters
                              else                                                                                                        ! Splitting the string into the number and the letter part.
                                String_Numbers    =   String(1:k-1)                                                                   ! Extracting the string of numbers
                                String_Letters    =   String(k:LenLoc)                                                                ! Extracting the string of letters
                              end if                                                                                                      ! End if case on the first lettr index
                              Length              =   max( Length, len_trim(String_Letters) )                                         ! Getting the maximum length
                            end do
                            allocate( character(Length) :: Output_String(NElements) )                                                     ! Allocating the output string
                            Output_String(:)      =   ""                                                                              ! Initializing the output string values (Required if somme elements have only numbers, then the output string is an empty string)
                            do i = 1,NElements                                                                                            ! Loop on all the elements to be processed
                              String              =   Input_String(i)                                                                 ! Copying current string in a new variable (Required in order to process each character one-by-one)
                              LenLoc              =   len_trim(String)                                                                ! Getting the length of current element (without trailing blancs)
                              do k = 1,LenLoc                                                                                             ! Loop on all character of current element in order to find the index of te first character corresponding to a letter
                                if ( Is_Letter(String(k:k)) ) exit                                                                        ! If current character corresponds to a letter, then exiting
                              end do                                                                                                      ! End loop on characters of current element
                              if ( k == 1 ) then                                                                                          ! If the first character corresponds to a letter, then the full string corresponds to the string we are looking for
                                String_Numbers    =   ""                                                                              ! Extracting the string of numbers
                                String_Letters    =   trim(String)                                                                    ! Extracting the string of letters
                              else                                                                                                        ! Splitting the string into the number and the letter part.
                                String_Numbers    =   String(1:k-1)                                                                   ! Extracting the string of numbers
                                String_Letters    =   String(k:LenLoc)                                                                ! Extracting the string of letters
                              end if                                                                                                      ! End if case on the first lettr index
                              Output_String(i)    =   trim(String_Letters)                                                            ! Setting the output string
                            end do
                          End Procedure

                          Module Procedure Get_Characters_AtRightOf_Numbers_0d
                            integer                                                               ::  k
                            integer                                                               ::  Length                          ! Length of the output string
                            integer                                                               ::  LenLoc                          ! Local length a element
                            character(:)  ,allocatable                                            ::  String
                            character(:)  ,allocatable                                            ::  String_Letters
                            character(:)  ,allocatable                                            ::  String_Numbers
                            String              =   Input_String                                                                    ! Copying current string in a new variable (Required in order to process each character one-by-one)
                            LenLoc              =   len_trim(String)                                                                ! Getting the length of current element (without trailing blancs)
                            do k = 1,LenLoc                                                                                             ! Loop on all character of current element in order to find the index of te first character corresponding to a letter
                              if ( Is_Letter(String(k:k)) ) exit                                                                        ! If current character corresponds to a letter, then exiting
                            end do                                                                                                      ! End loop on characters of current element
                            if ( k == 1 ) then                                                                                          ! If the first character corresponds to a letter, then the full string corresponds to the string we are looking for
                              String_Numbers    =   ""                                                                              ! Extracting the string of numbers
                              String_Letters    =   trim(String)                                                                    ! Extracting the string of letters
                            else                                                                                                        ! Splitting the string into the number and the letter part.
                              String_Numbers    =   String(1:k-1)                                                                   ! Extracting the string of numbers
                              String_Letters    =   String(k:LenLoc)                                                                ! Extracting the string of letters
                            end if                                                                                                      ! End if case on the first lettr index
                            Length              =   len_trim(String_Letters)                                                        ! Getting the maximum length
                            allocate( character(Length) :: Output_String )                                                     ! Allocating the output string
                            Output_String         =   ""                                                                              ! Initializing the output string values (Required if somme elements have only numbers, then the output string is an empty string)
                            String              =   Input_String                                                                 ! Copying current string in a new variable (Required in order to process each character one-by-one)
                            LenLoc              =   len_trim(String)                                                                ! Getting the length of current element (without trailing blancs)
                            do k = 1,LenLoc                                                                                             ! Loop on all character of current element in order to find the index of te first character corresponding to a letter
                              if ( Is_Letter(String(k:k)) ) exit                                                                        ! If current character corresponds to a letter, then exiting
                            end do                                                                                                      ! End loop on characters of current element
                            if ( k == 1 ) then                                                                                          ! If the first character corresponds to a letter, then the full string corresponds to the string we are looking for
                              String_Numbers    =   ""                                                                              ! Extracting the string of numbers
                              String_Letters    =   trim(String)                                                                    ! Extracting the string of letters
                            else                                                                                                        ! Splitting the string into the number and the letter part.
                              String_Numbers    =   String(1:k-1)                                                                   ! Extracting the string of numbers
                              String_Letters    =   String(k:LenLoc)                                                                ! Extracting the string of letters
                            end if                                                                                                      ! End if case on the first lettr index
                            Output_String       =   trim(String_Letters)                                                            ! Setting the output string
                          End Procedure
!               @TODO: To be removed

!
! Module Subroutine GetStrRHS_1D ( Str, Str_RHS )
!   character(*)  ,dimension(:)                   ,intent(in)     ::  Str                                                     !<
! !   character(*)  ,dimension(:)                   ,intent(out)    ::  Str_RHS                                                 !<
!   character(:)  ,dimension(:)   ,allocatable    ,intent(out)    ::  Str_RHS                                                 !<
!   integer                                                       ::  i
!   do i = 1,size(Str)
!     call GetStrRHS( Str(i), Str_RHS(i) )
!   end do
! End Subroutine


!! This procedure removes all characters at the LHS of a given character. \n
!! The result is stored in a new output string called \c StrOut. \n
!! An intermediary local string \c StrLoc is used because the \c Split procedure has side effects on the its
!! first argument. \n
!! After the call to the \c Split procedure, the \c StrLoc and \c StrOut variables have all characters located
!! to the right and left side of the \c Separator character respectively.
Module Procedure RemoveLeftChar
  character(:)  ,allocatable                                            ::  StrLoc                          ! Local copy of the input character string
  logical                                                               ::  i_Debug_Loc
  i_Debug_Loc = .False.; if ( present(i_Debug) ) i_Debug_Loc = i_Debug
  StrLoc        =   String
  StrOut        =   String
  if ( len_trim(String) == 0 ) return                                                                           ! If empty input string, then exiting the procedure
  call Split( String, StrOut, Separator, StrLoc, i_Debug=i_Debug )
End Procedure




! ! Module Procedure GetNumberOfItems!( String, Separator ) result(Number)
! Function GetNumberOfItems( String, Separator ) result(NItems)
!   character(*)                                          ,intent(in)     ::  String                          !< Input character string to be converted
!   character(*)                                ,optional ,intent(in)     ::  Separator                          !< Separation character string
!   integer                                                               ::  NItems                          !< Integer number corresponding to the number of item in the input string
!
!
!
! !   character(*)                                          ,intent(in)     ::  String                          !< Input character string to be converted
! !   character(*)                                ,optional ,intent(in)     ::  Separator                          !< Separation character string
!
!   integer                                                               ::  i, j, N, NSep, k
!   character(:)  ,allocatable                                            ::  Separator_
!   character(:)  ,allocatable                                            ::  SubString
!
!
!   Separator_  =   " "
!   if ( present(Separator) ) Separator_  = Separator
!   NSep    =   len(Separator_)
!
!   i       =   1
!   N  =   len_trim(String)
!   j       =   0
!   NItems = 0
!
!
!   do
! !     if ( i > N ) exit
!
!     if ( i+NSep-1 > N ) exit
!
!     call Logger%Write( "  -> i = ", i )
!     k = 0
!     do
!       k = k + 1
!       SubString   =   String(i:i+NSep-1)
!       call Logger%Write( "    -> k = ", k, "SubString = ", SubString )
!       if ( SubString /= Separator_ ) exit
!       i = i + NSep
!       call Logger%Write( "    -> i = ", i )
!       if (i > N) return
!     end do
! !     j       =   j + 1
!     NItems  =   NItems + 1
!
! !     do
!       i     =   i + 1
! !       SubString   =   String(i:i+NSep-1)
! !       if (i > N) return
! !       if (String(i:i) == ' ') exit
! !     end do
!
!   end do
! ! End Procedure
! End Function

Module Procedure Swap
  character(:)  ,allocatable                                            ::  Tmp
  Tmp       =   String1
  String1   =   String2
  String2   =   Tmp
End Procedure

!! This procedure converts multiple spaces and tabs to single spaces and removes left-hand spaces. \n
Module Procedure Compact
  character(1)                                                          ::  char1                           ! String single character
  logical                                                               ::  i_space                         ! Spacing indicator
  integer                                                               ::  i, k                            ! Character index
  integer                                                               ::  i_ASCII                         ! Interger ASCII code for a given characters
  integer                                                               ::  LenStr                          ! Length of input string (without trailing blank characters)
  LenStr        =   len_trim(StrInp)                                                                        ! Getting the length of the input string (without trailing blank characters)
  StrOut        =   StrInp
  StrOut(:)     =   ''                                                                                      ! Initialization of the output string
  i_space       =   .False.                                                                                 ! Initialization of the space indicator
  k             =   0                                                                                       ! Initialization of the character index
  do i = 1,LenStr                                                                                               ! Loop on all string characters
    char1       =   StrInp(i:i)                                                                             ! Store current character
    i_ASCII     =   iachar(char1)                                                                           ! Getting the code for the ASCII character of the current character
    select case(i_ASCII)                                                                                        ! Select case according ASCII character code
    case (i_ASCII_VertTab,i_ASCII_Space)                                                                        ! If the character is a vertical tabulation or a space
      if (.not.i_space) then                                                                                    ! If no space have yet been treated
        k           =   k + 1                                                                                   ! Incrementing the output character string index
        StrOut(k:k) =   ' '                                                                             ! Setting an space in the current character
      end if                                                                                                    ! End of if case on spacing indicator
      i_space       =   .True.                                                                                  ! Setting the spacing indicato
    case (33:)                                                                                                  ! If the character is not a control character
      k             =   k + 1                                                                                   ! Incrementing the output character string index
      StrOut(k:k)   =   char1                                                                           ! Setting the current charatcer to the input character
      i_space       =   .False.                                                                                 ! Setting the spacing indicato
    end select                                                                                                  ! End select on ASCII character code
  end do                                                                                                        ! End loop on string characters
  StrOut            =   adjustl(StrOut)                                                                         ! Removing left-hand spaces
End Procedure

!! This procedure converts tabs to spaces. \n
Module Procedure Tab2Space
  integer                                                               ::  i                               ! Character index
  StrOut        =   StrInp                                                                                  ! Initialization of the output string
  forall (i=1:len_trim(StrInp),iachar(StrOut(i:i))==i_ASCII_VertTab)    StrOut(i:i) = ' '                       ! If the current character is a vertical tabulation, then convert tabulation into space character
End Procedure

!! This procedure removes spaces and tabulation. \n
Module Procedure RemoveSpace
  integer                                                               ::  i, k                            ! Character index
  integer                                                               ::  i_ASCII                         ! Interger ASCII code for a given characters
  StrOut        =   StrInp
  StrOut(:)     =   ''                                                                                      ! Initialization of the output string
  k             =   0                                                                                       ! Initialization of the character index
  do i = 1,len_trim(StrInp)                                                                                     ! Loop on all string characters
    i_ASCII     =   iachar(StrInp(i:i))                                                                     ! Getting the code for the ASCII character of the current character
    if  (i_ASCII == i_ASCII_Space)      cycle                                                                   ! If the character is a space, then going to the next character
    k           =   k + 1                                                                                   ! Incrementing the output character string index
    StrOut(k:k) =   StrInp(i:i)                                                                             ! Setting the current charatcer to the input character
  end do                                                                                                        ! End loop on string characters
  StrOut    =   trim(StrOut)
End Procedure


! **************************************************************************************************************
! **************************************************************************************************************
!       PROCEDURES FOR CONVERTING UPPER TO LOWER CASE AND RRESIPROCALLY
! **************************************************************************************************************
! **************************************************************************************************************

!! This procedure converts a string into upper case. \n
!! @COMPILER_BUG: The "elemental" attribute is not used for the "UpperCase" procedure since it prevents to use this procedure
!! with allocatable defered-length vector character string variables.
!! At least, with ifort version 16.0.0 an ICE is obtained.
!! This bug should be reported to the intel forum.
!! A simple workaround is to defined 2 procedures, one for scalar input variable and one for vectors, both being accessible through a generic binding name
!! Note that the ICE is only for ifort v16, everything goes smoothly with v15.
Module Procedure UpperCase_0D
  integer                                       ::  i, ilen, ioffset, iquote, iav, iqc
  ilen          =   len_trim(StrInp)
  ioffset       =   iachar('A') - iachar('a')
  iquote        =   0
  StrOut        =   StrInp
  do i = 1,ilen
    iav=iachar(StrInp(i:i))
    if(iquote==0 .and. (iav==34 .or.iav==39)) then
      iquote    =   1
      iqc       =   iav
      cycle
    end if
    if(iquote==1 .and. iav==iqc) then
      iquote    =   0
    cycle
    end if
    if (iquote==1) cycle
    if(iav >= iachar('a') .and. iav <= iachar('z')) then
      StrOut(i:i)       =   achar(iav+ioffset)
    else
      StrOut(i:i)       =   StrInp(i:i)
    end if
  end do
End Procedure

!! This procedure converts a string into lower case.
Module Procedure LowerCase
  integer                                       ::  i, ilen, ioffset, iquote, iav, iqc
  ilen          =   len_trim(StrInp)
  ioffset       =   iachar('A')-iachar('a')
  iquote        =   0
  StrOut        =   StrInp
  do i = 1,ilen
    iav =   iachar(StrInp(i:i))
    if(iquote==0 .and. (iav==34 .or.iav==39)) then
      iquote    =   1
      iqc       =   iav
      cycle
    end if
    if(iquote==1 .and. iav==iqc) then
      iquote    =   0
      cycle
    end if
    if (iquote==1) cycle
    if(iav >= iachar('A') .and. iav <= iachar('Z')) then
      StrOut(i:i)       =   achar(iav-ioffset)
    else
      StrOut(i:i)       =   StrInp(i:i)
    end if
  end do
End Procedure


Pure Module Function UpperCase_1D(StrInp) result(StrOut)
  character(*)                  ,dimension(:)                   ,intent(in)     ::  StrInp                                                  !<
!   character(len(StrInp))        ,dimension(size(StrInp))                        ::  StrOut                                                  !<
  character(:)  ,allocatable    ,dimension(:)                                   ::  StrOut
  integer                                                                       ::  i
# ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
  character(:)  ,allocatable    ,dimension(:)                                   ::  Tmp
  allocate( Tmp, source = StrInp )
  call move_alloc( Tmp, StrOut )
# else
  allocate( StrOut, source = StrInp )
# endif
  do i = 1,size(StrInp)
    StrOut(i)   =   UpperCase(StrInp(i))
  end do
End Function



! **************************************************************************************************************
!       PROCEDURES FOR ACCFESSING THE LENGTH OF A STRING
! **************************************************************************************************************
! This procedure gets the length of a scalar string variable.
Module Procedure GetLength_0d
  logical                                                               ::  Trim_
  Trim_         = .False.; if ( present(Trim)    ) Trim_ = Trim
  if (Trim_) then;  Length  =   len_trim(String)
  else;             Length  =   len(String);      end if
End Procedure

! This procedure gets the length of an array of strings.
Module Procedure GetLength_1d
  use Utilities_Library   ,only:  IsIncluded
  integer                                                               ::  i
  logical                                                               ::  Trim_
  logical                                                               ::  IncludeSubSet
  logical                                                               ::  ExcludeSubSet
  Trim_         = .False.; if ( present(Trim)    ) Trim_ = Trim
  IncludeSubSet = .False.; if ( present(Include) ) IncludeSubSet = .True.
  ExcludeSubSet = .False.; if ( present(Exclude) ) ExcludeSubSet = .True.
  Length    =   0
  do i = 1,size(Strings)
    if (IncludeSubSet) then; if ( .Not. IsIncluded(i,Include) ) cycle; end if   ! If a subset of index to include has been defined an the current index is not part of them, then cycle
    if (ExcludeSubSet) then; if (       IsIncluded(i,Exclude) ) cycle; end if   ! If a subset of index to exclude has been defined an the current index is part of them, then cycle
    if (Trim_) then;  Length = max( Length, LenTrim(Strings) )
    else;             Length = max( Length, len(Strings) );      end if
  end do
End Procedure




    ! **************************************************************************************************************
    ! **************************************************************************************************************
    !       PROCEDURES FOR CHANGING THE LENGTH OF CHARACTER STRINGS
    ! **************************************************************************************************************
    ! **************************************************************************************************************
! This procedure returns the input string with the length  a character string containing the input string
! Padding is added if pad character is present and if the length of the output string is greater than the one of the input string
Module Procedure SetLength_0d
  integer                                                               ::  i
  character(Length)                                                     ::  S
  S   =   S1
  allocate( S2 , source = S )
  if ( present(Pad) ) then
    do i = len(S1)+1,Length
      S2(i:i) = Pad
    end do
  end if
End Procedure

!@COMPILER_BUG: gcc-7.2.0: There is an ICE if this procedure is a Function. Changing it to a Subroutine whors fine.
!               The compiler has issues on the line:    allocate( S2 , source = S )
!               I've tried:                             allocate( S2(size(S)) , source = S )
!               and                                     allocate( character(Length) :: S2(size(S)) ); S2 = S
!               but each time an ICE appears at the 'allocate' statement.
!               A workaround if to change the function to a subroutine.
!               This workaround is set depending on the WORKAROUND_GFORTRAN_SOURCE_ALLOCATION macro.
!               To comform with the standard, the subroutine an on on input strinc with the 'intent(inout)' attribute.
Module Procedure SetLength_1d
  integer                                                               ::  i, k, L
  character(Length)                                                     ::  S(size(S1))
  S   =   S1
# ifdef WORKAROUND_GCC_ALLOCATABLE_OUTPUT_CHARACTER_ARRAY_IN_FUNCTION
  Block
    character(:) ,allocatable   :: WASTR(:)
    allocate( WASTR, source = S )
    call move_alloc( WASTR, S2 )
  End Block
# else
  allocate( S2 , source = S )     ! @COMPILER_BUG: gcc-8.2.0: ICE
# endif
  if ( present(Pad) ) then
    do i = len(S1)+1,Length
      S2(:)(i:i) = Pad
    end do
  end if
!   character(Length)                                                     ::  S(size(S1))
!   S   =   S1
!   allocate( S2 , source = S )
!   if ( present(Pad) ) then
!     do i = len(S1)+1,Length
!       S2(:)(i:i) = Pad
!     end do
!   end if
! ! ####################################
!   allocate( Character(Length) :: S2 )
!   do i = 1,Length
!     if ( i > size(S1) ) exit
!     S2(:)(i:i)    =   S1(:)(i:i)
!   end do
!   if ( present(Pad) ) then
!     do i = len(S1)+1,Length
!       S2(:)(i:i)  =   Pad
!     end do
!   end if
! ! ####################################
End Procedure

Module Procedure SetSameLength_C0_C0
  integer                                                               ::  L1, L2            ! Length of the 2 input strings
  L1    =   len(S1)
  L2    =   len(S2)
  if ( L1 < L2 ) then
    S1  =   SetLength(S1,L2,Pad=Pad)
  else if ( L1 > L2 ) then
    S2  =   SetLength(S2,L1,Pad=Pad)
  end if
End Procedure

Module Procedure SetSameLength_C1_C1
  integer                                                               ::  L1, L2            ! Length of the 2 input strings
  if ( .Not. allocated(S1) ) allocate( character(0) :: S1(1) )
  if ( .Not. allocated(S2) ) allocate( character(0) :: S2(1) )
  L1    =   len(S1)
  L2    =   len(S2)
  if ( L1 < L2 ) then
#   ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
#     define  _RHS_Var    SetLength(S1,L2,Pad=Pad)
      _ASSIGN_ALLOCATABLE_CHARACTER_(S1,_RHS_Var)
#     undef   _RHS_Var
#   else
    S1  =   SetLength(S1,L2,Pad=Pad)
#   endif
  else if ( L1 > L2 ) then
#   ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
#     define  _RHS_Var    SetLength(S2,L1,Pad=Pad)
      _ASSIGN_ALLOCATABLE_CHARACTER_(S2,_RHS_Var)
#     undef   _RHS_Var
#   else
    S2  =   SetLength(S2,L1,Pad=Pad)
#   endif
  end if
End Procedure

Module Procedure SetSameLength_C0_C1
  integer                                                               ::  L1, L2            ! Length of the 2 input strings
  if ( .Not. allocated(S2) ) allocate( character(0) :: S2(1) )
  L1    =   len(S1)
  L2    =   len(S2)
  if ( L1 < L2 ) then
    S1  =   SetLength(S1,L2,Pad=Pad)
  else if ( L1 > L2 ) then
#   ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
#     define  _RHS_Var    SetLength(S2,L1,Pad=Pad)
      _ASSIGN_ALLOCATABLE_CHARACTER_(S2,_RHS_Var)
#     undef   _RHS_Var
#   else
    S2  =   SetLength(S2,L1,Pad=Pad)
#   endif
  end if
End Procedure

Module Procedure SetSameLength_C1_C0
  integer                                                               ::  L1, L2            ! Length of the 2 input strings
  if ( .Not. allocated(S1) ) allocate( character(0) :: S1(1) )
  L1    =   len(S1)
  L2    =   len(S2)
  if ( L1 < L2 ) then
#   ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
#     define  _RHS_Var    SetLength(S1,L2,Pad=Pad)
      _ASSIGN_ALLOCATABLE_CHARACTER_(S1,_RHS_Var)
#     undef   _RHS_Var
#   else
    S1  =   SetLength(S1,L2,Pad=Pad)
#   endif
  else if ( L1 > L2 ) then
    S2  =   SetLength(S2,L1,Pad=Pad)
  end if
End Procedure

! **************************************************************************************************************
!       PROCEDURES FOR REPLACING AN ELEMENT ON AN ARRAY OF STRING BY ONE OR SEVERAL ELEMENTS
! **************************************************************************************************************
! This procedure replaces the element in an array of strings matching a given value by another element.
! This is done by first finding the position of the element to be replaced.
! If there is no element in the array which matches the string to be replace, then nothing is done and the procedure is exited.

Module Procedure ReplaceElement_0d
  integer                                                               ::  iOld, iNew        ! Index of elements in the new and old arrays
  integer                                                               ::  iRep              ! Index of the element to be replaced
  integer                                                               ::  Length            ! Length of the new array after replacement of the element
  integer                                                               ::  NElements         ! Number of elements of the new array
  character(:)  ,allocatable                                            ::  ArrayTmp(:)       ! Temporary array
  iRep      =   GetPosition_0d( OldElement, Array )                                               ! Finding the position of the element to be replaced in the array
  if ( iRep == 0 ) return                                                                     ! Exiting the procedure if the element has not been found
  Length    =   max( GetLength(Array,Exclude=[iRep]), len(NewElement) )                       ! Setting the length of the new array
  NElements =   size(Array)                                                                   ! Setting the size of the new array
  allocate( character(Length) :: ArrayTmp(NElements) )
  do iOld = 1,size(Array)
    iNew    =   iOld
    if ( iOld /= iRep ) then; ArrayTmp(iNew) = Array(iOld)
    else;                     ArrayTmp(iNew) = NewElement; end if
  end do
  call move_alloc( ArrayTmp, Array )
End Procedure

Module Procedure ReplaceElement_1d
  integer                                                               ::  iOld, iNew        ! Index of elements in the new and old arrays
  integer                                                               ::  iRep              ! Index of the element to be replaced
  integer                                                               ::  Length            ! Length of the new array after replacement of the element
  integer                                                               ::  NElements         ! Number of elements of the new array
  character(:)  ,allocatable                                            ::  ArrayTmp(:)       ! Temporary array
  iRep      =   GetPosition_0d( OldElement, Array )                                               ! Finding the position of the element to be replaced in the array
  if ( iRep == 0 ) return                                                                     ! Exiting the procedure if the element has not been found
  Length    =   max( GetLength(Array,Exclude=[iRep]), len(NewElements) )                      ! Getting the new length of the array
  NElements =   size(Array) - 1 + size(NewElements)                                           ! Setting the size of the new array
  allocate( character(Length) :: ArrayTmp(NElements) )
  iNew      =   0
  do iOld = 1,size(Array)
    if ( iOld /= iRep ) then
      iNew  =   iNew + 1
      ArrayTmp(iNew) = Array(iOld)
    else
      ArrayTmp(iNew+1:iNew+size(NewElements)) = NewElements
      iNew  =   iNew + size(NewElements)
    end if
  end do
  call move_alloc( ArrayTmp, Array )
End Procedure

    ! **************************************************************************************************************
    ! **************************************************************************************************************
    !       PROCEDURES FOR ADDING A ELEMENT TO AN VECTOR OF STRINGS
    ! **************************************************************************************************************
    ! **************************************************************************************************************
! ! Pure Module Subroutine ReplaceElement( Array, Old_Element, New_Element )
! !   character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  Array                           !<
! !   character(*)                                          ,intent(in)     ::  Old_Element                     !<
! !   character(*)                                          ,intent(in)     ::  New_Element                     !<
! !   integer                                                               ::  Element_Index, i
! !   integer                                                               ::  Length
! !   character(:)  ,dimension(:)   ,allocatable                            ::  Array_tmp
! !   Element_Index         =   GetPosition_0d( Old_Element, Array )                                                   ! Finding the position of the element to be replaced in the array
! !   if ( Element_Index == 0 ) return                                                                              ! Exiting the procedure if the element has not been found
! !   Length                =   max( len(Array), len(New_Element) )                                                     ! Getting the new length of the array
! !   allocate( character(Length) :: Array_tmp(size(Array)) )
! !   do i = 1,size(Array)
! !     Array_tmp(i)        =   Array(i)
! !     if ( i == Element_Index ) Array_tmp(i) = New_Element
! !   end do
! !   call move_alloc( Array_tmp, Array )
! ! End Subroutine


Module Procedure Add_Line_To_String
  integer                                                               ::  Length
  character(:)  ,dimension(:)   ,allocatable                            ::  TmpVar
  if ( .not. allocated(OutVar) ) allocate( character(0) :: OutVar(0) )
  Length        =   max( len(OutVar), len(InpVar) )
  allocate( character(Length) :: TmpVar(size(OutVar)+1) )
  if ( present(At_Start) ) then
  if ( At_Start ) then
    TmpVar(1)               =   InpVar
    TmpVar(2:size(OutVar)+1)=   OutVar
    call move_alloc( TmpVar, OutVar )
    return
  end if
  end if
  TmpVar(1:size(OutVar))    =   OutVar
  TmpVar(size(OutVar)+1)    =   InpVar
  call move_alloc( TmpVar, OutVar )
End Procedure

!
! Pure Module Subroutine Add_Line_To_String( String, Line, At_End, At_Start, At_Position )
!   character(:)  ,dimension(:)   ,allocatable            ,intent(inout)  ::  String
!   character(*)                                          ,intent(in)     ::  Line
!   logical                                     ,optional ,intent(in)     ::  At_End  ! Default behavior
!   logical                                     ,optional ,intent(in)     ::  At_Start
!   integer                                     ,optional ,intent(in)     ::  At_Position
!   integer                                                               ::  Length
!   character(:)  ,dimension(:)   ,allocatable                            ::  String_tmp
! # ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!   integer                                                               ::  i
! # endif
!   if ( .not. allocated(String) ) allocate( character(0) :: String(0) )
!   Length        =   max( len(String), len(Line) )
!   allocate( character(Length) :: String_tmp(size(String)+1) )
!   if ( present(At_Start) ) then
!   if ( At_Start ) then
!     String_tmp(1)               =   Line
! # ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     do i = 2,size(String)+1
!       String_tmp(i)               =   String(i-1)
!     end do
! # else
!     String_tmp(2:size(String)+1)=   String
! # endif
!     call move_alloc( String_tmp, String )
!     return
!   end if
!   end if
! # ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!   do i = 1,size(String)
!     String_tmp(i)               =   String(i)
!   end do
! # else
!   String_tmp(1:size(String))    =   String
! # endif
!   String_tmp(size(String)+1)    =   Line
!   call move_alloc( String_tmp, String )
! End Subroutine


Module Procedure Add_Lines_To_String
  integer                                                               ::  Length!, i
  character(:)  ,dimension(:)   ,allocatable                            ::  String_tmp
!   write(*,*) "  [Add_Lines_To_String]: size(Lines) = ", size(Lines)
!   do i = 1,size(Lines)
!     write(*,*) "  [Add_Lines_To_String]: i = ", i, " -> ", Lines(i)
!   end do
  if ( .not. allocated(String) ) allocate( character(0) :: String(0) )
  Length      =   max( len(String), len(Lines) )
  allocate( character(Length) :: String_tmp(size(String)+size(Lines)) )
  if ( present(At_Start) ) then
  if ( At_Start ) then
    String_tmp(1:size(Lines))                   =   Lines
    String_tmp(size(Lines)+1:size(String_tmp))  =   String
    call move_alloc( String_tmp, String )
    return
  end if
  end if
  String_tmp(1:size(String))                  =   String
  String_tmp(size(String)+1:size(String_tmp)) =   Lines

!   write(*,*) "  [Add_Lines_To_String]: size(String_tmp) = ", size(String_tmp)
!   do i = 1,size(Lines)
!     write(*,*) "  [Add_Lines_To_String]: i = ", i, " -> ", String_tmp(i)
!   end do

  call move_alloc( String_tmp, String )

!   write(*,*) "  [Add_Lines_To_String]: size(String) = ", size(String)
!   do i = 1,size(Lines)
!     write(*,*) "  [Add_Lines_To_String]: i = ", i, " -> ", String(i)
!   end do

End Procedure

Module Procedure Add_Element_If_Absent_Integer
  use Utilities_Library   ,only:  IsIncluded
  integer       ,dimension(:)   ,allocatable                            ::  List_tmp
  if ( .not.allocated(List) ) then
    allocate( List(1) )
    List(1)       =   Element
  else
    if ( .not.IsIncluded(Element,List) ) then
      allocate( List_tmp, source=[List,Element] )
      call move_alloc( List_tmp, List )
    end if
  end if
End Procedure

Module Procedure Add_Element_If_Absent_C0
  use Utilities_Library   ,only:  IsIncluded
  character(:)  ,dimension(:)   ,allocatable                            ::  List
  if ( .Not. allocated(Array) ) allocate( character(0) :: Array(0) )
  if ( .Not. IsIncluded( UpperCase(Element), UpperCase_1D(Array) ) ) then
    allocate( List, source = [Array,Element] )
    call move_alloc( List, Array )
  end if
End Procedure

Module Procedure Add_Element_If_Absent_C1
  integer                                                               ::  i
  if ( .Not. allocated(Array) ) allocate( character(0) :: Array(0) )
  do i = 1,size(Elements)
    call Add_Element_If_Absent( Array, Elements(i))
  end do
End Procedure



# define _GetNumberOfDigits_    NumberOfDigits = floor( log10( real( abs(Number) ) ) ) + 1

Module Procedure GetNumberOfDigits_INT8
  if ( Number == 0 ) then
    NumberOfDigits  =   1
  else
    _GetNumberOfDigits_
  end if
End Procedure

Module Procedure GetNumberOfDigits_INT16
  if ( Number == 0 ) then
    NumberOfDigits  =   1
  else
    _GetNumberOfDigits_
  end if
End Procedure

Module Procedure GetNumberOfDigits_INT32
  if ( Number == 0 ) then
    NumberOfDigits  =   1
  else
    _GetNumberOfDigits_
  end if
End Procedure

Module Procedure GetNumberOfDigits_INT64
  if ( Number == 0 ) then
    NumberOfDigits  =   1
  else
    _GetNumberOfDigits_
  end if
End Procedure

# undef _GetNumberOfDigits_



Module Procedure RemoveDuplicate
  integer                                                               ::  i, j
  character(:)  ,allocatable                                            ::  Copy(:)
# ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
  _ASSIGN_ALLOCATABLE_CHARACTER_(OutStr,InpStr)
# else
  OutStr  =   InpStr
# endif
  i       =   0
  do
    i     =   i + 1
    if ( i >= size(OutStr,1) ) exit
    j     =   i
    do
      j   =   j + 1
      if ( j > size(OutStr,1) ) exit
      if ( OutStr(i) /= OutStr(j) ) cycle
      if ( size(OutStr,1) == 2 ) then
        allocate( Copy, source = [OutStr(1)] )
        call move_alloc( Copy, OutStr )
      else
#       ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
        Block
          integer :: k, n
          allocate( Character(len(OutStr)) :: Copy(size(OutStr)-1) )
          n = 0
          do k = 1,size(Copy)
            n = n + 1
            if (k == j) n = j + 1
            Copy(k) = OutStr(n)
          end do
        End Block
#       else
        allocate( Copy, source = [OutStr(1:j-1),OutStr(j+1:)] )
#       endif
        call move_alloc( Copy, OutStr )
      end if
      j   =   j - 1
    end do
  end do
End Procedure

! This procedure removes all empty elements from a vector of character strings
Module Procedure Remove_Empty
  integer                                                               ::  i
  integer                                                               ::  NElementsToKeep
  integer       ,dimension(:)   ,allocatable                            ::  IndexElementsToKeep
  character(:)  ,dimension(:)   ,allocatable                            ::  ListElements
  if ( .Not. allocated(Array) ) return
  if ( size(Array) == 0 ) return
  allocate( IndexElementsToKeep(size(Array)) )
  NElementsToKeep   =   0
  do i = lbound(Array,1),ubound(Array,1)
    if ( len_trim(Array(i)) == 0 ) cycle
    NElementsToKeep = NElementsToKeep + 1
    IndexElementsToKeep(NElementsToKeep)  =   i
  end do
  allocate( character(len(Array)) :: ListElements(NElementsToKeep) )
  do i = 1,NElementsToKeep
    ListElements(i)   =   Array( IndexElementsToKeep(i) )
  end do
  call move_alloc( ListElements, Array )
End Procedure


! REMARK:
! The input argument corresponds to a character string corresponding to a real number.
! This procedure will remove the trailling zeros from the fractional part of the real number.
! If no fractional part is found, then the input character string is unchanged
!
! 0. 0.13400666E-11

! 1340066600000000
! e-11

Module Procedure Remove_Trailing_Zeros
  character(*)                                              ,parameter  ::  Separator_Int_Fra="."
  character(*)                                              ,parameter  ::  Separator_Fra_Exp="E"
  integer                                                               ::  iSpe
  character(:)  ,allocatable                                            ::  Integer_Part
  character(:)  ,allocatable                                            ::  Fractional_Part
  character(:)  ,allocatable                                            ::  Exponent_Part
  integer                                                               ::  i
  StrOut        =   UpperCase( StrInp )                                                                     ! The conversion to upper case is required to be sure that the exponent character is "E" and not "e"
  iSpe          =   index( StrOut, Separator_Int_Fra )                                                      ! Getting the index of the separation character between the integer and fractional parts (zero if any)
  if ( iSpe /= 0 ) then                                                                                         ! If a separation character is found
    Integer_Part        =   trim( StrOut(1:iSpe-1) )                                                        ! Getting the integer part of the real number, ie. all characters before the '.' character
    Fractional_Part     =   trim( StrOut(iSpe+1:)  )                                                        ! Getting the fractional part of the real number, ie. all characters after the '.' character
    Exponent_Part       =   ""                                                                              ! Initializing the exponent part
    iSpe          =   index( Fractional_Part, Separator_Fra_Exp )                                           ! Getting the index of the separation character between the fractional and exponent part (zero if any)
    if ( iSpe /= 0 ) then                                                                                       ! If a exponent part existe, then further splitting
      Exponent_Part     =   trim( Fractional_Part(iSpe+1:)  )                                               ! Getting the exponent part of the real number, ie. all characters after the 'E' character
      Fractional_Part   =   trim( Fractional_Part(1:iSpe-1)  )                                              ! Getting the fractional part of the real number, ie. all characters before the 'E' character
    end if                                                                                                      ! End if case on separation character presence
    if ( len_trim(Integer_Part) == 0 ) Integer_Part = "0"                                                       ! If no integer part (the input string has the format ".1234"), then explicitly setting the integer part is zero
    i   =   len(Fractional_Part) + 1                                                                        ! Initializing the character index to the length+1 of the fractional part string (beacause of backward processing of the string)
    do                                                                                                          ! Loop on all characters of the fractional part string
      i = i - 1                                                                                                 ! Incrementing the character index
      if ( i == 0 )                     exit                                                                    ! Exiting the loop if the 1st character the entire string has been processed, (that is, if i=0)
      if ( Fractional_Part(i:i)/="0" )  exit                                                                    ! Exiting the loop if current character does not correspond to a zero ("0")
      Fractional_Part(i:i)      =   " "                                                                     ! Replacing the zero character by a blank character
    end do                                                                                                      ! End loop on characters of the fractional part string
    Fractional_Part     =   trim( Fractional_Part )                                                         ! Removing trailling blanks
    if ( len_trim(Fractional_Part) == 0 ) Fractional_Part = "0"                                                 ! If the no fractional part (the input string has the format "X.000...000"), then setting it to zero so that it become "X.0"
    StrOut      =   Integer_Part                                                                            ! Reconstructing the character string from the integer ...
    if ( len_trim(Fractional_Part) /= 0 ) StrOut = StrOut // Separator_Int_Fra // Fractional_Part               ! ... the fractional part if required
    if ( len_trim(Exponent_Part) /= 0 )   StrOut = StrOut // Separator_Fra_Exp // Exponent_Part                 ! ... and the exponent part if required
  end if                                                                                                        ! End if case on separation character presence
End Procedure












! This procedure replaces in the character string 'String' all occurences of the sub-string contained in
! the variable 'Old' by the value contained in the variables 'New'.
! Example:
!   NewString   =   ReplaceCharacter( "this is a test", "test", "dog")    =>  NewString = "this is a dog"
Module Procedure ReplaceCharacter_C0d_From_C0d
  use Utilities_Library    ,only:  GetOptArgValue
  logical                                                               ::  Trimed_
  integer                                                               ::  ios, ioe                            ! Starting/ending index of old string
  integer                                                               ::  ins, ine                            ! Starting/ending index of new string
  character(:)  ,allocatable                                            ::  TmpString, Old_, New_
  Trimed_       =   GetOptArgValue(.False.,Trimed)
  if (Trimed_) then
    Old_        =   trim(Old)
    New_        =   trim(New)
  else
    Old_        =   Old
    New_        =   New
  end if
  NewString     =   ""
  TmpString     =   String
  do
    ios         =   index(TmpString,Old_)
    ioe         =   ios - 1 + len(Old_)
    ins         =   ios
    ine         =   ios - 1 + len(New_)
    if ( ios == 0 ) then
      NewString =   NewString // TmpString
      exit
    end if
    NewString =   NewString // TmpString(1:ios-1) // New_
    TmpString =   TmpString(ioe+1:)
  end do
End Procedure

! This procedure replaces in the character string 'String' all occurences of the sub-strings contained in the
! variable 'Old' by the values contained in the variables 'New'. The variables 'Old' and 'New' should have
! the same number of elements and each element in 'Old' is relpaced by the associated element in 'New'.
! Example:
!   NewString   =   ReplaceCharacter( "abcde abcde", ["b","c","d"], ["2","3","4"])    =>  NewString = "a234e a234e"
Module Procedure ReplaceCharacter_C0d_From_C1d
  integer                                                               ::  i
  NewString    =   String
  do i = 1,size(Old)
    NewString  =   ReplaceCharacter( NewString, Old(i), New(i), Trimed=Trimed )
  end do
  NewString    =   trim(NewString)
End Procedure

! This procedure replaces in all the character strings in 'Strings' all occurences of the sub-string contained in
! the variable 'Old' by the value contained in the variables 'New'.
! Example:
!   NewStrings  =   ReplaceCharacter( ["abcde","edcba"], "c", "X" )    =>  NewStrings = ["abXde","edXba"]
Module Procedure ReplaceCharacter_C1d_From_C0d
  integer                                                               ::  i, Length
  character(:)  ,allocatable                                            ::  TmpString
# ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
    _ASSIGN_ALLOCATABLE_CHARACTER_(NewStrings,Strings)
# else
  NewStrings    =   Strings
# endif
  Length            =   LenTrim(Strings)
  do i = 1,size(Strings)
    TmpString       =   ReplaceCharacter( Strings(i), Old, New, Trimed=Trimed )
    if ( len(TmpString) > Length ) then ! Increase string length if required
      Length        =   len(TmpString)
#     ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
#       define  _RHS_Var  SetLength( NewStrings, Length )
        _ASSIGN_ALLOCATABLE_CHARACTER_(NewStrings,_RHS_Var)
#       undef   _RHS_Var
#     else
      NewStrings    =   SetLength( NewStrings, Length )
#     endif
    end if
    NewStrings(i)   =   trim(TmpString)
  end do
# ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
    _ASSIGN_ALLOCATABLE_CHARACTER_(NewStrings,VecTrim(NewStrings))
# else
  NewStrings        =   VecTrim(NewStrings)
# endif
End Procedure

Module Procedure ReplaceCharacter_C1d_From_C1d
  integer                                                               ::  i, k, Length
  character(:)  ,allocatable                                            ::  TmpString
# ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
    _ASSIGN_ALLOCATABLE_CHARACTER_(NewStrings,Strings)
# else
  NewStrings    =   Strings
# endif
  Length            =   LenTrim(Strings)
  do i = 1,size(Strings)
    do k = 1,size(Old)
      TmpString     =   ReplaceCharacter( Strings(i), Old(k), New(k), Trimed=Trimed )
      if ( len(TmpString) > Length ) then ! Increase string length if required
        Length      =   len(TmpString)
#       ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
#         define  _RHS_Var  SetLength( NewStrings, Length )
          _ASSIGN_ALLOCATABLE_CHARACTER_(NewStrings,_RHS_Var)
#         undef   _RHS_Var
#       else
        NewStrings  =   SetLength( NewStrings, Length )
#       endif
      end if
      NewStrings(i) =   trim(TmpString)
    end do
  end do
# ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
    _ASSIGN_ALLOCATABLE_CHARACTER_(NewStrings,VecTrim(NewStrings))
# else
  NewStrings        =   VecTrim(NewStrings)
# endif
End Procedure






! Sources_  =   EscapeFileCharacters( Sources, ['(',')','=',','], ['\(','\)','\=','\,'] )

! This procedure replaces in the character string 'String' all occurences of the sub-string contained in
! the variable 'Old' by the value contained in the variables 'New'.
! Example:
!   NewString   =   EscapeFileCharacters( "this is a test", "test", "dog")    =>  NewString = "this is a dog"
Module Procedure EscapeFileCharacters_C0d_From_C0d
  use Utilities_Library    ,only:  AddElementToArray
  character(*)                                              ,parameter  ::  DefOld(5) = ['(' ,')' ,'=' ,',' ,'$' ]
  character(*)                                              ,parameter  ::  DefNew(5) = ['\(','\)','\=','\,','\$']
  character(:)  ,allocatable                                            ::  TmpString
  character(:)  ,allocatable                                            ::  ListOld(:), ListNew(:)
  ListOld  =   DefOld
  ListNew  =   DefNew
  if ( present(Old) ) call AddElementToArray(Old,ListOld)
  if ( present(New) ) call AddElementToArray(New,ListNew)
  NewString   =   ReplaceCharacter( String, ListOld, ListNew, Trimed=.True. )
End Procedure

Module Procedure EscapeFileCharacters_C1d_From_C1d
  use Utilities_Library    ,only:  AddElementToArray
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  String
  do i = 1,size(Strings)
    String    =   EscapeFileCharacters( trim(Strings(i)), Old, New )
    call AddElementToArray( String, NewStrings )
  end do
End Procedure


! ! This procedure replaces in the character string 'String' all occurences of the sub-strings contained in the
! ! variable 'Old' by the values contained in the variables 'New'. The variables 'Old' and 'New' should have
! ! the same number of elements and each element in 'Old' is relpaced by the associated element in 'New'.
! ! Example:
! !   NewString   =   EscapeFileCharacters( "abcde abcde", ["b","c","d"], ["2","3","4"])    =>  NewString = "a234e a234e"
! Module Procedure EscapeFileCharacters_C0d_From_C1d
!   integer                                                               ::  i
!   NewString    =   String
!   do i = 1,size(Old)
!     NewString  =   EscapeFileCharacters( NewString, Old(i), New(i) )
!   end do
!   NewString    =   trim(NewString)
! End Procedure
!
! ! This procedure replaces in all the character strings in 'Strings' all occurences of the sub-string contained in
! ! the variable 'Old' by the value contained in the variables 'New'.
! ! Example:
! !   NewStrings  =   EscapeFileCharacters( ["abcde","edcba"], "c", "X" )    =>  NewStrings = ["abXde","edXba"]
! Module Procedure EscapeFileCharacters_C1d_From_C0d
!   integer                                                               ::  i, Length
!   character(:)  ,allocatable                                            ::  TmpString
! # ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
!     _ASSIGN_ALLOCATABLE_CHARACTER_(NewStrings,Strings)
! # else
!   NewStrings    =   Strings
! # endif
!   Length            =   LenTrim(Strings)
!   do i = 1,size(Strings)
!     TmpString       =   EscapeFileCharacters( Strings(i), Old, New )
!     if ( len(TmpString) > Length ) then ! Increase string length if required
!       Length        =   len(TmpString)
!       NewStrings    =   SetLength( NewStrings, Length )
!     end if
!     NewStrings(i)   =   trim(TmpString)
!   end do
!   NewStrings        =   VecTrim(NewStrings)
! End Procedure
!






! This procedure is used to enhanced species names so they have a nice rendering in LaTeX.
! Only works in a limited number of case:
! 1) the number must be lower than 9 (only one digits) (always the case for the considered species)
! 2) the fist character should not be a number (never the case for species name)
! 3) only work for one substitution (because of the allocate statement))
! However, that's fine for my needs...
Module Procedure Set_Enhanced_Name
  integer                                                               ::  i                               ! Index of character position
  character(:)  ,allocatable                                            ::  String_Old
  character(:)  ,allocatable                                            ::  String_New
  character(:)  ,allocatable                                            ::  String_L, String_R, String_C
  Enhanced_Name                    =   Name
  do i = 1,len(Enhanced_Name)
    String_Old                  =   Enhanced_Name(i:i)
    if ( .not. Is_Numeric(String_Old) ) cycle
    String_New                =   "_{" // String_Old // "}"
    String_L          =   Enhanced_Name(1:i-1)     ! String on the LHS of the substritution
    String_C          =   String_New
    String_R          =   Enhanced_Name(i+1:)      ! String on the RHS of the substritution
    Enhanced_Name     =   String_L // String_C // String_R
  end do
  do i = 1,len(Enhanced_Name)
    String_Old                  =   Enhanced_Name(i:i)
    if ( (String_Old/='+') .and. (String_Old/='-') ) cycle
    String_New                =   "^{" // String_Old // "}"
    String_L          =   Enhanced_Name(1:i-1)
    String_C          =   String_New
    String_R          =   Enhanced_Name(i+1:)
    Enhanced_Name     =   String_L // String_C // String_R
  end do
  Enhanced_Name         =   trim(Enhanced_Name)
End Procedure

Module Procedure Is_Numeric
  character(*)          ,parameter                              ::  Valid_Charaters=' 1234567890.'          ! List of characters wich are valid numbers
  Is_Numeric    =   ( verify(String,Valid_Charaters) == 0 )                                                 ! Setting the numeric indicator ( verify returns 0 if each character in String appears in Valid_Charaters)
End Procedure

Module Procedure Is_A_Number
  real(8)                                                       ::  Number
  integer                                                       ::  ios
  read(String,*,iostat=ios) Number                                                                                         ! Converting the input string into a real number
  Is_A_Number    =  ( ios == 0 )
End Procedure

Module Procedure IsIntegerNumber
  use Utilities_Library    ,only:  PresentAndTrue
  real(8)                                                       ::  RealNumber
  integer                                                       ::  ios
  IsIntegerNumber   =   .False.
  if ( .Not. PresentAndTrue(Equiv) .and. (index(String,".") /=0 ) ) return ! If the is a dot ".", exit
  read(String,*,iostat=ios) RealNumber
  if ( ios == 0 ) IsIntegerNumber =  ( mod(RealNumber,1.0_8) == 0 )
End Procedure

Module Procedure IsRealNumber
  use Utilities_Library    ,only:  AbsentOrFalse
  real(8)                                                       ::  RealNumber
  integer                                                       ::  ios
  read(String,*,iostat=ios) RealNumber
  IsRealNumber    =  ( ios == 0 )
  if ( AbsentOrFalse(Equiv) .and. IsRealNumber ) IsRealNumber = .Not. IsIntegerNumber(String)
End Procedure

Module Procedure Remove_Last_Directory_From_Path
  integer                                                               ::  Length
  integer                                                               ::  iSlash
  character(*)                                              ,parameter  ::  Slash_Char = '/'
  character(1)                                                          ::  Last_Char
  logical                                                               ::  Trailing_Slash
  DirOut        =   trim(DirInp)
  Length        =   len_trim(DirOut)
  Last_Char     =   DirOut(Length:Length)
  Trailing_Slash        =   ( Last_Char == Slash_Char )
  if (Trailing_Slash) DirOut = DirOut(1:Length-1)       ! Remove the trailing slash
  iSlash        =   index(DirOut,Slash_Char,back=.True.)
  if (Trailing_Slash) then
    DirOut      =   DirOut(1:iSlash)
  else
    DirOut      =   DirOut(1:iSlash-1)
  end if
End Procedure

Module Procedure GetFilePath
  integer                                                               ::  iSeparator
  logical                                                   ,parameter  ::  Backward  = .True.
  character(*)                                              ,parameter  ::  Separator = "/"
  iSeparator    =   scan( FileName, Separator, Backward )
  FilePath      =   ""
  if ( iSeparator > 1 ) FilePath = FileName(1:iSeparator)
End Procedure

Module Procedure GetBaseName
  integer                                                               ::  iSeparator
  logical                                                   ,parameter  ::  Backward  = .True.
  character(*)                                              ,parameter  ::  Separator = "/"
  iSeparator    =   scan( FileName, Separator, Backward )
  BaseName      =   FileName
  if (iSeparator>1) BaseName = FileName(iSeparator+1:)
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
!       PROCEDURES FOR INLINING AN ARRAY OF CHARACTER STRINGS INTO A SCALAR CHARACTER STRING
! **************************************************************************************************************
! **************************************************************************************************************

! # define _Debug_ !
! ! # define _Debug_
Module Procedure GetSubString
  use Utilities_Library    ,only:  AbsentOrTrue, PresentAndTrue
!   use Logger_Class         ,only:  Logger
  integer                                                               ::  is, ie, js, je, ks, ke
  integer                                                               ::  Ns, Ne
  character(:)  ,allocatable                                            ::  StartKey, EndKey

  ! _Debug_ call Logger%Entering("GetSubString" )

  SubString =   ""
  StartKey  =   trim(InBetween(1))
  EndKey    =   trim(InBetween(2))

  Ns  =   0
  Ne  =   0

  ! _Debug_ call Logger%Write("-> String   = ", String )
  ! _Debug_ call Logger%Write("-> StartKey = ", StartKey )
  ! _Debug_ call Logger%Write("-> EndKey   = ", EndKey )
  ! _Debug_ call Logger%Write("-> PresentAndTrue(Inner) = ", PresentAndTrue(Inner)  )
  is         =   index( String, StartKey )
  if ( is == 0 ) return

  Ns  =   Ns + 1
  is  =   is + len(StartKey)
  ie  =   index( String, EndKey )
  if ( ie == 0 ) return
  ! _Debug_ call Logger%Write( "-> is = ", is, "ie = ", ie, "String(is:ie)=|"//String(is:ie)//"|", Fi="i3" )
  if ( AbsentOrTrue(Protect) ) then
    Loop1: do js = is,len(String)

      ! _Debug_ call Logger%Write( "  -> js = ", js )

!     Checking if the next characters correspond to a start key
      je  =   min(js-1+len(StartKey),len(String))
      if ( String(js:je) == StartKey ) then
        Ns  =   Ns + 1
        ! _Debug_ call Logger%Write( "  -> StartKey found: [Ns,Ne] = ", Inline([Ns,Ne]), "js = ", js, "je = ", je, "String(js:)=|"//String(js:)//"|", Fi="i3" )
        if ( PresentAndTrue(Inner) ) then
          is  =   js + len(StartKey)
          ! _Debug_ call Logger%Write( "  -> Inner: is = ", is )
        end if
      end if

!     Checking if the next characters correspond to a end key
      je  =   min(js-1+len(EndKey),len(String))
      if ( String(js:je) == EndKey ) then
        Ne  =   Ne + 1
        ! _Debug_ call Logger%Write( "* -> EndKey found:   [Ns,Ne] = ", Inline([Ns,Ne]), "js = ", js, "je = ", je, "String(js:je)=|"//String(js:je)//"|", Fi="i3" )
        if ( PresentAndTrue(Inner) ) then
          ie = je
          ! _Debug_ call Logger%Write( "  -> Inner: ie = ", ie )
          exit
        else
          ie  =   js
        end if

      end if

      ! _Debug_ call Logger%Write( "  -> js = ", js, "Ns-Ne = ", Ns-Ne )
      if ( Ns-Ne == 0 ) exit

    end do Loop1
  end if

!   if ( PresentAndTrue(Inner) ) then
!     ie = ke-1
!   else
    ie = ie-1
!   end if
    SubString   =   String(is:ie)

  ! _Debug_ call Logger%Write( "-> is = ", is, "ie = ", ie, "String(is:ie)=|"//String(is:ie)//"|", Fi="i3" )
  ! _Debug_ call Logger%Exiting

End Procedure
! # undef _Debug_

Module Procedure GetSubStringIndexes
  character(:)  ,allocatable                                            ::  Str1
  character(:)  ,allocatable                                            ::  Str2
  integer                                                               ::  iIni
  integer                                                               ::  iFin
  Indexes       =   0
  Str1          =   trim(InBetween(1))
  Str2          =   trim(InBetween(2))
  iIni          =   index( InpStr, Str1 )
  if ( iIni == 0 ) return
  iIni          =   iIni + len(Str1)
  iFin          =   index( InpStr, Str2, back=.True. )
  if ( iFin == 0 ) return
  iFin          =   iFin - 1
  Indexes(1)    =   iIni
  Indexes(2)    =   iFin
End Procedure

Module Procedure Indent_0d
  character(:)  ,allocatable                                            ::  Prefix
  Prefix    =   ""
  if ( Indentation > 0 ) Prefix = repeat(" ",Indentation)
  IndentedString  =   AddPrefix( String, Prefix )
End Procedure

Module Procedure Indent_1d
  character(:)  ,allocatable                                            ::  Prefix
  Prefix    =   ""
  if ( Indentation > 0 ) Prefix = repeat(" ",Indentation)
  IndentedStrings =   AddPrefix( Strings, Prefix )
End Procedure

Module Procedure AddPrefix_0d
  OutputString       =   Prefix // String
End Procedure

Module Procedure AddPrefix_1d
  integer                                                               ::  i
# ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
  character(:)  ,dimension(:)           ,allocatable                    ::  tmp
  allocate( character(len(Prefix)+len(Strings)) :: tmp(size(Strings)) )
  do i = 1,size(Strings)
    tmp(i)   =   Prefix // Strings(i)
  end do
  call move_alloc( tmp, OutputStrings )
# else
  allocate( character(len(Prefix)+len(Strings)) :: OutputStrings(size(Strings)) )
  do i = 1,size(Strings)
    OutputStrings(i)   =   Prefix // Strings(i)
  end do
# endif
End Procedure



Module Procedure RemovePrefix_0d
  integer                                                               ::  N
  OutputString       =   InputString
  if ( len_trim(OutputString) < len_trim(Prefix) ) return
  if ( len_trim(Prefix) == 0 ) return
  if ( len_trim(OutputString) == 0 ) return
  N   =   len_trim(Prefix)
  if ( OutputString(1:N) /= Prefix(1:N) ) return
  OutputString  =   trim(OutputString(N+1:))
End Procedure


Module Procedure Add_Suffix_0d
  OutputString       =   trim(String) // Suffix
End Procedure

Module Procedure Add_Suffix_1d
  integer                                                               ::  i
# ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
  character(:)  ,dimension(:)           ,allocatable                    ::  tmp
  allocate( character(len(Suffix)+len(Strings)) :: tmp(size(Strings)) )
  do i = 1,size(Strings)
    tmp(i)   =   trim(Strings(i)) // Suffix
  end do
  call move_alloc( tmp, OutputStrings )
# else
  allocate( character(len(Suffix)+len(Strings)) :: OutputStrings(size(Strings)) )
  do i = 1,size(Strings)
    OutputStrings(i)   =   trim(Strings(i)) // Suffix
  end do
# endif
End Procedure

Module Procedure Convert_Ratio
  integer                                                               ::  NDigits
  integer                                                               ::  Length_
  character(:)  ,allocatable                                            ::  NumStr
  character(:)  ,allocatable                                            ::  DenStr
  NDigits       =   GetNumberOfDigits( Denominator )                                                     ! Getting the number of digits of the denominator
  NumStr        =   Convert_To_String( Numerator, Pos='R', Len=NDigits )                                    ! Converting the numerator to a string
  DenStr        =   Convert_To_String( Denominator )                                                        ! Converting the numerator to a string
  Ratio         =   NumStr // "/" // DenStr                                                                 ! Setting the string corresponding to the 2 input numbers
  if ( present(Length) ) Ratio = SetLength(Ratio,Length)
  if ( present(MaxLength) ) then
    Length_     =   2 * NDigits + 1
    Ratio       =   SetLength(Ratio,Length_)
  end if
End Procedure

Module Procedure RemoveCharacter_0d
  Output    =   ReplaceCharacter( Input, String, "" )
End Procedure

Module Procedure RemoveCharacter_1d
  integer                                                               ::  i
  Output    =     Input
  do i = 1,size(String)
    Output  =     ReplaceCharacter( Output, String(i), "" )
  end do
End Procedure

Module Procedure CountCharacter_0d
  integer                                                               ::  i, j, N
  character(:)  ,allocatable                                            ::  Copy
  Copy    =   Input
  Number  =   0
  N       =   len(String)
  do
    i     =   index(Copy,String)
    if ( i == 0 ) exit
    Copy    =   Copy(i+N:)
    Number  =   Number + 1
  end do
End Procedure

! Module Procedure CountCharacter_1d
!   integer                                                               ::  i
!   Output    =     Input
!   do i = 1,size(String)
!     Output  =     ReplaceCharacter( Output, String(i), "" )
!   end do
! End Procedure


Module Procedure Equal
  use Utilities_Library    ,only:  GetOptArgValue
  logical                                                               ::  CaseSens_, Trimed_
  character(:)  ,allocatable                                            ::  C1, C2
  C1        =   S1
  C2        =   S2
  CaseSens_ =   GetOptArgValue(.True.,CaseSensitive)
  Trimed_   =   GetOptArgValue(.True.,Trimed)
  if ( .Not. CaseSens_ ) then
    C1      =   UpperCase(C1)
    C2      =   UpperCase(C2)
  end if
  if ( Trimed_ ) then
    C1      =   trim(C1)
    C2      =   trim(C2)
  end if
  Equal     =   ( C1 == C2 )
End Procedure

! ! **************************************************************************************************************
! ! **************************************************************************************************************
! !                             PRIVATE PROCEDURES
! ! **************************************************************************************************************
! ! **************************************************************************************************************
!
! Subroutine Error_Message( ErrMsg, ProcName )
!   character(*)                                ,optional ,intent(in)     ::  ErrMsg
!   character(*)                                ,optional ,intent(in)     ::  ProcName
!   write(*,"('Fatal error leading to code termination')")
!   if ( present(ProcName) ) write(LogUnit,"(4x,'Calling procedure   :   ',g0)") trim(ProcName)                   ! Printing the calling procedure
!   if ( present(ErrMsg)   ) write(LogUnit,"(4x,'Error message       :   ',g0)") trim(ErrMsg)                     ! Printing the error message
!   stop                                                                                                          ! Stopping the code
! End Subroutine
! @TODO: Implement Invert
Module Procedure Inline_Strings_1d

  use Utilities_Library    ,only:  GetOptArgValue, SelectedItem, FirstSkippedItem

  character(*)                                              ,parameter  ::  DefaultSeparator=" "
  integer                                                               ::  i                               ! Indexes of the input character string elements
  character(:)  ,allocatable                                            ::  String
  character(:)  ,allocatable                                            ::  Separator_                 ! Local separator used between elements
  logical                                                               ::  Trimed_                    ! Local indicator whether the strings should be trimed
  logical                                                               ::  Invert_
  logical                                                               ::  ChangeLength

  Separator_  =   GetOptArgValue( DefaultSeparator , Separator )
  Trimed_     =   GetOptArgValue( .True.  , Trimed    )
  Invert_     =   GetOptArgValue( .False. , Invert    )
  ChangeLength    =   .False.
  if ( present(Length) ) then
    ChangeLength  =   .True.
    Trimed_       =   .False.
  end if
  OutStr        =   ""

  if ( .Not. present(NMax) ) then

    do i = 1,size(InpStr)
      if ( present(Fmt) ) then
        String    =   Convert_To_String( InpStr(i), Fmt=Fmt )
      else
        String    =   InpStr(i)
      end if
      if (ChangeLength) String = SetLength( String, Length )
      if (Trimed_) String = trim(String)
      OutStr      =   OutStr // String
      if ( i /= size(InpStr) ) then
        if (Trimed_) then
          OutStr  =   trim(OutStr) // Separator_
        else
          OutStr  =   OutStr // Separator_
        end if
      end if
    end do

  else

    do i = 1,size(InpStr)
      if ( SelectedItem( i, size(InpStr), NMax ) ) then
        if ( present(Fmt) ) then; String = Convert_To_String( InpStr(i), Fmt=Fmt )
        else;                     String = InpStr(i); end if
      else
        if ( FirstSkippedItem( i, size(InpStr), NMax ) ) then
          String    =   "..."
        else
          cycle
        end if
      end if
      if (ChangeLength) String = SetLength( String, Length )
      if (Trimed_) String = trim(String)
      OutStr      =   OutStr // String
      if ( i /= size(InpStr) ) then
        if (Trimed_) then
          OutStr  =   trim(OutStr) // Separator_
        else
          OutStr  =   OutStr // Separator_
        end if
      end if
    end do

  end if

End Procedure

! This procedure returns a scalar character string corresponding to the inlined input vector of reals
Module Procedure Inline_Reals_1D
  character(:)  ,dimension(:)   ,allocatable                            ::  InpStr                          !< Array of strings to be inlined
  integer                                                               ::  i                               ! Indexes of the input character string elements
  character(:)  ,allocatable                                            ::  String
!   logical                                                               ::  ChangeLength
  do i = 1,size(InpVar)
    String      =   Convert_To_String( InpVar(i), Fmt=Fmt, Len=Length )
    call Add_Line_To_String( InpStr, String )
  end do
  OutStr        =   Inline( InpStr, Separator, Trimed, Invert )
End Procedure

! This procedure returns a scalar character string corresponding to the inlined
! input vector of integers.
Module Procedure Inline_Integers_1D
  character(:)  ,dimension(:)   ,allocatable                            ::  InpStr                          !< Array of strings to be inlined
  integer                                                               ::  i                               ! Indexes of the input character string elements
  character(:)  ,allocatable                                            ::  String
!   do i = 1,size(InpVar)
!     String  =   Convert_To_String( InpVar(i), Fmt=Fmt, Len=Length )
!     call Add_Line_To_String( InpStr, String )
!   end do
!   OutStr    =   Inline( InpStr, Separator, Trimed, Invert )

! @TODO: Use: call Convert( InpVar, InpStr )
! @TODO: Try to avoid to convert all elements is not needed
  allocate( Character(100) :: InpStr(size(InpVar)) )
  do i = 1,size(InpVar)
!     write(InpStr(i),"(i0)") InpVar(i)
    InpStr(i)   =   Convert_To_String( InpVar(i), Fmt=Fmt, Len=Length )
  end do
  OutStr    =   Inline( InpStr              &
                  , Separator =   Separator &
                  , Trimed    =   Trimed    &
                  , Invert    =   Invert    &
!                   , Fmt       =   Fmt       &
                  , Length    =   Length    &
                  , NMax      =   NMax      )


End Procedure


! @COMPILER_BUG: gcc-6.3.1: This function need to be called using the "(:)" on the LHS string otherwise there is a ICE: "String(:) = Remove_Quotes(String)"

Module Procedure RemoveQuotes_0d
  character(:)  ,allocatable                                            ::  Value
  character(1)                                                          ::  FirstChar, LastChar, Key
  character(1)                                              ,parameter  ::  ListKeys(2) = ['"',"'"]
  integer                                                               ::  i
  OutputString    =   InputString
  Value           =   trim( InputString )
  if ( len_trim(Value) < 2 ) return
  FirstChar       =   Value(1:1)                                                                              ! Getting the first character
  LastChar        =   Value(len(Value):len(Value))                                                            ! Getting the last character
  do i = 1,size(ListKeys)
    Key   =   ListKeys(i)
    if ( .Not. ( (FirstChar==Key) .and. (LastChar==Key) ) ) cycle
    Value         =   Value(2:len(Value)-1)
    OutputString  =   Value
    exit  ! Comment this line to remove several quotes
  end do
End Procedure

Module Procedure RemoveQuotes_1d
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  Value
  character(1)                                                          ::  FirstChar, LastChar
  character(len(InputString))   ,dimension(size(InputString))           ::  TmpString
  TmpString         =   InputString
  do i = 1,size(InputString)
    TmpString(i)    =   RemoveQuotes( InputString(i) )
  end do
# ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
! @COMPILER_BUG: gcc-7.3.0: ICE
  Block
    character(:)  ,allocatable    ::  Tmp(:)
    allocate( Tmp, source = TmpString )
    call move_alloc( Tmp, OutputString )
  End Block
# else
  allocate( OutputString, source = TmpString )
# endif

End Procedure

Module Procedure Quote_0d
  character(1)                                                          ::  FirstChar, LastChar
  integer                                                               ::  Length
  QuotedString    =   trim(String)
  Length          =   len_trim(QuotedString)
  if ( Length >= 2 ) then
    FirstChar     =   QuotedString(1:1)
    LastChar      =   QuotedString(Length:Length)
    if ( FirstChar == '"' .and. LastChar == '"' ) return  ! Already quoted
  end if
  QuotedString    =   '"' // QuotedString // '"'
End Procedure

Module Procedure Quote_1d
  integer                                                               ::  i
  integer                                                               ::  Length
  character(len(String)+2)  ,dimension(size(String))                    ::  TmpString
  TmpString       =   String
  Length          =   0
  do i = 1,size(String)
    TmpString(i)  =   Quote( String(i) )
    Length        =   max( Length , len_trim(TmpString(i)))
  end do
# ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
#   define  _RHS_Var    TmpString(:)(1:Length)
    _ASSIGN_ALLOCATABLE_CHARACTER_(QuotedString,_RHS_Var)
#   undef   _RHS_Var
# else
  QuotedString    =   TmpString(:)(1:Length)
# endif
End Procedure


Module Procedure RemoveDuplicateCharacters
  integer                                                               ::  k, Length, j
  character(:)  ,allocatable                                            ::  Str, Left, Right
  CleanedString   =   ""
  Length          =   len(SubString)
  Right           =   String
  do
    k     =   index(Right,SubString)              ! Find the index of the character to replace
    if ( k == 0 ) then                            ! If not chara., then exit
      CleanedString =   CleanedString // Right
      exit
    else                                          ! If character found, split the string in left/right (left contaisn the character)
      Left          =   Right(1:k)
      Right         =   Right(k+1:)
      do                                          ! Loop to remove all duplicate character
        if ( len(Right) < Length ) exit
        Str         =   Right(1:Length)
        if ( Str /= SubString ) exit
        Right       =   Right(Length+1:)
      end do
      CleanedString =   CleanedString // Left
    end if
  end do
End Procedure


Module Procedure StartWith_0d
  logical                                                               ::  CaseSensitive_, Trimed_
  integer                                                               ::  k
  character(:)  ,allocatable                                            ::  Str, SubStr
  Trimed_         =   .True.
  CaseSensitive_  =   .True.
  if ( present(CaseSensitive) ) CaseSensitive_  =   CaseSensitive
  if ( present(Trimed) )        Trimed_         =   Trimed
  Str       =   String
  SubStr    =   SubString
  if ( .Not.CaseSensitive_ ) then
    Str     =   UpperCase(Str   )
    SubStr  =   UpperCase(SubStr)
  end if
  if ( Trimed_ ) then
    Str     =     trim(Str   )
    SubStr  =     trim(SubStr)
  end if
  Indicator =   index( Str, SubStr ) == 1
End Procedure

Module Procedure StartWith_1d
  integer                                                               ::  i
  do i = 1,size(SubStrings)
    Indicator   =   StartWith( String, trim(SubStrings(i)), Trimed, CaseSensitive )
    if ( Indicator ) exit
  end do
End Procedure


Module Procedure GetAssociatedString
  B   =   A
  select case (A)
    case ("("); B = ")"
    case ("["); B = "]"
    case ("{"); B = "}"
    case (")"); B = "("
    case ("]"); B = "["
    case ("}"); B = "{"
  end select
End Procedure



! This procedure returns the last index of the substring which correspond to the
! specification of the exponent part of a number. If the input string does not
! corresponds to an exponent part, then 0 is returned.
! For the string to be identify as an exponent part it must have the following
! properties:
! * Its 1st character must start one of the following characters: {e,E,d,D}
! * Its 2nd character must be a digit, or the character '+' or '-'
! * If there is character '+' or '-', then the next character must be a digit
! Examples:
!   Procedure call                      Results
!   --------------                      -------
!   Last = GetFinalIndexExponentPart( "E0" )       2
!   Last = GetFinalIndexExponentPart( "E0 0" )     2
!   Last = GetFinalIndexExponentPart( "E+0" )      3
!   Last = GetFinalIndexExponentPart( "E-0" )      3
!   Last = GetFinalIndexExponentPart( "E+0 abc" )  3
!   Last = GetFinalIndexExponentPart( "E00125" )   6
Module Procedure GetFinalIndexExponentPart
  integer                                                               ::  k
  character(1)                                                          ::  s
  Last        =   0
  if ( len_Trim(String) == 0 ) return
  s           =   String(1:1)
  if ( .Not. IsExponentCharacter(s) ) return ! Return if 1st character is not a valid 'exponent' character {e,E,d,D}.
  k           =   1
  MainLoop: do
    k     =   k + 1
    if ( k > len_trim(String) ) exit
    s     =   String(k:k)
    if ( (s==" ") .or. Is_Letter(s) ) exit                 ! Exit if current character is an space of a letter
    if ( k == len_trim(String) .and. Is_Digit(s) ) then
      Last        =   k
      exit
    end if
    if ( .Not. Is_Digit(s) ) cycle ! Skiping the '+'/'-' character
    Last  =   k
    Next: do
      k   =   k + 1
      if ( k > len_trim(String) ) exit
      s    =   String(k:k)
      if ( Is_Digit(s) ) Last = k
      if ( .Not. Is_Digit(s) ) exit MainLoop
      if ( s == " " ) exit MainLoop
    end do Next
  end do MainLoop
End Procedure

! This procedure indicates whether the input character corresponds to a exponent character,
! that is, if it corresponds to one of these characters {e,E,d,D}.
Module Procedure IsExponentCharacter
  Indicator   =   (s=="e") .or. (s=="E") .or. (s=="d") .or. (s=="D")
End Procedure

! This procedure indicates whether the input character corresponds to a valid character whcih
! comes after an exponent character, that is, if it is a digit, a '+' or a '-' character.
! Indeed, the exponent  part of a number are limited to the following subset
! {E+<N>, E-<N>, E<N>} where <N> is a arbirtraty integer.
Module Procedure IsPostExponentCharacter
  Indicator   =   (Is_Digit(s)) .or. (s=="+") .or. (s=="-")
End Procedure


Module Procedure NiceInteger
  integer                                                               ::  N, i, j, k
  character(:)  ,allocatable                                            ::  Copy, Str
  Copy      =   Convert_To_String(Number)
  N         =   len_trim(Copy)
  String    =   ""
  j         =   0
  do k = 1,N
    i       =   N + 1 - k
    String  =   Copy(i:i) // trim(String)
    j       =   j + 1
    if ( j /= 3 ) cycle
    if ( k == N ) cycle
    j       =   0
    String  =   "," // trim(String)
  enddo
End Procedure




Module Procedure GetSeconds
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  Letters(:), Unit
  real(REAL64)  ,allocatable                                            ::  Numbers(:)
  real(REAL64)                                                          ::  Value
  call GetNumberLetterPairs( String, Numbers, Letters )
  Seconds   =   0
  do i = 1,size(Numbers)
    Value   =   Numbers(i)
    Unit    =   trim(Letters(i))
    select case(Unit)
      case ("","s","sec","second","seconds"); Seconds = Seconds + Value
      case ("m","min","minute","minutes");    Seconds = Seconds + Value * 60
      case ("h","hour","hours");              Seconds = Seconds + Value * 60*60
      case ("d","day","days");                Seconds = Seconds + Value * 60*60*24
    end select
  end do
End Procedure

Module Procedure RemoveComment
  integer                                                               ::  i
  OutputString  =   InputString
  if ( len_trim(Comment) == 0 ) return
  i             =   index( OutputString , Comment )
  if ( i == 0 ) return
  OutputString  =   trim( OutputString(1:i-1) )
End Procedure


! **************************************************************************************************************
! **************************************************************************************************************
!                             PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Error_Message( ErrMsg, ProcName )
  character(*)                                ,optional ,intent(in)     ::  ErrMsg
  character(*)                                ,optional ,intent(in)     ::  ProcName
  write(*,"('Fatal error leading to code termination')")
  if ( present(ProcName) ) write(LogUnit,"(4x,'Calling procedure   :   ',g0)") trim(ProcName)                   ! Printing the calling procedure
  if ( present(ErrMsg)   ) write(LogUnit,"(4x,'Error message       :   ',g0)") trim(ErrMsg)                     ! Printing the error message
  stop                                                                                                          ! Stopping the code
End Subroutine

End SubModule
