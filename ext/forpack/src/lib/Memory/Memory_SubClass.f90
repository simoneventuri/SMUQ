SubModule(Memory_Class) Memory_SubClass

  use Logger_Class      ,only:  Logger
  use Table_Class       ,only:  Table_Type

  implicit none

  logical       ,parameter      ::  i_Debug_Loc = .False.

  contains

Module Procedure FinalizeMemory
  This%Bits    =       0
  This%Level   =       1
  if ( allocated(  This%Var_Name  ) ) deallocate( This%Var_Name  )
  if ( allocated(  This%Type_Name ) ) deallocate( This%Type_Name )
  if ( associated( This%SubMemory ) ) nullify( This%SubMemory )
End Procedure

Module Procedure InitializeMemory
  character(:)  ,allocatable                                            ::  Variable_Name           !< Variable name
  Variable_Name         =       "<Undefined>"
  if ( present(Var_Name) ) Variable_Name = Var_Name
  This%Type_Name        =       Type_Name                                                               ! Setting the object type
  This%Var_Name         =       Variable_Name                                                           ! Setting the object type
  This%Bits            =       0                                                                       ! Initializing the memory size to zero [bite]
  This%Level            =       1                                                                       ! Initializing the memory level to one
  allocate( This%SubMemory(0) )                                                                         ! Initializing the SubMemory component
End Procedure

Module Procedure FreeMemory
  call FinalizeMemory( This )
End Procedure


Module Procedure GetBits
  Bits  =   This%Bits
End Procedure

Module Procedure Set_Variable_Name
  This%Var_Name =       Var_Name                                                                        ! Setting the variable name
End Procedure

Module Procedure Add_Value_To_Memory
  This%Bits    =       This%Bits + Value                                                              ! Adding the input value to the storage memory
End Procedure

! This procedure adds a Memory object as an element of the SubMemory component of the passed-object dummy argument.
Module Procedure Add_SubMemory
  type(Memory_Type)     ,dimension(:)   ,allocatable                    ::  List
  type(Memory_Type)                                                     ::  Memory
  integer                                                               ::  i
  Memory        =       SubMemory                                                                               ! Copying the input SubMemory object to a temporary varibale ...
  Memory%Level  =       This%Level + 1                                                                          ! ... and increasing the level
!   allocate( List, source = [This%SubMemory,Memory] )    ! @COMPILER_BUG                                                     ! Creating a Array of Memory object containing the old and new ones
  if ( associated(This%SubMemory) ) then
    allocate( List, source = [This%SubMemory,Memory] )                                                         ! Creating a Array of Memory object containing the old and new ones
  else
    allocate( List, source = [Memory] )                                                         ! Creating a Array of Memory object containing the old and new ones
  end if
!   call move_alloc( From=List, To=This%SubMemory )     ! This line can only be use if the SubMemory component has the 'allocatable' attribute, which is not possible due a an non-implemented feature (cf type definition).
  if ( associated(This%SubMemory) ) deallocate( This%SubMemory )
!   allocate( This%SubMemory, source = List )         ! @COMPILER_BUG                              ! Reallocating the SubMemory component to the new array
  allocate( This%SubMemory( size(List) ) )         ! @COMPILER_BUG                              ! Reallocating the SubMemory component to the new array
  do i = 1,size(List)
    This%SubMemory(i)     =       List(i)
  end do
End Procedure

Module Procedure OutputToLogger
  character(*)                                              ,parameter  ::  ProcName = "OutputToLogger" ! Name of current procedure
  integer                                                               ::  i
  call Logger%Entering( ProcName )
  call Logger%Write( "Memory state:" )
  call Logger%Write( "-> This%Var_Name              = ", This%Var_Name              )
  call Logger%Write( "-> This%Type_Name             = ", This%Type_Name             )
  call Logger%Write( "-> This%Bits                  = ", This%Bits                  )
  call Logger%Write( "-> associated(This%SubMemory) = ", associated(This%SubMemory) )
  if (associated(This%SubMemory)) then
    call Logger%Write( "-> size(This%SubMemory) = ", size(This%SubMemory) )
    do i = 1,size(This%SubMemory)
      call Logger%Write( "-> i = ", i, "This%SubMemory(i)%Bits = ", This%SubMemory(i)%Bits )
    end do
  end if
  call Logger%Exiting()
End Procedure

! This procedure adds 2 Memory objects into a single Memory object
! First, the 1st Memory object is copied into the output Memory object ( 'Memory = This' )
Module Procedure Addition_Memory_0d

!   character(*)                                              ,parameter  ::  ProcName = "Addition_Memory_0d" ! Name of current procedure
  type(Memory_Type)                                                     ::  Mem2
  type(Memory_Type)     ,dimension(:)   ,allocatable                    ::  List
  integer                                                               ::  i

!   if (i_Debug_Loc) call Logger%Entering( ProcName )
!   if (i_Debug_Loc) then
!     call Logger%Write( "On entry: Calling This%OutputToLogger" )
!     call This%OutputToLogger()
!   end if

!   if (i_Debug_Loc) call Logger%Write( "Assignment: Memory = This" )
  Memory        =       This

!   if (i_Debug_Loc) then
!     call Logger%Write( "After assignment: Calling Memory%OutputToLogger" )
!     call Memory%OutputToLogger()
!   end if


! ! **********************************************************************************************************************
!   if (i_Debug_Loc) call Logger%Write( "Calling Memory%Add_SubMemory( That )" )
! !   call Memory%Add_SubMemory( That )                                                                             ! Add an additional element to the SubMemory component of the Memory object of the passed-object dummy argument
! ! **********************************************************************************************************************
  Mem2        =       That                                                                               ! Copying the input SubMemory object to a temporary varibale ...
  Mem2%Level  =       Memory%Level + 1                                                                          ! ... and increasing the level
!   allocate( List, source = [Memory%SubMemory,Mem2] )    ! @COMPILER_BUG                                                     ! Creating a Array of Memory object containing the old and new ones
  if ( associated(Memory%SubMemory) ) then
!     allocate( List, source = [Memory%SubMemory,Mem2] )                                                         ! Creating a Array of Memory object containing the old and new ones
    allocate( List( size(Memory%SubMemory)+1 ) )                                                         ! Creating a Array of Memory object containing the old and new ones
    do i = 1,size(List)-1
      List(i)   =   Memory%SubMemory(i)
    end do
    List(size(List))   =   Mem2
  else
!     allocate( List, source = [Mem2] )                                                         ! Creating a Array of Memory object containing the old and new ones
    allocate( List(1) )                                                         ! Creating a Array of Memory object containing the old and new ones
    List(size(List))   =   Mem2
  end if
  if ( associated(Memory%SubMemory) ) deallocate( Memory%SubMemory )
!   allocate( Memory%SubMemory, source = List )         ! @COMPILER_BUG                              ! Reallocating the SubMemory component to the new array
  allocate( Memory%SubMemory( size(List) ) )         ! @COMPILER_BUG                              ! Reallocating the SubMemory component to the new array
  do i = 1,size(List)
    Memory%SubMemory(i)     =       List(i)
  end do
! **********************************************************************************************************************
!   if (i_Debug_Loc) call Logger%Write( "-> That%Bits = ", That%Bits )

!   if (i_Debug_Loc) call Logger%Write( "Calling Memory%Add_Value( That%Bits )" )
  call Memory%Add_Value( That%Bits )                                                                           ! Adding the memory storage size of the input Universal variable 'Var' to the memory of the passed-object dummy argument

!   if (i_Debug_Loc) then
!     call Logger%Write( "On exit: Calling Memory%OutputToLogger" )
!     call Memory%OutputToLogger()
!   end if

!   if (i_Debug_Loc) call Logger%Exiting()

End Procedure

Module Procedure  Addition_Memory_1d
  type(Memory_Type)                                                     ::  SumThat                         ! Temporary Memory object containing the sum of the memory storage of all elements of the input array of Memory object
  Res   =       This
  call SumThat%Sum( That )      ! SHOULD BE THE SAME THAN SumThat = That                                                                  ! Sum
  call Res%Add_SubMemory( SumThat )                                                                          ! Add an additional element to the SubMemory component of the Memory object of the passed-object dummy argument
  call Res%Add_Value( SumThat%Bits )                                                                              ! Adding the memory storage size of the input Universal variable 'Var' to the memory of the passed-object dummy argument
End Procedure

! Elemental
Module Procedure Assign_To_Memory_0d
  integer                                                               ::  i
!   character(*)                                              ,parameter  ::  ProcName = "Assign_To_Memory_0d" ! Name of current procedure
!   if (i_Debug_Loc) call Logger%Entering( ProcName )
!   if (i_Debug_Loc) call Logger%Write( "rhs%Type_Name = ", rhs%Type_Name )
  call lhs%Free()
  if ( allocated(rhs%Var_Name ) ) lhs%Var_Name    =   rhs%Var_Name
  if ( allocated(rhs%Type_Name) ) lhs%Type_Name   =   rhs%Type_Name
  lhs%Bits   =       rhs%Bits
  lhs%Level   =       rhs%Level
!   if (i_Debug_Loc) call Logger%Write( "associated(rhs%SubMemory) = ", associated(rhs%SubMemory) )
  if ( associated(rhs%SubMemory) ) then
    deallocate( lhs%SubMemory )  ! Always allocated from the 'Initialize' procedure
!     if (i_Debug_Loc) call Logger%Write( "size(rhs%SubMemory) = ", size(rhs%SubMemory) )
    allocate( lhs%SubMemory( size(rhs%SubMemory) ) )
    do i = 1,size(rhs%SubMemory)
      lhs%SubMemory(i)  =       rhs%SubMemory(i)
!       if (i_Debug_Loc) call Logger%Write( "lhs%SubMemory(i)%Type_Name = ", lhs%SubMemory(i)%Type_Name )
    end do
  end if
!   if (i_Debug_Loc) call Logger%Exiting()
End Procedure


! This procedure copy a array of Memory objects into a scalar Memory object.
! The output scalar Memory object has the following properties:
!  - Type_Name:
!    Same than the one of the 1st element of the input Memory array.
!    This is ok since all elements of the input array of Memory object have the same type.
!  - Name:
!    Same than the one of the 1st element of the input Memory array.
!    This is ok since all elements of the input array of Memory object have the same 'Name' component.
!  - Value:
!    Its memory storage value (in bits) corresponds to the sum of the memory storage value of all
!    elements of the input Memory array. me is the name of the first element of the input Memory array.
!  - Level:
!    Same than the one of the 1st element of the input Memory array.
!    This is ok since all elements of the input array of Memory object have the same 'Level' component.
!  - SubMemory:
!    Empty zero dimensional array.
! Since the all the value component is sum there is no lost in the global memory nstorage associated to the
! array of Memory objects, only the details are lost since the SubMemory component is not saved.


Module Procedure  Sum_Memory_1d
  call This%Free()
  if ( size(Memory) >= 1 ) then
  associate( Mem1 => Memory(1) )
    This%Var_Name       =       Mem1%Var_Name                                                          ! Setting the 'Var_Name' component as the one of the 1st element
    This%Type_Name      =       Mem1%Type_Name                                                         ! Setting the 'Type_Name' component as the one of the 1st element
    This%Bits           =       sum( Memory%Bits )                                                    ! Setting the 'Value' component as the sum of the value of all elements
    This%Level          =       Mem1%Level                                                             ! Setting the 'Level' component as the one of the 1st element
    if ( associated( This%SubMemory ) ) nullify( This%SubMemory )
    allocate( This%SubMemory(0) )                                                                       ! Setting the 'SubMemory' component to an SubMemoryempty zero dimensional array
  end associate
  end if
End Procedure

Module Procedure Sum_Memory_2d
  call This%Free()
  if ( size(Memory) >= 1 ) then
  associate( Mem1 => Memory(1,1) )
    This%Var_Name        =       Mem1%Var_Name                                                          ! Setting the 'Var_Name' component as the one of the 1st element
    This%Type_Name       =       Mem1%Type_Name                                                         ! Setting the 'Type_Name' component as the one of the 1st element
    This%Bits           =       sum( Memory%Bits )                                                    ! Setting the 'Value' component as the sum of the value of all elements
    This%Level           =       Mem1%Level                                                             ! Setting the 'Level' component as the one of the 1st element
    if ( associated( This%SubMemory ) ) nullify( This%SubMemory )
    allocate( This%SubMemory(0) )                                                                       ! Setting the 'SubMemory' component to an SubMemoryempty zero dimensional array
  end associate
  end if
End Procedure

Module Procedure Sum_Memory_3d
  call This%Free()
  if ( size(Memory) >= 1 ) then
  associate( Mem1 => Memory(1,1,1) )
    This%Var_Name        =       Mem1%Var_Name                                                          ! Setting the 'Var_Name' component as the one of the 1st element
    This%Type_Name       =       Mem1%Type_Name                                                         ! Setting the 'Type_Name' component as the one of the 1st element
    This%Bits           =       sum( Memory%Bits )                                                    ! Setting the 'Value' component as the sum of the value of all elements
    This%Level           =       Mem1%Level                                                             ! Setting the 'Level' component as the one of the 1st element
    if ( associated( This%SubMemory ) ) nullify( This%SubMemory )
    allocate( This%SubMemory(0) )                                                                       ! Setting the 'SubMemory' component to an SubMemoryempty zero dimensional array
  end associate
  end if
End Procedure

Module Procedure Sum_Memory_4d
  call This%Free()
  if ( size(Memory) >= 1 ) then
  associate( Mem1 => Memory(1,1,1,1) )
    This%Var_Name        =       Mem1%Var_Name                                                          ! Setting the 'Var_Name' component as the one of the 1st element
    This%Type_Name       =       Mem1%Type_Name                                                         ! Setting the 'Type_Name' component as the one of the 1st element
    This%Bits           =       sum( Memory%Bits )                                                    ! Setting the 'Value' component as the sum of the value of all elements
    This%Level           =       Mem1%Level                                                             ! Setting the 'Level' component as the one of the 1st element
    if ( associated( This%SubMemory ) ) nullify( This%SubMemory )
    allocate( This%SubMemory(0) )                                                                       ! Setting the 'SubMemory' component to an SubMemoryempty zero dimensional array
  end associate
  end if
End Procedure


Module Procedure Write_Memory
  integer                                                               ::  NRow, NCol
  integer                                                               ::  iRow, iCol, i
  type(Table_Type)                                                      ::  Table
  NRow          =       1 + size(This%SubMemory)
  NCol          =       8
  call Table%Initialize( NRow, NCol )
  iRow          =       1
  call Set_Table_Row( This, Table, iRow )
  do i = 1,size(This%SubMemory)
    iRow        =       iRow + 1
    call Set_Table_Row( This%SubMemory(i), Table, iRow, Total_Memory=This%Bits )
  end do
  do iCol = 1,Table%NCol
!     call Table%Cells(:,iCol)%Set_Same_Length()
    call Table%SetSameCellRowLength( iCol )
  end do
!   call Table%Cells(:,3)%Adjust_Right()
  call Table%AdjustToRightColumn( 3 )
  write(Unit,*)
  do iRow = 1,Table%NRow
    write(Unit,"(*(a,1x))")     ( Table%Cells(iRow,iCol)%Value, iCol = 1,Table%NCol )
  end do
  write(Unit,*)
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
!                             PROCEDURES TO ADD THE STORAGE SIZE OF A GIVEN VARIABLE
! **************************************************************************************************************
! **************************************************************************************************************

Module Procedure Add_Storage_Size_Var_0d
  select type (Variable)
    type is (Memory_Type);  call This%Add_SubMemory( Variable )                                                 ! Add an additional element to the SubMemory component of the Memory object of the passed-object dummy argument
  end select
  This%Bits      =       This%Bits + This%Get_Variable_Storage_Size( Variable )                               ! Getting the storage size of the input variable and adding it to the storage memory
End Procedure

Module Procedure Add_Storage_Size_Var_1d
  type(Memory_Type)                                                     ::  Memory                          ! Temporary Memory object containing the sum of the memory storage of all elements of the input array of Memory object
  select type (Variable)
  type is (Memory_Type)
    call Memory%Sum( Variable )                                                                                   ! Sum
    call This%Add_Variable( Memory )                                                                              ! Add\
  class default
    This%Bits      =       This%Bits + This%Get_Variable_Storage_Size( Variable )                               ! Getting the storage size of the input variable and adding it to the storage memory
  end select
End Procedure

Module Procedure Add_Storage_Size_Var_2d
  This%Bits      =       This%Bits + This%Get_Variable_Storage_Size( Variable )                               ! Getting the storage size of the input variable and adding it to the storage memory
End Procedure

Module Procedure Add_Storage_Size_Var_3d
  This%Bits      =       This%Bits + This%Get_Variable_Storage_Size( Variable )                               ! Getting the storage size of the input variable and adding it to the storage memory
End Procedure

Module Procedure Add_Storage_Size_Var_4d
  This%Bits      =       This%Bits + This%Get_Variable_Storage_Size( Variable )                               ! Getting the storage size of the input variable and adding it to the storage memory
End Procedure


! **************************************************************************************************************
! **************************************************************************************************************
!                             PROCEDURES TO GET THE STORAGE SIZE OF A GIVEN VARIABLE
! **************************************************************************************************************
! **************************************************************************************************************

Module Procedure Get_Storage_Size_Var_0d
  integer                                                               ::  NElements                       ! Number of elements contains in the input variable
  NElements   =     1                                                                                           ! Getting the number of elements contained in the input variable
  select type (Variable)
    type is (logical);      Memory  =   Storage_Size(Variable) * NElements
    type is (integer);      Memory  =   Storage_Size(Variable) * NElements
    type is (real(4));      Memory  =   Storage_Size(Variable) * NElements
    type is (real(8));      Memory  =   Storage_Size(Variable) * NElements
    type is (character(*)); Memory  =   Storage_Size(Variable) * NElements
    type is (Memory_Type);  Memory  =   Variable%Bits
    class default;          Memory  =   0   !     Error: Unknown type of variable. What should we do here ? For now, set memory to zero
  end select
End Procedure

Module Procedure Get_Storage_Size_Var_1d
  integer                                                               ::  NElements                       ! Number of elements contains in the input variable
  NElements   =     size(Variable)                                                                              ! Getting the number of elements contained in the input variable
  select type (Variable)
    type is (logical);      Memory  =   Storage_Size(Variable) * NElements
    type is (integer);      Memory  =   Storage_Size(Variable) * NElements
    type is (real(4));      Memory  =   Storage_Size(Variable) * NElements
    type is (real(8));      Memory  =   Storage_Size(Variable) * NElements
    type is (character(*)); Memory  =   Storage_Size(Variable) * NElements
    type is (Memory_Type);  Memory  =   sum( Variable%Bits )
    class default;          Memory  =   0   !     Error: Unknown type of variable. What should we do here ? For now, set memory to zero
  end select
End Procedure

Module Procedure Get_Storage_Size_Var_2d
  integer                                                               ::  NElements                       ! Number of elements contains in the input variable
  NElements   =     size(Variable)                                                                              ! Getting the number of elements contained in the input variable
  select type (Variable)
    type is (logical);      Memory  =   Storage_Size(Variable) * NElements
    type is (integer);      Memory  =   Storage_Size(Variable) * NElements
    type is (real(4));      Memory  =   Storage_Size(Variable) * NElements
    type is (real(8));      Memory  =   Storage_Size(Variable) * NElements
    type is (character(*)); Memory  =   Storage_Size(Variable) * NElements
    type is (Memory_Type);  Memory  =   sum( Variable%Bits )
    class default;          Memory  =   0   !     Error: Unknown type of variable. What should we do here ? For now, set memory to zero
  end select
End Procedure

Module Procedure Get_Storage_Size_Var_3d
  integer                                                               ::  NElements                       ! Number of elements contains in the input variable
  NElements   =     size(Variable)                                                                              ! Getting the number of elements contained in the input variable
  select type (Variable)
    type is (logical);      Memory  =   Storage_Size(Variable) * NElements
    type is (integer);      Memory  =   Storage_Size(Variable) * NElements
    type is (real(4));      Memory  =   Storage_Size(Variable) * NElements
    type is (real(8));      Memory  =   Storage_Size(Variable) * NElements
    type is (character(*)); Memory  =   Storage_Size(Variable) * NElements
    type is (Memory_Type);  Memory  =   sum( Variable%Bits )
    class default;          Memory  =   0   !     Error: Unknown type of variable. What should we do here ? For now, set memory to zero
  end select
End Procedure

Module Procedure Get_Storage_Size_Var_4d
  integer                                                               ::  NElements                       ! Number of elements contains in the input variable
  NElements   =     size(Variable)                                                                              ! Getting the number of elements contained in the input variable
  select type (Variable)
    type is (logical);      Memory  =   Storage_Size(Variable) * NElements
    type is (integer);      Memory  =   Storage_Size(Variable) * NElements
    type is (real(4));      Memory  =   Storage_Size(Variable) * NElements
    type is (real(8));      Memory  =   Storage_Size(Variable) * NElements
    type is (character(*)); Memory  =   Storage_Size(Variable) * NElements
    type is (Memory_Type);  Memory  =   sum( Variable%Bits )
    class default;          Memory  =   0   !     Error: Unknown type of variable. What should we do here ? For now, set memory to zero
  end select
End Procedure


    ! **************************************************************************************************************
    ! **************************************************************************************************************
    !                                                  PRIVATE PROCEDURES
    ! **************************************************************************************************************
    ! **************************************************************************************************************

Subroutine Set_Table_Row( This, Table, iRow, Total_Memory )
  use String_Library          ,only:  Convert_To_String
  use Memory_Utilities_Module ,only:  ConvertBitsToXOctet
  type(Memory_Type)                                     ,intent(in)     ::  This                    !< Passed-object dummy argument
  type(Table_Type)                                      ,intent(inout)  ::  Table
  integer                                               ,intent(in)     ::  iRow
  integer                                     ,optional ,intent(in)     ::  Total_Memory
  character(:)  ,allocatable                                              ::  String, MemoryValue, MemoryUnit, Percentage
  integer                                                                 ::  Total_Memory_
  Total_Memory_   =   This%Bits
  if ( present(Total_Memory) ) Total_Memory_ = Total_Memory
  call ConvertBitsToXOctet( This%Bits, CharValue=MemoryValue, Units=MemoryUnit, Fmt="(f6.2)" )
  if ( Total_Memory_ == 0 ) Total_Memory_ = 1
  Percentage    =     Convert_To_String( This%Bits * 100 / Total_Memory_, Fmt="g0" )
  String        =     repeat("  ",This%Level) // This%Var_Name // ":"
  call Table%SetCellValue( iRow, 1, String )
  call Table%SetCellValue( iRow, 2, This%Type_Name )
  call Table%SetCellValue( iRow, 3, Convert_To_String(This%Bits) )
  call Table%SetCellValue( iRow, 4, "bits" )
  call Table%SetCellValue( iRow, 5, MemoryValue )
  call Table%SetCellValue( iRow, 6, MemoryUnit )
  call Table%SetCellValue( iRow, 7, Percentage )
  call Table%SetCellValue( iRow, 8, "%" )
End Subroutine

End SubModule