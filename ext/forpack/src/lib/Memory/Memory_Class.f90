Module Memory_Class

  implicit none

  private
  public        ::  Get_Variable_Storage_Size
  public        ::  Memory_Type, Addition_Memory_0d

  Type  ::  Memory_Type
    character(:)        ,allocatable                    ::  Var_Name                                        !< Name of the object holding the current Memory object
    character(:)        ,allocatable                    ::  Type_Name                                       !< Type of the object holding the current Memory object
    integer                                             ::  Bits    =       0                               !< Memory storage size expressed in bits
    integer                                             ::  Level   =       1                               !< Memory level used for indenting the output
    type(Memory_Type)   ,dimension(:)   ,pointer        ::  SubMemory  ! type(Memory_Type),dimension(:),allocatable :: SubMemory ! error #8541: Not yet implemented: type containing allocatable field of same type.  Use POINTER instead.
  contains
    Final                       ::  FinalizeMemory                                                         ! Finalizes a Memory object
    procedure   ,public         ::  Initialize  =>      InitializeMemory                       !< Initializes a Memory object
    procedure   ,public         ::  Free        =>      FreeMemory                       !< Free a Memory object
    procedure   ,public         ::  GetBits
    procedure   ,public         ::  OutputToLogger
    procedure   ,public         ::  Add_Value               =>      Add_Value_To_Memory                     !< Adds a value (in bits) to a Memory object
    procedure   ,public         ::  Add_SubMemory                                                           !<
    procedure   ,public         ::  Write                   =>      Write_Memory
    procedure   ,public         ::  Set_Variable_Name
    generic     ,public         ::  assignment(=)   =>      Assign_To_Memory_0d
    procedure   ,private        ::  Assign_To_Memory_0d
    generic     ,public         ::  operator(+)   =>      Addition_Memory_0d, Addition_Memory_1d
    procedure   ,private        ::  Addition_Memory_0d
    procedure   ,private        ::  Addition_Memory_1d
    generic     ,public         ::  Sum             =>      Sum_Memory_1d, Sum_Memory_2d, Sum_Memory_3d, Sum_Memory_4d
    procedure   ,private        ::  Sum_Memory_1d
    procedure   ,private        ::  Sum_Memory_2d
    procedure   ,private        ::  Sum_Memory_3d
    procedure   ,private        ::  Sum_Memory_4d
!===> Type-bound procedure for adding the storage size of a given variable to a Memory object
    generic     ,public         ::  Add_Variable            =>      Add_Storage_Size_Var_0d, Add_Storage_Size_Var_1d, &
                                                                        Add_Storage_Size_Var_2d, Add_Storage_Size_Var_3d, &
                                                                        Add_Storage_Size_Var_4d
    procedure ,private          ::  Add_Storage_Size_Var_0d
    procedure ,private          ::  Add_Storage_Size_Var_1d
    procedure ,private          ::  Add_Storage_Size_Var_2d
    procedure ,private          ::  Add_Storage_Size_Var_3d
    procedure ,private          ::  Add_Storage_Size_Var_4d

!===> Type-bound procedure for getting the storage size associated to a given variable
    generic     ,public         ::  Get_Variable_Storage_Size =>    Get_Storage_Size_Var_0d, Get_Storage_Size_Var_1d,   &
                                                                        Get_Storage_Size_Var_2d, Get_Storage_Size_Var_3d,   &
                                                                        Get_Storage_Size_Var_4d
    procedure ,private ,nopass  ::  Get_Storage_Size_Var_0d
    procedure ,private ,nopass  ::  Get_Storage_Size_Var_1d
    procedure ,private ,nopass  ::  Get_Storage_Size_Var_2d
    procedure ,private ,nopass  ::  Get_Storage_Size_Var_3d
    procedure ,private ,nopass  ::  Get_Storage_Size_Var_4d

  End Type

  Interface             Get_Variable_Storage_Size
    Module Procedure    Get_Storage_Size_Var_0d
    Module Procedure    Get_Storage_Size_Var_1d
    Module Procedure    Get_Storage_Size_Var_2d
    Module Procedure    Get_Storage_Size_Var_3d
    Module Procedure    Get_Storage_Size_Var_4d
  End Interface

  Interface
    Pure Elemental Module Subroutine FinalizeMemory( This )
      type(Memory_Type)                                     ,intent(inout)  ::  This                    !< Passed-object dummy argument corresponding to a Memory object to be finalize
    End Subroutine
    Module Subroutine InitializeMemory( This, Type_Name, Var_Name )
      class(Memory_Type)                                    ,intent(out)    ::  This                    !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Type_Name               !< Type of the object holding the current Memory object
      character(*)                                ,optional ,intent(in)     ::  Var_Name                !< Name of the object holding the current Memory object
    End Subroutine
    Pure Elemental Module Subroutine FreeMemory( This )
      class(Memory_Type)                                    ,intent(inout)  ::  This                    !< Passed-object dummy argument
    End Subroutine


    Pure Elemental Module Function GetBits( This ) result(Bits)
      class(Memory_Type)                                    ,intent(in)     ::  This                            !< Passed-object dummy argument
      integer                                                               ::  Bits                            !< Memory value of current object in bits
    End Function

    Module Subroutine OutputToLogger( This )
      class(Memory_Type)                                    ,intent(in)     ::  This                            !< Passed-object dummy argument
    End Subroutine

    Module Subroutine Write_Memory( This, Unit )
      class(Memory_Type)                                    ,intent(in)     ::  This                    !< Passed-object dummy argument
      integer                                               ,intent(in)     ::  Unit                    !< File unit number
    End Subroutine

    Pure Elemental Module Subroutine Set_Variable_Name( This, Var_Name )
      class(Memory_Type)                                    ,intent(inout)  ::  This                    !< Passed-object dummy argument
      character(*)                                          ,intent(in)     ::  Var_Name                !< Name of the object holding the current Memory object
    End Subroutine

    Pure Elemental Module Subroutine Add_Value_To_Memory( This, Value )
      class(Memory_Type)                                    ,intent(inout)  ::  This                    !< Passed-object dummy argument
      integer                                               ,intent(in)     ::  Value                   !< Storage size value is to be added to the one of current object [bite]
    End Subroutine

    Pure Elemental Module Subroutine Add_SubMemory( This, SubMemory )
      class(Memory_Type)                                    ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to a Memory object
      type(Memory_Type)                                     ,intent(in)     ::  SubMemory
    End Subroutine

    Pure Elemental Module Function Addition_Memory_0d( This, That ) result(Memory)
      class(Memory_Type)                                    ,intent(in)     ::  This                            !< Passed-object dummy argument
      type(Memory_Type)                                     ,intent(in)     ::  That                            !< Variable whose storage size is to be added to the one of current object
      type(Memory_Type)                                                     ::  Memory
    End Function

    Pure Module Function Addition_Memory_1d( This, That ) result(Res)
      class(Memory_Type)                                    ,intent(in)     ::  This                            !< Passed-object dummy argument
      type(Memory_Type)     ,dimension(:)                   ,intent(in)     ::  That                            !< Variable whose storage size is to be added to the one of current object
      type(Memory_Type)                                                     ::  Res
    End Function

    Recursive Pure Module Subroutine Assign_To_Memory_0d( lhs, rhs )
      class(Memory_Type)                                    ,intent(inout)  ::  lhs
      type(Memory_Type)                                     ,intent(in)     ::  rhs
    End Subroutine

    Pure Module Subroutine Sum_Memory_1d( This, Memory )
      class(Memory_Type)                                    ,intent(inout)  ::  This                    !< Passed-object dummy argument
      type(Memory_Type)     ,dimension(:)                   ,intent(in)     ::  Memory                  !< Array of Memory object to be summed
    End Subroutine

    Pure Module Subroutine Sum_Memory_2d( This, Memory )
      class(Memory_Type)                                    ,intent(inout)  ::  This                    !< Passed-object dummy argument
      type(Memory_Type)     ,dimension(:,:)                 ,intent(in)     ::  Memory                  !< Array of Memory object to be summed
    End Subroutine

    Pure Module Subroutine Sum_Memory_3d( This, Memory )
      class(Memory_Type)                                    ,intent(inout)  ::  This                    !< Passed-object dummy argument
      type(Memory_Type)     ,dimension(:,:,:)               ,intent(in)     ::  Memory                  !< Array of Memory object to be summed
    End Subroutine

    Pure Module Subroutine Sum_Memory_4d( This, Memory )
      class(Memory_Type)                                     ,intent(inout)  ::  This                    !< Passed-object dummy argument
      type(Memory_Type)     ,dimension(:,:,:,:)              ,intent(in)     ::  Memory                  !< Array of Memory object to be summed
    End Subroutine


    ! **************************************************************************************************************
    !                             PROCEDURES TO ADD THE STORAGE SIZE OF A GIVEN VARIABLE
    ! **************************************************************************************************************
    Pure Module Subroutine Add_Storage_Size_Var_0d( This, Variable )
      class(Memory_Type)                                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      class(*)                                              ,intent(in)     ::  Variable                        !< Variable whose storage size is to be added to the one of current object
    End Subroutine
    Pure Module Subroutine Add_Storage_Size_Var_1d( This, Variable )
      class(Memory_Type)                                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      class(*)              ,dimension(:)                   ,intent(in)     ::  Variable                        !< Variable whose storage size is to be added to the one of current object
    End Subroutine
    Pure Module Subroutine Add_Storage_Size_Var_2d( This, Variable )
      class(Memory_Type)                                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      class(*)              ,dimension(:,:)                 ,intent(in)     ::  Variable                        !< Variable whose storage size is to be added to the one of current object
    End Subroutine
    Pure Module Subroutine Add_Storage_Size_Var_3d( This, Variable )
      class(Memory_Type)                                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      class(*)              ,dimension(:,:,:)               ,intent(in)     ::  Variable                        !< Variable whose storage size is to be added to the one of current object
    End Subroutine
    Pure Module Subroutine Add_Storage_Size_Var_4d( This, Variable )
      class(Memory_Type)                                    ,intent(inout)  ::  This                            !< Passed-object dummy argument
      class(*)              ,dimension(:,:,:,:)             ,intent(in)     ::  Variable                        !< Variable whose storage size is to be added to the one of current object
    End Subroutine

    ! **************************************************************************************************************
    !                             PROCEDURES TO GET THE STORAGE SIZE OF A GIVEN VARIABLE
    ! **************************************************************************************************************
    Pure Module Function Get_Storage_Size_Var_0d( Variable ) result(Memory)
      class(*)                                              ,intent(in)     ::  Variable                        !< Variable whose storage size is to be computed
      integer                                                               ::  Memory                          !< Storage size of the input variable
    End Function
    Pure Module Function Get_Storage_Size_Var_1d( Variable ) result(Memory)
      class(*)              ,dimension(:)                   ,intent(in)     ::  Variable                        !< Variable whose storage size is to be computed
      integer                                                               ::  Memory                          !< Storage size of the input variable
    End Function
    Pure Module Function Get_Storage_Size_Var_2d( Variable ) result(Memory)
      class(*)              ,dimension(:,:)                 ,intent(in)     ::  Variable                        !< Variable whose storage size is to be computed
      integer                                                               ::  Memory                          !< Storage size of the input variable
    End Function
    Pure Module Function Get_Storage_Size_Var_3d( Variable ) result(Memory)
      class(*)              ,dimension(:,:,:)               ,intent(in)     ::  Variable                        !< Variable whose storage size is to be computed
      integer                                                               ::  Memory                          !< Storage size of the input variable
    End Function
    Pure Module Function Get_Storage_Size_Var_4d( Variable ) result(Memory)
      class(*)              ,dimension(:,:,:,:)             ,intent(in)     ::  Variable                        !< Variable whose storage size is to be computed
      integer                                                               ::  Memory                          !< Storage size of the input variable
    End Function

  End Interface

End Module