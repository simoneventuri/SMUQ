Module DebugVariable_Class

  use Input_Library             ,only:  InputParameter_Type

  implicit none

  private
  public  ::  DebugVariable_Type
  public  ::  NewDebugVariable

  Type  ::  DebugVariable_Type
!     private
    character(:)  ,allocatable    ::  Name
    integer       ,allocatable    ::  iValues(:)
    character(:)  ,allocatable    ::  cValues(:)
    logical                       ::  Active = .False.
  contains
    private
    generic   ,public ::  Initialize  =>  InitializeDebugVariableFromParam
    procedure ,public ::  Output      =>  OutputDebugVariableToString
    procedure ,public ::  GetName     =>  GetDebugVariableName
    procedure         ::  InitializeDebugVariableFromParam
  End Type

  Interface           NewDebugVariable
    Module Procedure  NewDebugVariableFromNameValues
    Module Procedure  NewDebugVariableFromParam
  End Interface

  Interface

    Module Subroutine InitializeDebugVariableFromParam( This, Param )
      class(DebugVariable_Type)                             ,intent(out)    ::  This
      type(InputParameter_Type)                             ,intent(in)     ::  Param
    End Subroutine

    Module Subroutine OutputDebugVariableToString( This, String )
      class(DebugVariable_Type)                             ,intent(in)     ::  This
      character(:)  ,allocatable                            ,intent(out)    ::  String
    End Subroutine

    Module Pure Function GetDebugVariableName( This ) result(Name)
      class(DebugVariable_Type)                             ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Name
    End Function

    Module Function NewDebugVariableFromNameValues( Name, Values ) result(This)
      character(*)                                          ,intent(in)     ::  Name
      integer                                     ,optional ,intent(in)     ::  Values(:)
      type(DebugVariable_Type)                                              ::  This
    End Function

    Module Function NewDebugVariableFromParam( Param ) result(This)
      type(InputParameter_Type)                             ,intent(in)     ::  Param
      type(DebugVariable_Type)                                              ::  This
    End Function

  End Interface

End Module
