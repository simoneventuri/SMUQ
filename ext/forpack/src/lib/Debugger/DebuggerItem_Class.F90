Module DebuggerItem_Class

  use Logger_Class                  ,only:  Logger_Type
  use DebugVariable_Class           ,only:  DebugVariable_Type
  use DebuggerComponentsList_Class  ,only:  DebuggerComponentsList_Type

  implicit none

  private
  public  ::  DebuggerItem_Type
  public  ::  NewDebuggerItem

  Type  ::  DebuggerItem_Type
!     private
    logical                                 ::  Activated = .False.
    integer                                 ::  iLogUnit  = 1 ! Index of the Logger unit associated to current Debugger Item
!     character(:)              ,allocatable  ::  Label
    character(:)              ,allocatable  ::  IncludedComponentList(:)
    character(:)              ,allocatable  ::  ExcludedComponentList(:)
    type(DebugVariable_Type)  ,allocatable  ::  Var(:)
  contains
    private
    procedure ,public ::  Active            =>  ActiveDebuggerItem
    procedure ,public ::  Output            =>  OutputDebuggerItem
    generic   ,public ::  AssessConditions  =>  AssessDebuggerItemConditionsIntegerVariable, AssessDebuggerItemConditionsCharacterVariable
    procedure ,public ::  GetLabel          =>  GetDebuggerItemLabel
    generic   ,public ::  GetVariableIndex  =>  GetVariableIndexFromVariableName
    generic   ,public ::  AddVar            =>  AddDebugVariableToDebugger, AddDebugVariablesToDebugger
    generic   ,public ::  ConditionsOk      =>  VariableMatchAcceptedValue
    procedure ,public ::  UpdateStatus      =>  UpdateDebuggerVariableStatus
    procedure         ::  GetVariableIndexFromVariableName
    procedure         ::  AssessDebuggerItemConditionsIntegerVariable
    procedure         ::  AssessDebuggerItemConditionsCharacterVariable
    procedure         ::  AddDebugVariableToDebugger
    procedure         ::  AddDebugVariablesToDebugger
    procedure         ::  VariableMatchAcceptedValue
  End Type

  Interface

    Module Function NewDebuggerItem( Var, Vars, Components, LogLevel ) result(This)
      type(DebugVariable_Type)                    ,optional ,intent(in)     ::  Var
      type(DebugVariable_Type)                    ,optional ,intent(in)     ::  Vars(:)
      character(*)                                ,optional ,intent(in)     ::  Components(:)
      integer                                     ,optional ,intent(in)     ::  LogLevel
      type(DebuggerItem_Type)                                               ::  This
    End Function

    Module Function ActiveDebuggerItem( This, DbgLog ) result(Active)
      class(DebuggerItem_Type)                              ,intent(in)     ::  This
      type(Logger_Type)                                     ,intent(in)     ::  DbgLog
!       character(*)                                ,optional ,intent(in)     ::  Label
      logical                                                               ::  Active
    End Function

    Module Subroutine OutputDebuggerItem( This )
      class(DebuggerItem_Type)                              ,intent(in)     ::  This
    End Subroutine

    Module Pure Function GetDebuggerItemLabel( This ) result(Label)
      class(DebuggerItem_Type)                              ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Label
    End Function

    Module Pure Function GetVariableIndexFromVariableName( This, VarName ) result(iVar)
      class(DebuggerItem_Type)                              ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  VarName
      integer                                                               ::  iVar
    End Function

    Module Subroutine AddDebugVariableToDebugger( This, Var )
      class(DebuggerItem_Type)                              ,intent(inout)  ::  This
      type(DebugVariable_Type)                              ,intent(in)     ::  Var
    End Subroutine

    Module Subroutine AddDebugVariablesToDebugger( This, Var )
      class(DebuggerItem_Type)                              ,intent(inout)  ::  This
      type(DebugVariable_Type)                              ,intent(in)     ::  Var(:)
    End Subroutine

    Module Subroutine AssessDebuggerItemConditionsIntegerVariable( This, VarName, VarValue )
      class(DebuggerItem_Type)                              ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  VarName
      integer                                               ,intent(in)     ::  VarValue
    End Subroutine

    Module Subroutine AssessDebuggerItemConditionsCharacterVariable( This, VarName, VarValue )
      class(DebuggerItem_Type)                              ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  VarName
      character(*)                                          ,intent(in)     ::  VarValue
    End Subroutine

    Module Subroutine UpdateDebuggerVariableStatus( This, Name, Value, Components, DbgLog )
      class(DebuggerItem_Type)                              ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Name
      class(*)                                              ,intent(in)     ::  Value
      type(DebuggerComponentsList_Type)                     ,intent(in)     ::  Components
      type(Logger_Type)                                     ,intent(inout)  ::  DbgLog
    End Subroutine


    Module Function VariableMatchAcceptedValue( This, Name, Value ) result(Ok)
      class(DebuggerItem_Type)                              ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Name
      class(*)                                              ,intent(in)     ::  Value
      logical                                                               ::  Ok
    End Function

  End Interface

End Module
