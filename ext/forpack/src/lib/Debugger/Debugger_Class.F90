Module Debugger_Class

  use DebuggerItem_Class            ,only:  DebuggerItem_Type
  use DebuggerComponentsList_Class  ,only:  DebuggerComponentsList_Type
  use DebugVariable_Class           ,only:  DebugVariable_Type
  use Input_Library                 ,only:  InputSection_Type
  use Logger_Class                  ,only:  Logger_Type

  implicit none

  private
  public  ::  Debugger_Type
  public  ::  Debugger

  Type  ,extends(Logger_Type)  ::  Debugger_Type
!   Type  ::  Debugger_Type
!     private
!     logical                               ::  Activated = .False.   ! Already in Logger
!     integer                               ::  NItems = 0            ! Already in Logger
    type(DebuggerItem_Type) ,allocatable  ::  DbgItems(:)
    type(DebuggerComponentsList_Type)     ::  Components
  contains
    private
    procedure ,public ::  Init              =>  InitializeDebugger
    procedure ,public ::  AddItem           =>  AddDebuggerItem
    procedure ,public ::  Output            =>  OutputDebugger
    procedure ,public ::  SetConditions     =>  SetDebuggerConditions
    procedure ,public ::  RemoveItem        =>  RemoveDebuggerItem
    generic   ,public ::  UpdateStatus      =>  UpdateDebuggerStatus
    generic   ,public ::  GetItemIndex      =>  GetDebuggerItemIndexFromItemName
    generic   ,public ::  ConditionsOk      =>  ConditionsSatisfied_All
    generic   ,public ::  GetIntVarConditions  =>  GetDebuggerIntVarConditionsFromLabelName
    generic   ,public ::  GetVarConditions  =>  GetDebuggerVarConditions
    procedure         ::  UpdateDebuggerStatus
    procedure         ::  GetDebuggerItemIndexFromItemName
    procedure         ::  ConditionsSatisfied_All
    procedure         ::  GetDebuggerIntVarConditionsFromLabelName
    procedure         ::  GetDebuggerVarConditions

!     Override the procedure from the Logger so that Debugger%Write() will only do something
!     ogf the Debugger is actually active
    procedure ,public ::  LoggerActive => LoggerActive
    generic   ,public ::  On => ActiveDebugger
    procedure         ::  ActiveDebugger

!     procedure         ::  FindComponentIndex
    procedure ,public ::  AddComponent      =>  AddComponentToDebuggerFromName
    procedure ,public ::  RemoveComponent   =>  RemoveComponentFromDebuggerFromName
    procedure ,public ::  ActivateComponent
    procedure ,public ::  DeactivateComponent
    procedure ,public ::  GetComponentList

  End Type

  type(Debugger_Type)     ::  Debugger


  Interface

    Module Subroutine InitializeDebugger( This, Input, LogLevel )
      class(Debugger_Type)                                  ,intent(out)    ::  This
      class(InputSection_Type)                              ,intent(in)     ::  Input
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Subroutine AddDebuggerItem( This, Var, Vars, Components, LogLevel )
      class(Debugger_Type)                                  ,intent(inout)  ::  This
      type(DebugVariable_Type)                    ,optional ,intent(in)     ::  Var
      type(DebugVariable_Type)                    ,optional ,intent(in)     ::  Vars(:)
      character(*)                                ,optional ,intent(in)     ::  Components(:)
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine


    Module Function ActiveDebugger( This, Label ) result(Active)
      class(Debugger_Type)                                  ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Label
      logical                                                               ::  Active
    End Function

    Module Function LoggerActive( This, LogLevel ) result(IsActive)
      class(Debugger_Type)                                  ,intent(in)     ::  This                                !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  LogLevel  ! Never used
      logical                                                               ::  IsActive
    End Function
!
    Module Subroutine OutputDebugger( This )
      class(Debugger_Type)                                  ,intent(in)     ::  This
    End Subroutine

    Module Subroutine SetDebuggerConditions( This, Label, Var, Vars, LogLevel )
      class(Debugger_Type)                                  ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Label
      type(DebugVariable_Type)                    ,optional ,intent(in)     ::  Var
      type(DebugVariable_Type)                    ,optional ,intent(in)     ::  Vars(:)
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Subroutine RemoveDebuggerItem( This, Label )
      class(Debugger_Type)                                  ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Label
    End Subroutine

    Module Subroutine UpdateDebuggerStatus( This, Name, Value, Component )
      class(Debugger_Type)                                  ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Name
      class(*)                                              ,intent(in)     ::  Value
      character(*)                                ,optional ,intent(in)     ::  Component
    End Subroutine

    Module Function ConditionsSatisfied_All( This, Name, Value, Label ) result(Ok)
      class(Debugger_Type)                                  ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Name
      class(*)                                              ,intent(in)     ::  Value
      character(*)                                ,optional ,intent(in)     ::  Label
      logical                                                               ::  Ok
    End Function

    Pure Module Function GetDebuggerItemIndexFromItemName( This, Label ) result(iItem)
      class(Debugger_Type)                                  ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Label
      integer                                                               ::  iItem
    End Function

    Module Function GetDebuggerIntVarConditionsFromLabelName( This, Label, VarName ) result(VarValues)
      class(Debugger_Type)                                  ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  Label
      character(*)                                          ,intent(in)     ::  VarName
      integer   ,allocatable                                                ::  VarValues(:)
    End Function

    Module Function GetDebuggerVarConditions( This, VarName, Label ) result(Strings)
      class(Debugger_Type)                                  ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  VarName
      character(*)                                ,optional ,intent(in)     ::  Label
      character(:)  ,allocatable                                            ::  Strings(:)
    End Function


    Module Subroutine AddComponentToDebuggerFromName( This, ComponentName )
      class(Debugger_Type)                                  ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  ComponentName
    End Subroutine

    Module Subroutine RemoveComponentFromDebuggerFromName( This, ComponentName )
      class(Debugger_Type)                                  ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  ComponentName
    End Subroutine


!
!     Pure Module Function FindComponentIndex( This, Component ) result(Idx)
!       class(Debugger_Type)                                  ,intent(in)     ::  This
!       character(*)                                          ,intent(in)     ::  Component
!       integer                                                               ::  Idx
!     End Function

    Module Subroutine ActivateComponent( This, ComponentNames )
      class(Debugger_Type)                                  ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  ComponentNames
    End Subroutine

    Module Subroutine DeactivateComponent( This, ComponentNames )
      class(Debugger_Type)                                  ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  ComponentNames
    End Subroutine
!
!     Module Subroutine AddComponemt( This, Name )
!       class(Debugger_Type)                                  ,intent(inout)  ::  This
!       character(*)                                          ,intent(in)     ::  Name
!     End Subroutine

    Pure Module Function GetComponentList( This ) result(ComponentNames)
      class(Debugger_Type)                                  ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  ComponentNames(:)
    End Function




  End Interface

End Module
