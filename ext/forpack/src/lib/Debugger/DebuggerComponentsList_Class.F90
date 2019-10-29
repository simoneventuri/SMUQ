Module DebuggerComponentsList_Class

  implicit none

  private
  public  ::  DebuggerComponentsList_Type

  Type  ::  DebuggerComponent_Type
    character(:)  ,allocatable            ::  Name
    logical                               ::  Active = .False.
  End Type

  Type  ::  DebuggerComponentsList_Type
    integer                                     ::  NItems
    type(DebuggerComponent_Type)  ,allocatable  ::  Items(:)
  contains
    private
    procedure ,public ::  Initialize  =>  InitializeComponentsList
    procedure ,public ::  Find        =>  FindComponentIndex
    procedure ,public ::  Activate    =>  ActivateComponentFromName
    procedure ,public ::  Deactivate  =>  DeactivateComponentFromName
    procedure ,public ::  Add         =>  AddComponemtFromName
    procedure ,public ::  Remove      =>  RemoveComponentFromName
    procedure ,public ::  GetNames    =>  GetComponentNames
    procedure ,public ::  IncludesAny =>  IncludesAnyInComponent
    procedure ,public ::  WriteStatus =>  WriteComponentsStatus
  End Type

  Interface

    Pure Module Subroutine InitializeComponentsList( This )
      class(DebuggerComponentsList_Type)                    ,intent(inout)    ::  This
    End Subroutine

    Pure Module Function FindComponentIndex( This, ComponentName ) result(Idx)
      class(DebuggerComponentsList_Type)                    ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  ComponentName
      integer                                                               ::  Idx
    End Function

    Module Subroutine ActivateComponentFromName( This, ComponentName )
      class(DebuggerComponentsList_Type)                    ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  ComponentName
    End Subroutine
!
    Module Subroutine DeactivateComponentFromName( This, ComponentName )
      class(DebuggerComponentsList_Type)                    ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  ComponentName
    End Subroutine
!
    Module Subroutine AddComponemtFromName( This, ComponentName )
      class(DebuggerComponentsList_Type)                    ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  ComponentName
    End Subroutine
!
    Module Subroutine RemoveComponentFromName( This, ComponentName )
      class(DebuggerComponentsList_Type)                    ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  ComponentName
    End Subroutine


    Pure Module Function GetComponentNames( This, OnlyActive ) result(ComponentNames)
      class(DebuggerComponentsList_Type)                    ,intent(in)     ::  This
      logical                                     ,optional ,intent(in)     ::  OnlyActive
      character(:)  ,allocatable                                            ::  ComponentNames(:)
    End Function

    Pure Module Function IncludesAnyInComponent( This, ComponentNames ) result(Included)
      class(DebuggerComponentsList_Type)                    ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  ComponentNames(:)
      logical                                                               ::  Included
    End Function

    Module Subroutine WriteComponentsStatus( This )
      class(DebuggerComponentsList_Type)                    ,intent(in)     ::  This
    End Subroutine

  End Interface

End Module
