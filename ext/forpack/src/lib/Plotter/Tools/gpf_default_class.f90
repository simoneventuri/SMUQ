! #include "preprocessor_variables.inc"

Module GPF_Default_Class

  use GPF_Parameters            ,only:  DbgUnit, rkp
  use GPF_Parameters            ,only:  File_Output_Directory_Default, i_Graph_Grid_Default, Default_LineStyle_Width,   &
                                        i_Debug_Default

  implicit none

  private

  public        GPF_Default_Type

  Type  ::  GPF_Default_Type
    character(:)        ,allocatable    ::  File_Output_Directory                                           !< Name of the output directory where all files should be stored
    logical                             ::  i_Graph_Grid    =       i_Graph_Grid_Default                    !< Graph grid indicator
    character(:)        ,allocatable    ::  View_Map
!     logical                             ::  i_View_Map
    character(:)        ,allocatable    ::  Size_Isometric
!     logical                             ::  i_Size_Isometric
!   Arguments related to the X-Axis object
    real(rkp)                           ::  X_Min                                                           !< X-Axis minimum 1st coordinate
    logical                             ::  i_X_Min         =       .False.
    real(rkp)                           ::  X_Max                                                           !< X-Axis maximum 1st coordinate
    logical                             ::  i_X_Max         =       .False.
    logical                             ::  X_LogScale                                                      !< X-Axis log scale indicator
    logical                             ::  i_X_LogScale    =       .False.
    character(:)        ,allocatable    ::  X_Label                                                         !< X-Axis label
    logical                             ::  i_X_Label       =       .False.
    character(:)        ,allocatable    ::  X_Format                                                        !< X-Axis format
    logical                             ::  i_X_Format      =       .False.
!   Arguments related to the LineStyle object
    character(:)        ,allocatable    ::  LineStyle_Width                                                 !< Line width specified in the LineStyle option
    logical                             ::  i_LineStyle_Width=      .False.

  contains
    procedure   ,public   ::  Reset_Parameters        =>  Reset_Default_Parameters
    procedure   ,public   ::  Set_Parameters          =>  Set_Default_Parameters
    generic     ,private                ::  Set_Component           =>  Set_Logical_Component, Set_Integer_Component, Set_Real_Component, Set_Character_Component
    procedure   ,private        ,nopass ::  Set_Logical_Component
    procedure   ,private        ,nopass ::  Set_Integer_Component
    procedure   ,private        ,nopass ::  Set_Real_Component
    procedure   ,private        ,nopass ::  Set_Character_Component
  End Type

  contains

! This procedures Resets all configurable parameters to default value.
! The parameters that are Reset are:
!  - File_Output_Directory              =>  Name of the output directory where all files should be stored
Subroutine Reset_Default_Parameters( This, i_Debug )

  class(GPF_Default_Type)                               ,intent(out)    ::  This                            !< Passed-object dummy argument corresponding to the Default object
  logical                                     ,optional ,intent(in)     ::  i_Debug                         !< Debugging indicator

  logical                                                               ::  i_Debug_Loc                     ! Local debugging indicator

  i_Debug_Loc   =       i_Debug_Default                                                                         ! Initializing local debugging indicator to the default value
  if ( present(i_Debug) ) i_Debug_Loc = i_Debug                                                                 ! Setting local debugging indicator to the optional input value
  if (i_Debug_Loc) write(DbgUnit,"(8x,'[Reset_Default_Parameters]: Entering')")

  This%File_Output_Directory    =       File_Output_Directory_Default                                           ! Setting to the default value the name of the output directory
  This%i_Graph_Grid             =       i_Graph_Grid_Default                                                    ! Setting to the default value the graph grid indicator
  This%LineStyle_Width          =       Default_LineStyle_Width                                                 ! Setting to the default value the line's width

  This%X_Label                  =       ""
  This%i_X_Label                =       .False.

  This%View_Map                 =       ""

  if (i_Debug_Loc) then
    write(DbgUnit,"(8x,'[Reset_Default_Parameters]: This%File_Output_Directory = ',g0)") This%File_Output_Directory
    write(DbgUnit,"(8x,'[Reset_Default_Parameters]: This%i_Graph_Grid          = ',g0)") This%i_Graph_Grid
    write(DbgUnit,"(8x,'[Reset_Default_Parameters]: This%LineStyle_Width       = ',g0)") This%LineStyle_Width
    write(DbgUnit,"(8x,'[Reset_Default_Parameters]: Exiting')")
  end if

End Subroutine

Subroutine Set_Default_Parameters( This, File_Output_Directory, i_Graph_Grid,           &
                X_Min, X_Max, X_LogScale, X_Label, X_Format,                            &                       ! Arguments related to the X-Axis object
                LineStyle_Width,                                                        &                       ! Arguments related to the LineStyle object
                i_Debug                                                                 )

  class(GPF_Default_Type)                               ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Default object
  character(*)                                ,optional ,intent(in)     ::  File_Output_Directory           !< Name of the output directory
  logical                                     ,optional ,intent(in)     ::  i_Graph_Grid                    !< Graph grid indicator
! Arguments related to the X-Axis object
  real(rkp)                                   ,optional ,intent(in)     ::  X_Min                           !< X-Axis minimum 1st coordinate
  real(rkp)                                   ,optional ,intent(in)     ::  X_Max                           !< X-Axis maximum 1st coordinate
  logical                                     ,optional ,intent(in)     ::  X_LogScale                      !< X-Axis log scale indicator
  character(*)                                ,optional ,intent(in)     ::  X_Label                         !< X-Axis label
  character(*)                                ,optional ,intent(in)     ::  X_Format                        !< X-Axis format
! Arguments related to the LineStyle object
  character(*)                                ,optional ,intent(in)     ::  LineStyle_Width                 !< Line width specified in the LineStyle option
  logical                                     ,optional ,intent(in)     ::  i_Debug                         !< Debugging indicator

  logical                                                               ::  i_Debug_Loc                     ! Local debugging indicator

  i_Debug_Loc   =       i_Debug_Default                                                                         ! Initializing local debugging indicator to the default value
  if ( present(i_Debug) ) i_Debug_Loc = i_Debug                                                                 ! Setting local debugging indicator to the optional input value
  if (i_Debug_Loc) write(DbgUnit,"(8x,'[Set_Default_Parameters]: Entering')")

  if ( present(File_Output_Directory)   ) This%File_Output_Directory    =       File_Output_Directory           ! Setting to the input value (if present) the name of the output directory
  if ( present(i_Graph_Grid)            ) This%i_Graph_Grid             =       i_Graph_Grid                    ! Setting to input value (if present) the graph grid indicator
  if ( present(LineStyle_Width)         ) This%LineStyle_Width          =       LineStyle_Width                 ! Setting to input value (if present) the line's width

  if (i_Debug_Loc) write(DbgUnit,"(8x,'[Set_Default_Parameters]: Setting arguments related to the X-Axis object')")
  call This%Set_Component( This%X_Min,                  This%i_X_Min,                   X_Min           )
  call This%Set_Component( This%X_Max,                  This%i_X_Max,                   X_Max           )
  call This%Set_Component( This%X_LogScale,             This%i_X_LogScale,              X_LogScale      )
  call This%Set_Component( This%X_Label,                This%i_X_Label,                 X_Label         )
  call This%Set_Component( This%X_Format,               This%i_X_Format,                X_Format        )
  if (i_Debug_Loc) write(DbgUnit,"(8x,'[Set_Default_Parameters]: Setting arguments related to the LineStyle object')")
  call This%Set_Component( This%LineStyle_Width,        This%i_LineStyle_Width,         LineStyle_Width )

  if (i_Debug_Loc) then
    write(DbgUnit,"(8x,'[Set_Default_Parameters]: present(File_Output_Directory) = ',l3,3x,'This%File_Output_Directory = ',g0)") present(File_Output_Directory), This%File_Output_Directory
    write(DbgUnit,"(8x,'[Set_Default_Parameters]: present(i_Graph_Grid)          = ',l3,3x,'This%i_Graph_Grid          = ',g0)") present(i_Graph_Grid),          This%i_Graph_Grid
    write(DbgUnit,"(8x,'[Set_Default_Parameters]: present(X_Min)           = ',l1,3x,'This%i_X_Min           = ',l1,3x,'This%X_Min           = ',es15.8)") present(X_Min),          This%i_X_Min,          This%X_Min
    write(DbgUnit,"(8x,'[Set_Default_Parameters]: present(X_Max)           = ',l1,3x,'This%i_X_Max           = ',l1,3x,'This%X_Max           = ',es15.8)") present(X_Max),          This%i_X_Max,          This%X_Max
    write(DbgUnit,"(8x,'[Set_Default_Parameters]: present(X_LogScale)      = ',l1,3x,'This%i_X_LogScale      = ',l1,3x,'This%X_LogScale      = ',g0)") present(X_LogScale),     This%i_X_LogScale,     This%X_LogScale
    write(DbgUnit,"(8x,'[Set_Default_Parameters]: present(X_Label)         = ',l1,3x,'This%i_X_Label         = ',l1,3x,'This%X_Label         = ',g0)") present(X_Label),        This%i_X_Label,        This%X_Label
    write(DbgUnit,"(8x,'[Set_Default_Parameters]: present(X_Format)        = ',l1,3x,'This%i_X_Format        = ',l1,3x,'This%X_Format        = ',g0)") present(X_Format),       This%i_X_Format,       This%X_Format
    write(DbgUnit,"(8x,'[Set_Default_Parameters]: present(LineStyle_Width) = ',l1,3x,'This%i_LineStyle_Width = ',l1,3x,'This%LineStyle_Width = ',g0)") present(LineStyle_Width),This%i_LineStyle_Width,This%LineStyle_Width
    write(DbgUnit,"(8x,'[Set_Default_Parameters]: Exiting')")
  end if

End Subroutine

Subroutine Set_Logical_Component( Variable, Indicator, Value )
  logical                                               ,intent(inout)  ::  Variable
  logical                                               ,intent(inout)  ::  Indicator
  logical                                     ,optional ,intent(in)     ::  Value
  if ( present(Value) ) then
    Variable    =       Value
    Indicator   =       .True.
  end if
End Subroutine

Subroutine Set_Integer_Component( Variable, Indicator, Value )
  integer                                               ,intent(inout)  ::  Variable
  logical                                               ,intent(inout)  ::  Indicator
  integer                                     ,optional ,intent(in)     ::  Value
  if ( present(Value) ) then
    Variable    =       Value
    Indicator   =       .True.
  end if
End Subroutine

Subroutine Set_Real_Component( Variable, Indicator, Value )
  real(rkp)                                             ,intent(inout)  ::  Variable
  logical                                               ,intent(inout)  ::  Indicator
  real(rkp)                                   ,optional ,intent(in)     ::  Value
  if ( present(Value) ) then
    Variable    =       Value
    Indicator   =       .True.
  end if
End Subroutine

Subroutine Set_Character_Component( Variable, Indicator, Value )
  character(:)        ,allocatable                      ,intent(inout)  ::  Variable
  logical                                               ,intent(inout)  ::  Indicator
  character(*)                                ,optional ,intent(in)     ::  Value
  if ( present(Value) ) then
    Variable    =       Value
    Indicator   =       .True.
  end if
End Subroutine

End Module