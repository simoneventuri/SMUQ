Module GPF_Pm3D_Class

  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private

  public  ::  GPF_Pm3D_Type


! ==============================================================================================================
!   THE PM3D COMMAND
! ==============================================================================================================
! pm3d is an splot style for drawing palette-mapped 3d and 4d data as color/gray maps and surfaces.
! It uses an algorithm that allows plotting gridded as well as non-gridded data without preprocessing,
! even when the data scans do not have the same number of points.
! Syntax (the options can be given in any order):
!   set pm3d {
!              { at <position> }
!              { interpolate <steps/points in scan, between scans> }
!              { scansautomatic | scansforward | scansbackward | depthorder }
!              { flush { begin | center | end } }
!              { ftriangles | noftriangles }
!              { clip1in | clip4in }
!              { corners2color { mean|geomean|median|min|max|c1|c2|c3|c4 } }
!              { hidden3d {<linestyle>} | nohidden3d }
!              { implicit | explicit }
!              { map }
!            }
!   unset pm3d
! ==============================================================================================================

!   Type  ,extends(GPF_Command_Type) ::  GPF_Pm3D_Position_Type
!     character(:)        ,allocatable                            ::  Command                                 !< Plot command
!   End Type
!
!   Type  ,extends(GPF_Command_Type) ::  GPF_Pm3D_Interpolate_Type
!     character(:)        ,allocatable                            ::  Command                                 !< Plot command
!   End Type
!
!   Type  ,extends(GPF_Command_Type) ::  GPF_Pm3D_Scan_Type
!     character(:)        ,allocatable                            ::  Command                                 !< Plot command
!   End Type
!
!   Type  ,extends(GPF_Command_Type) ::  GPF_Pm3D_Flush_Type
!     character(:)        ,allocatable                            ::  Command                                 !< Plot command
!   End Type
!
!   Type  ,extends(GPF_Command_Type) ::  GPF_Pm3D_Tria_Type
!     character(:)        ,allocatable                            ::  Command                                 !< Plot command
!   End Type
!
!   Type  ,extends(GPF_Command_Type) ::  GPF_Pm3D_Clip_Type
!     character(:)        ,allocatable                            ::  Command                                 !< Plot command
!   End Type
!
!   Type  ,extends(GPF_Command_Type) ::  GPF_Pm3D_Corners3Color_Type
!     character(:)        ,allocatable                            ::  Command                                 !< Plot command
!   End Type
!
!   Type  ,extends(GPF_Command_Type) ::  GPF_Pm3D_Hidden3D_Type
!     character(:)        ,allocatable                            ::  Command                                 !< Plot command
!   End Type

  Type  ,extends(GPF_Command_Type)                              ::  GPF_Pm3D_Type
    character(:)        ,allocatable                            ::  Value                                   !< Value of the pm3d command
  contains
    private
    procedure             ::  Set_Value       =>  Set_Pm3D_Value                                  !< Sets the pm3d value
    procedure   ,public   ::  Set_Command     =>  Set_Pm3D_Command                                !< Sets the pm3d command
  End Type

  Interface             GPF_Pm3D_Type
    Module Procedure    Construct_Pm3D
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Pm3D( Plot_Function, Value, Debug ) result(This)

  implicit none

  type(GPF_Pm3D_Type)                                                   ::  This                            !< Pm3D object to be constructed
  character(*)                                          ,intent(in)     ::  Plot_Function                   !< Plotting function
  character(*)                                ,optional ,intent(in)     ::  Value                           !< Value of the pm3d command
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  character(*)                                              ,parameter  ::  Keyword='pm3d'

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Pm3D]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Pm3D]: Calling This%Set_Value')")
  call This%Set_Value( Plot_Function, Value )                                                                   ! Setting the pm3d value

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Pm3D]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Pm3D]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the plot command


!   write(This%Unit_Command,"('  set view 0,0')")
!   write(This%Unit_Command,"('  set size ratio -1')")
!   write(This%Unit_Command,"('  set pm3d map')")




  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Pm3D]: Exiting',/)")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Set_Pm3D_Value( This, Plot_Function, Value )
  use GPF_Parameters            ,only:  KEY_splot
  implicit none
  class(GPF_Pm3D_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Pm3D object
  character(*)                                          ,intent(in)     ::  Plot_Function                   !< Plotting function
  character(*)                                ,optional ,intent(in)     ::  Value                           !< Value of the pm3d command
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Pm3D_Value]: Entering')")
  This%Value    =       ""
  if ( present(Value) ) then
    This%Value  =       Value
  else
    if ( Plot_Function == KEY_splot ) This%Value = "map interpolate 0,0"
  end if
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Pm3D_Value]: This%Value = ',a)") This%Value
    write(DbgUnit,"(12x,'[Set_Pm3D_Value]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Pm3D_Command( This )
  implicit none
  class(GPF_Pm3D_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Pm3D object
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Pm3D_Command]: Entering')")
  This%Command  =       ""
  if ( len_trim(This%Value) /= 0 )      This%Command = This%Command // This%Value
  if ( len_trim(This%Command) /= 0 )    This%Command = 'set ' // This%Keyword // This%Command                   ! Setting the command string
  This%Presence         =       ( len_trim(This%Command) /= 0 )                                                 ! Setting the presence indicator
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Pm3D_Command]: This%Command  = ',a )") This%Command
    write(DbgUnit,"(12x,'[Set_Pm3D_Command]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(12x,'[Set_Pm3D_Command]: Exiting',/)")
  end if
End Subroutine


End Module