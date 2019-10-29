Module GPF_Grid_Class

  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private
  public  ::  GPF_Grid_Type

! ==============================================================================================================
!   THE GRID COMMAND
! ==============================================================================================================
! The set grid command allows grid lines to be drawn on the plot.
! Syntax:
!       set grid {{no}{m}xtics} {{no}{m}ytics} {{no}{m}ztics}
!                {{no}{m}x2tics} {{no}{m}y2tics}
!                {{no}{m}cbtics}
!                {polar {<angle>}}
!                {layerdefault | front | back}
!                { {linestyle <major_linestyle>}
!                  | {linetype | lt <major_linetype>}
!                    {linewidth | lw <major_linewidth>}
!                  { , {linestyle | ls <minor_linestyle>}
!                      | {linetype | lt <minor_linetype>}
!                        {linewidth | lw <minor_linewidth>} } }
! unset grid
! ==============================================================================================================

  Type  ,extends(GPF_Command_Type) ::  GPF_Grid_Type
    character(:)  ,allocatable  ::  Value
  contains
    private
    procedure   ,public   ::  Initialize    =>  InitializeGrid
    procedure   ,public   ::  Set_Command   =>  SetGridCommand
  End Type

  Interface

    Module Subroutine InitializeGrid( This, Grid_Command, Debug )
      class(GPF_Grid_Type)                                  ,intent(out)    ::  This                            !< Grid object to be constructed
      character(*)                                ,optional ,intent(in)     ::  Grid_Command                    !< Grid command
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator
    End Subroutine

    Module Subroutine SetGridCommand( This )
      class(GPF_Grid_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Grid object
    End Subroutine

  End Interface

End Module