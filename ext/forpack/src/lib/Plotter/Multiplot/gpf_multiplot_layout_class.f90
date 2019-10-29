Module GPF_Multiplot_Layout_Class

  use GPF_Parameters            ,only:  DbgUnit, rkp
  use GPF_Command_Class            ,only:  GPF_Command_Type

  implicit none

  private

  public  ::  GPF_Multiplot_Layout_Type

  Type  ,extends(GPF_Command_Type) ::  GPF_Multiplot_Layout_Type
    private
    integer                                     ::  NRows                                                   !< Number of rows
    integer                                     ::  NCols                                                   !< Number of rows
    character(:)        ,allocatable            ::  Dimensions                                              !< Dimensions of the Multiplot-Layout
    character(:)        ,allocatable            ::  Scales                                                  !< Scales of the Multiplot-Layout
    character(:)        ,allocatable            ::  Offsets                                                 !< Offsets of the Multiplot-Layout
  contains
    private
    procedure   ,public   ::  Get_Command     =>  Get_Multiplot_Title_Command                     !< Gets the Multiplot-Layout command string
    procedure             ::  Set_Dimensions  =>  Set_Multiplot_Layout_Dimensions                 !< Sets the Multiplot-Layout dimensions
    procedure             ::  Set_Scales      =>  Set_Multiplot_Layout_Scales                     !< Sets the Multiplot-Layout scales
    procedure             ::  Set_Offsets     =>  Set_Multiplot_Layout_Offsets                    !< Sets the Multiplot-Layout offsets
    procedure   ,public   ::  Set_Command     =>  Set_Multiplot_Layout_Command                     !< Sets the Multiplot-Layout command string
  End Type

  Interface             GPF_Multiplot_Layout_Type
    Module Procedure    Construct_Multiplot_Layout
  End Interface

! @TODO: The following option of the Multiplot$-Layout object still need to be implemented: {rowsfirst|columnsfirst} {downwards|upwards}

! REMARK:
! The gnuplot syntax is the following:
!       set multiplot
!         { title <page title> {font <fontspec>} {enhanced|noenhanced} }
!         { layout <rows>,<cols>
!           {rowsfirst|columnsfirst} {downwards|upwards}
!           {scale <xscale>{,<yscale>}} {offset <xoff>{,<yoff>}}
!         }
!       unset multiplot

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         CONSTRUCTOR PROCEDURES                                             *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Multiplot_Layout( Debug,                                             &
                Multiplot_NRows, Multiplot_NCols,                                       &                       ! Arguments related to the Multiplot layout dimensions
                Multiplot_Xscale, Multiplot_Yscale,                                     &                       ! Arguments related to the Multiplot layout scales
                Multiplot_Xoffset, Multiplot_Yoffset,                                   &                       ! Arguments related to the Multiplot layout offsets
                Multiplot_NGraphs )                                                     &
                result(This)

  implicit none

  type(GPF_Multiplot_Layout_Type)                                       ::  This                            !< Multiplot-Layout object to be constructed
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
  integer                                     ,optional ,intent(in)     ::  Multiplot_NRows                 !< Number of rows for the muliplot layout
  integer                                     ,optional ,intent(in)     ::  Multiplot_NCols                 !< Number of colums for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Xscale                !< Scale along X for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Yscale                !< Scale along Y for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Xoffset               !< Offset along X for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Yoffset               !< Offset along Y for the muliplot layout
  integer                                     ,optional ,intent(in)     ::  Multiplot_NGraphs               !< Number of Graphs

  character(*)                                              ,parameter  ::  Keyword='layout'                !< Keyword of current command

  call This%Set_Debug( Debug )                                                                                  ! Setting debugging indicator
  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Layout]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Layout]: Calling This%Set_Dimensions')")
  call This%Set_Dimensions( Multiplot_NRows, Multiplot_NCols, Multiplot_NGraphs )                               ! Setting the Multiplot-Layout dimensions

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Layout]: Calling This%Set_Scales')")
  call This%Set_Scales( Multiplot_Xscale, Multiplot_Yscale )                                                    ! Setting the Multiplot-Layout scales

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Layout]: Calling This%Set_Offsets')")
  call This%Set_Offsets( Multiplot_Xoffset, Multiplot_Yoffset )                                                 ! Setting the Multiplot-Layout offsets

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Layout]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Layout]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the Multiplot-Layout command string

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Layout]: Exiting',/)")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Function Get_Multiplot_Title_Command( This ) result(Command)
  implicit none
  class(GPF_Multiplot_Layout_Type)                      ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Multiplot-Layout object
  character(:)  ,allocatable                                            ::  Command                         !< Command string
  Command       =       This%Command                                                                            ! Getting Command
End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Set_Multiplot_Layout_Dimensions( This, Multiplot_NRows, Multiplot_NCols, Multiplot_NGraphs )
  use GPF_Tools                 ,only:  Convert_To_String
  implicit none
  class(GPF_Multiplot_Layout_Type)                      ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Mutliplot-Layout object
  integer                                     ,optional ,intent(in)     ::  Multiplot_NRows                 !< Number of rows for the muliplot layout
  integer                                     ,optional ,intent(in)     ::  Multiplot_NCols                 !< Number of colums for the muliplot layout
  integer                                     ,optional ,intent(in)     ::  Multiplot_NGraphs               !< Number of Graphs
  character(*)                                              ,parameter  ::  Separator=","                   ! Seprator character between each value
  if (This%i_Debug) write(DbgUnit,"(12x,'[This%Set_Multiplot_Layout_Dimensions]: Entering')")
  This%NRows            =       0
  This%NCols            =       0
  This%Dimensions       =       ""
  if ( present(Multiplot_NRows) ) This%NRows = Multiplot_NRows
  if ( present(Multiplot_NCols) ) This%NCols = Multiplot_NCols
  if ( present(Multiplot_NRows) .or. present(Multiplot_NCols) ) This%Dimensions = Convert_To_String(This%NRows) // Separator // Convert_To_String(This%NCols) // " "

  if ( present(Multiplot_NGraphs) ) then
    if (This%i_Debug) write(DbgUnit,"(12x,'[This%Set_Multiplot_Layout_Dimensions]: Forcing the Layout')")
    This%NRows          =       Multiplot_NGraphs
    This%NCols          =       1
    This%Dimensions     =       Convert_To_String(This%NRows) // Separator // Convert_To_String(This%NCols) // " "
  end if

  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[This%Set_Multiplot_Layout_Dimensions]: This%NRows      = ',i0)") This%NRows
    write(DbgUnit,"(12x,'[This%Set_Multiplot_Layout_Dimensions]: This%NCols      = ',i0)") This%NCols
    write(DbgUnit,"(12x,'[This%Set_Multiplot_Layout_Dimensions]: This%Dimensions = ',a)")  This%Dimensions
    write(DbgUnit,"(12x,'[This%Set_Multiplot_Layout_Dimensions]: Exiting')")
  end if
End Subroutine

Subroutine Set_Multiplot_Layout_Scales( This, Multiplot_Xscale, Multiplot_Yscale )
  implicit none
  class(GPF_Multiplot_Layout_Type)                      ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Mutliplot-Layout object
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Xscale                !< Scale along X for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Yscale                !< Scale along Y for the muliplot layout
  character(*)                                              ,parameter  ::  Prefix='scale '                 ! Prefix string
  if (This%i_Debug) write(DbgUnit,"(12x,'[This%Set_Multiplot_Layout_Scales]: Entering')")
  if ( present(Multiplot_Xscale) ) then
    This%Scales =       This%Command // "???"
    if ( present(Multiplot_Yscale) ) This%Scales = This%Command // "???"
  end if
!   Length        =       floor(log10(real(maxval(Num)))) + 1                                                     ! Setting the maximum length

  if (This%i_Debug) write(DbgUnit,"(12x,'[This%Set_Multiplot_Layout_Scales]: Exiting')")
End Subroutine

Subroutine Set_Multiplot_Layout_Offsets( This, Multiplot_Xoffset, Multiplot_Yoffset )
  implicit none
  class(GPF_Multiplot_Layout_Type)                      ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Mutliplot-Layout object
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Xoffset               !< Offset along X for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Yoffset               !< Offset along Y for the muliplot layout
  character(*)                                              ,parameter  ::  Prefix='offset '                ! Prefix string
  if (This%i_Debug) write(DbgUnit,"(12x,'[This%Set_Multiplot_Layout_Offsets]: Entering')")
  if ( present(Multiplot_Xoffset) ) then
    This%Offsets =       This%Command // "???"
    if ( present(Multiplot_Yoffset) ) This%Offsets = This%Command // "???"
  end if
!   Length        =       floor(log10(real(maxval(Num)))) + 1

  if (This%i_Debug) write(DbgUnit,"(12x,'[This%Set_Multiplot_Layout_Offsets]: Exiting')")
End Subroutine


Subroutine Set_Multiplot_Layout_Command( This )
  implicit none
  class(GPF_Multiplot_Layout_Type)                      ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Multiplot-Layout object

  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Multiplot_Layout_Command]: Entering')")

  This%Command  =       ""
  if ( len_trim(This%Dimensions) /= 0 ) This%Command = This%Command // This%Dimensions
  if ( len_trim(This%Scales)     /= 0 ) This%Command = This%Command // This%Scales
  if ( len_trim(This%Offsets)    /= 0 ) This%Command = This%Command // This%Offsets

  This%Presence =       .False.
  if ( len_trim(This%Command) /= 0 ) then
    This%Presence       =       .True.
    This%Command        =       This%Keyword // ' ' // This%Command
  end if

  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Multiplot_Layout_Command]: This%Command = ',a)") This%Command
    write(DbgUnit,"(12x,'[Set_Multiplot_Layout_Command]: Exiting',/)")
  end if

end Subroutine

End Module