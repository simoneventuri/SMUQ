Module GPF_Colorspec_Class

  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private

  public  ::  GPF_Colorspec_Type

! @TODO: Create a more complex color object that could identify whether the input color corresponds to "rgbcolor", "palette" or "variable".
! @TODO: Define some nice colors
! @TODO: Create a global object containing some user-defined "colorname"
! @TODO: Add a "Keyword" optional input argument which corresponds to either "lc" or "tc" so that the color command is complet on return of the constructor

! ==============================================================================================================
!   THE COLOR COMMAND
! ==============================================================================================================
! Many commands allow you to specify a linetype with an explicit color. Terminal-independent color choice
! is only possible for terminals that support RGB color or pm3d palettes.
! Syntax:
!   ... {linecolor | lc} {<colorspec> | <n>}
!   ... {textcolor | tc} {<colorspec> | {linetype | lt} <n>}
! where <colorspec> has one of the following forms:
!       rgbcolor "colorname"
!       rgbcolor "#RRGGBB"
!       rgbcolor variable       # color is read from input file
!       palette frac <val>      # <val> runs from 0 to 1
!       palette cb <value>      # <val> lies within cbrange
!       palette z
!       variable                # color index is read from input file
! The "<n>" is the linetype number the color of which is used, see test (p. 172).
! "colorname" refers to one of the color names built in to gnuplot. For a list of the available names, see show
! colornames (p. 105).
! "#RRGGBB" is a hexadecimal constant preceded by the "#" symbol. The RRGGBB represents the red,
! green, and blue components of the color, each on a scale from 0 - 255. For example, magenta = full-scale
! red + full-scale blue would be represented by #FF00FF, which is the hexadecimal representation of (255
! << 16) + (0 << 8) + (255).
! The color palette is a linear gradient of colors that smoothly maps a single numerical value onto a particular
! color. Two such mappings are always in effect. palette frac maps a fractional value between 0 and 1 onto
! the full range of the color palette. palette cb maps the range of the color axis onto the same palette. See
! set cbrange (p. 167). See also set colorbox (p. 104). You can use either of these to select a constant
! color from the current palette.
! "palette z" maps the z value of each plot segment or plot element into the cbrange mapping of the palette.
! This allows smoothly-varying color along a 3d line or surface. It also allows coloring 2D plots by palette
! values read from an extra column of data (not all 2D plot styles allow an extra column).
! ==============================================================================================================

  Type  ,extends(GPF_Command_Type)                      ::  GPF_Colorspec_Type
    private
    character(:)        ,allocatable                    ::  Value                                           !< Color value which corresponds to the entire character string after the color command
  contains
    private
    procedure             ::  Set_Value       =>  Set_Colorspec_Value                     !< Sets the colorspec value
    procedure   ,public   ::  Set_Command     =>  Set_Colorspec_Command                   !< Sets the colorspec command
  End Type

  Interface             GPF_Colorspec_Type
    module procedure    Construct_Colorspec
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         CONSTRUCTOR PROCEDURES                                             *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Colorspec( Keyword, Value, Debug ) result(This)
  implicit none
  type(GPF_Colorspec_Type)                                              ::  This                            !< Colorspec object to be constructed
  character(*)                                ,optional ,intent(in)     ::  Keyword                         !< Keyword associated to the Colorspec command (either empty, "lc" or "tc")
  character(*)                                ,optional ,intent(in)     ::  Value                           !< Colorspec values
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator

  call This%Set_Debug( Debug )                                                                                  ! Setting the debugging indicator
  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Colorspec]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Colorspec]: Calling This%Set_Value')")
  call This%Set_Value( Value )                                                                                  ! Setting the colorspec value

  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Colorspec]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the keyword

  if (This%i_Debug) write(DbgUnit,"(16x,'[Construct_Colorspec]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the command

End Function

! **************************************************************************************************************
! **************************************************************************************************************
!                                       PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************
! @TODO: Write a Check_Colorspec_Validity procedure which checks if a given string corresponds to a color specificaction.

Subroutine Set_Colorspec_Value( This, Value )
  implicit none
  class(GPF_Colorspec_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Colorspec object
  character(*)                                ,optional ,intent(in)     ::  Value                           !< Colorspec value
  if (This%i_Debug) write(DbgUnit,"(18x,'[Set_Colorspec_Value]: Entering')")
  This%Value      =       ''                                                                                    ! Initialisation to an empty string: default value
  if ( present(Value) ) This%Value = Value                                                                      ! If present optional input argument, then setting the colorspec value to the input value
  if ( len_trim(This%Value) /= 0 ) This%Value = This%Value // " "                                               ! Adding an extra space at the end of the string
  This%Presence         =       ( len_trim(This%Value) /= 0 )                                                   ! Setting the presence indicator if required
  if (This%i_Debug) then
    write(DbgUnit,"(18x,'[Set_Colorspec_Value]: This%Value    = ',a )") This%Value
    write(DbgUnit,"(18x,'[Set_Colorspec_Value]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(18x,'[Set_Colorspec_Value]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Colorspec_Command( This )
  implicit none
  class(GPF_Colorspec_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Colorspec object
  if (This%i_Debug) write(DbgUnit,"(18x,'[Set_Colorspec_Command]: Entering')")
  This%Command          =       ''                                                                              ! Setting the command to empty value
  if ( This%Presence ) This%Command = This%Keyword // This%Value                                                ! If present offset specification, setting the colorspec command
  if (This%i_Debug) then
    write(DbgUnit,"(18x,'[Set_Colorspec_Command]: This%Command  = ',a )") This%Command
    write(DbgUnit,"(18x,'[Set_Colorspec_Command]: Exiting',/)")
  end if
End Subroutine

End Module