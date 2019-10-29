! ==============================================================================================================
!   THE KEY COMMAND
! ==============================================================================================================
! The set key command enables a key (or legend) describing plots on a plot.
! The contents of the key, i.e., the names given to each plotted data set and function and samples of the
! lines and/or symbols used to represent them, are determined by the title and with options of the {s}plot
! command. Please see plot title (p. 89) and plot with (p. 90) for more information.
! Syntax:
!       set key {on|off} {default}
!               {{inside | outside} | {lmargin | rmargin | tmargin | bmargin}
!                 | {at <position>}}
!               {left | right | center} {top | bottom | center}
!               {vertical | horizontal} {Left | Right}
!               {{no}opaque}
!               {{no}reverse} {{no}invert}
!               {samplen <sample_length>} {spacing <vertical_spacing>}
!               {width <width_increment>}
!               {height <height_increment>}
!               {{no}autotitle {columnheader}}
!               {title "<text>"} {{no}enhanced}
!               {font "<face>,<size>"} {textcolor <colorspec>}
!               {{no}box { {linestyle | ls <line_style>}
!                          | {linetype | lt <line_type>}
!                            {linewidth | lw <line_width>}}}
!               {maxcols {<max no. of columns> | auto}}
!               {maxrows {<max no. of rows> | auto}}
! ==============================================================================================================
Module GPF_Key_Class

  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_Font_Class            ,only:  GPF_Font_Type

  implicit none

  private
  public  ::  GPF_Key_Type

  Type  ,extends(GPF_Command_Type)  ::  GPF_Key_Type
    private
    logical                     ::  UnSet                                           !< Key unsetting indicator
    type(GPF_Font_Type)         ::  Font                                            !< Key Font object
    character(:)  ,allocatable  ::  Position                                        !< Position of the legend in the graph
    character(:)  ,allocatable  ::  Spacing                                         !< Spacing between legent lines
  contains
    private
    procedure   ,public   ::  Initialize    =>  InitializeKey
    procedure   ,public   ::  Set_Command   =>  SetKeyCommand
  End Type

  Interface

    Module Subroutine InitializeKey( This, FontName, FontSize, Position, Spacing, Unset, Debug )
      class(GPF_Key_Type)                                   ,intent(out)    ::  This
      character(*)                                ,optional ,intent(in)     ::  FontName                        !< Key font name
      character(*)                                ,optional ,intent(in)     ::  FontSize                        !< Key font size
      character(*)                                ,optional ,intent(in)     ::  Position                        !< Key position
      character(*)                                ,optional ,intent(in)     ::  Spacing                         !< Key spacing
      logical                                     ,optional ,intent(in)     ::  Unset                           !< Key Unset indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine SetKeyCommand( This )
      class(GPF_Key_Type)                                   ,intent(inout)  ::  This                            !< Passed-object dummy argument
    End Subroutine

  End Interface

End Module