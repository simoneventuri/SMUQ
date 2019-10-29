Module GPF_Axis_Label_Class

  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_Class                 ,only:  GPF

  use GPF_Offset_Class          ,only:  GPF_Offset_Type
  use GPF_Font_Class            ,only:  GPF_Font_Type
  use GPF_Colorspec_Class       ,only:  GPF_Colorspec_Type

  implicit none

  private

  public  ::  GPF_Axis_Label_Type

! ==============================================================================================================
!   COMMAND DESCRIPTION: LABEL
! ==============================================================================================================
! The set xlabel command sets the x axis label. Similar commands set labels on the other axes.
! Syntax:
!       set xlabel {"<label>"} {offset <offset>} {font "<font>{,<size>}"}
!                  {textcolor <colorspec>} {{no}enhanced}
!                  {rotate by <degrees> | rotate parallel | norotate}
!       show xlabel
! The same syntax applies to x2label, ylabel, y2label, zlabel and cblabel.
! If <offset> is specified by either x,y or x,y,z the label is moved by the given offset. It may be preceded by
! first, second, graph, screen, or character to select the coordinate system. See coordinates (p. 22) for
! details. By default, the character coordinate system is used. For example, "set xlabel offset -1,0" will
! change only the x offset of the title, moving the label roughly one character width to the left. The size of a
! character depends on both the font and the terminal.
! <font> is used to specify the font in which the label is written; the units of the font <size> depend upon
! which terminal is used.
! noenhanced requests that the label text not be processed by the enhanced text mode parser, even if
! enhanced text mode is currently active.
! To clear a label, put no options on the command line, e.g., "set y2label".
! The default positions of the axis labels are as follows:
! xlabel: The x-axis label is centered below the bottom of the plot.
! ylabel: The y-axis label is centered to the left of the plot, defaulting to either horizontal or vertical orientation
! depending on the terminal type.
! zlabel: The z-axis label is centered along the z axis and placed in the space above the grid level.
! cblabel: The color box axis label is centered along the box and placed below or to the right according to
! horizontal or vertical color box gradient.
! y2label: The y2-axis label is placed to the right of the y2 axis. The position is terminal-dependent in the
! same manner as is the y-axis label.
! x2label: The x2-axis label is placed above the plot but below the title. It is also possible to create an x2-axis
! label by using new-line characters to make a multi-line plot title, e.g.,
!       set title "This is the title\n\nThis is the x2label"
! Note that double quotes must be used. The same font will be used for both lines, of course.
! The orientation (rotation angle) of the x, x2, y and y2 axis labels in 2D plots can be changed by specifying
! rotate by <degrees>. The orientation of the x and y axis labels in 3D plots defaults to horizontal but
! can be changed to run parallel to the axis by specifying rotate parallel.
! If you are not satisfied with the default position of an axis label, use set label instead–that command gives
! you much more control over where text is placed.
! Please see syntax (p. 41) for further information about backslash processing and the difference between
! single- and double-quoted strings.
! ==============================================================================================================

  Type  ,extends(GPF_Command_Type)                      ::  GPF_Axis_Label_Type
    character(:)        ,allocatable                    ::  Text                                            !< Label text
    type(GPF_Offset_Type)                               ::  Offset                                          !< Label Offset object
    type(GPF_Font_Type)                                 ::  Font                                            !< Label Font object
    type(GPF_Colorspec_Type)                            ::  Color                                           !< Label Color object
    character(:)        ,allocatable                    ::  Enhanced                                        !< Label enhancement text (either "", "enhanced" or "noenhanced")
    character(:)        ,allocatable                    ::  Rotation                                        !< Label rotation text: either "" (identical as "norotate"), "rotate by <degrees>" or "rotate parallel"
  contains
    private
    procedure             ::  Initialize      =>  Initialize_AxisLabel                            !< Initializes the object components
    procedure             ::  Set_Text        =>  Set_AxisLabel_Text                              !< Sets the axis label text
    procedure             ::  Set_Offset      =>  Set_AxisLabel_Offset                            !< Sets the axis label offset
    procedure             ::  Set_Font        =>  Set_AxisLabel_Font                              !< Sets the axis label font
    procedure             ::  Set_Color       =>  Set_AxisLabel_Color                             !< Sets the axis label color
    procedure             ::  Set_Enhanced    =>  Set_AxisLabel_Enhanced                          !< Sets the axis label enhanced indicator
!     procedure             ::  Set_Rotation    =>  Set_AxisLabel_Rotation                          !< Sets the axis label rotation
    procedure   ,public   ::  Set_Command     =>  Set_AxisLabel_Command                           !< Sets the axis label command
  End Type

  Interface             GPF_Axis_Label_Type
    Module Procedure    Construct_Axis_Label
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                            CONSTRUCTOR                                                     *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Axis_Label( Name, Text, Offset, Font_Name, Font_Size, Color, Enhanced, Debug ) result(This)

  implicit none

  type(GPF_Axis_Label_Type)                                             ::  This                            !< Axis object to be constructed
  character(*)                                          ,intent(in)     ::  Name                            !< Axis name
  character(*)                                ,optional ,intent(in)     ::  Text                            !< Label text
  character(*)                                ,optional ,intent(in)     ::  Offset                          !< Label offset
  character(*)                                ,optional ,intent(in)     ::  Font_Name                       !< Label font name
  character(*)                                ,optional ,intent(in)     ::  Font_Size                       !< Label font size
  character(*)                                ,optional ,intent(in)     ::  Color                           !< Label color
  logical                                     ,optional ,intent(in)     ::  Enhanced                        !< Label enhanced indicator
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Graph debugging indicator

  call This%Set_Debug( Debug )                                                                                  ! Setting debugging indicator
  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Label]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Label]: Calling This%Initialize')")
  call This%Initialize()                                                                                        ! Initializing the object components to default values

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Label]: Calling This%Set_Text')")
  call This%Set_Text( Text )                                                                                    ! Setting the axis label text

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Label]: Calling This%Set_Offset')")
  call This%Set_Offset( Offset )                                                                                ! Setting the axis label offset

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Label]: Calling This%Set_Font')")
  call This%Set_Font( Font_Name, Font_Size )                                                                    ! Setting the axis label font

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Label]: Calling This%Set_Color')")
  call This%Set_Color( Color )                                                                                  ! Setting the axis label color

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Label]: Calling This%Set_Enhanced')")
  call This%Set_Enhanced( Enhanced )                                                                            ! Setting the axis label enhanced indicator

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Label]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Name, i_Trim=.True. )                                                                  ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Label]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the axis command (actually, it is just a comment)

  if (This%i_Debug) write(DbgUnit,"(14x,'[Construct_Axis_Label]: Exiting')")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

! REMARK:
! The Name component must  not be initialized
Subroutine Initialize_AxisLabel( This )
  implicit none
  class(GPF_Axis_Label_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Label object
  if (This%i_Debug) write(DbgUnit,"(16x,'[Initialize_AxisLabel]: Entering')")
  This%Text             =       ''                                                                              ! Initializing to an empty string: default value => no label
  This%Offset%Command   =       ''                                                                              ! Initializing the command to an empty string: default value
  This%Font%Command     =       ''                                                                              ! Initializing the command to an empty string: default value
  This%Color%Command    =       ''                                                                              ! Initializing the command to an empty string: default value
  This%Enhanced         =       ''                                                                              ! Initializing to an empty string: default value => no enhancement
  if (This%i_Debug) write(DbgUnit,"(16x,'[Initialize_AxisLabel]: Exiting',/)")
End Subroutine

Subroutine Set_AxisLabel_Text( This, Text )
  use GPF_Tools                 ,only:  Add_Apostroph
  implicit none
  class(GPF_Axis_Label_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Label object
  character(*)                                ,optional ,intent(in)     ::  Text                            !< Axis label text
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_AxisLabel_Text]: Entering')")
  if ( present(Text) )  This%Text = Add_Apostroph( Text )                                                       ! If present optional input argument, then setting its value
  if ( len_trim(This%Text) /= 0 ) This%Text = This%Text // " "                                                  ! Adding an extra space at the end of the string
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_AxisLabel_Text]: This%Text = ',a)") This%Text
    write(DbgUnit,"(12x,'[Set_AxisLabel_Text]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_AxisLabel_Offset( This, Offset )
  implicit none
  class(GPF_Axis_Label_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Label object
  character(*)                                ,optional ,intent(in)     ::  Offset                          !< Axis label offset
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_AxisLabel_Offset]: Entering')")
  if ( len_trim(This%Text) /= 0 ) then                                                                          ! If specifed text, then the offset specification make sense and so setting it according to input value
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_AxisLabel_Offset]: Calling This%GPF_Offset_Type')")
    This%Offset         =       GPF_Offset_Type( Offset, This%i_Debug )                                         ! Setting the offset if any
  end if                                                                                                        ! End if case on text specificaton (if no text, then no need to define the associated offset)
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_AxisLabel_Offset]: This%Offset%Command = ',a)") This%Offset%Command
    write(DbgUnit,"(12x,'[Set_AxisLabel_Offset]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_AxisLabel_Font( This, Font_Name, Font_Size )
  implicit none
  class(GPF_Axis_Label_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Label object
  character(*)                                ,optional ,intent(in)     ::  Font_Name                       !< Font name
  character(*)                                ,optional ,intent(in)     ::  Font_Size                       !< Font size
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_AxisLabel_Font]: Entering')")
  if ( len_trim(This%Text) /= 0 ) then                                                                          ! If specifed text, then the font specification make sense and so setting it according to input value
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_AxisLabel_Font]: Calling This%Font%Initialize')")
    call This%Font%Initialize( Font_Name, Font_Size, This%i_Debug )                               ! Setting the text font if any
  end if                                                                                                        ! End if case on text specificaton (if no text, then no need to define the associated font)
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_AxisLabel_Font]: This%Font%Command = ',a)") This%Font%Command
    write(DbgUnit,"(12x,'[Set_AxisLabel_Font]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_AxisLabel_Color( This, Color )
  implicit none
  class(GPF_Axis_Label_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Label object
  character(*)                                ,optional ,intent(in)     ::  Color                           !< Axis label color
  character(*)                                              ,parameter  ::  Color_Keyword="tc"              ! Setting the color keyword to "tc" (for "textcolor")
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_AxisLabel_Color]: Entering')")
  if ( len_trim(This%Text) /= 0 ) then                                                                          ! If specifed text, then the color specification make sense and so setting it according to input value
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_AxisLabel_Color]: Calling GPF_Colorspec_Type')")
    This%Color  =       GPF_Colorspec_Type( Color_Keyword, Color, This%i_Debug )                                ! Setting the text color if any
  end if                                                                                                        ! End if case on text specificaton (if no text, then no need to define the associated color)
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_AxisLabel_Color]: This%Color%Command = ',a)") This%Color%Command
    write(DbgUnit,"(12x,'[Set_AxisLabel_Color]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_AxisLabel_Enhanced( This, Enhanced )
  use GPF_Parameters            ,only:  KEY_enhanced, KEY_noenhanced
  implicit none
  class(GPF_Axis_Label_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Label object
  logical                                     ,optional ,intent(in)     ::  Enhanced                        !< Axis label enhanced indicator
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_AxisLabel_Enhanced]: Entering')")
  if ( len_trim(This%Text) /= 0 ) then                                                                          ! If specifed text, then the enhancement specification make sense and so setting it according to input value
    if ( present(Enhanced) ) then                                                                               ! If present optional input argument
      if (Enhanced) then; This%Enhanced = KEY_enhanced                                                          ! If text enhancement is on, then setting on text enhancement
      else;               This%Enhanced = KEY_noenhanced                                                        ! If text enhancement is off, then setting on text enhancement
      end if                                                                                                    ! Enf if case on enhancement indicator
    end if                                                                                                      ! Enf if case optional input argument presence
  end if                                                                                                        ! End if case on text specificaton (if no text, then no need to define the associated enhancement indicator)
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_AxisLabel_Enhanced]: This%Enhanced = ',a)") This%Enhanced
    write(DbgUnit,"(12x,'[Set_AxisLabel_Enhanced]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_AxisLabel_Command( This )
  implicit none
  class(GPF_Axis_Label_Type)                            ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Axis-Label object
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_AxisLabel_Command]: Entering')")
  This%Command          =       ''                                                                              ! Initializing the command to an empty string: default value => no label
  if ( len_trim(This%Text) /= 0 )               This%Command = This%Command // This%Text                        ! Setting the text text if defined
  if ( len_trim(This%Offset%Command) /= 0 )     This%Command = This%Command // This%Offset%Command              ! Setting the text offset if defined
  if ( len_trim(This%Font%Command) /= 0 )       This%Command = This%Command // This%Font%Command                ! Setting the text font if defined
  if ( len_trim(This%Color%Command) /= 0 )      This%Command = This%Command // This%Color%Command               ! Setting the text color if defined
  if ( len_trim(This%Enhanced) /= 0 )           This%Command = This%Command // This%Enhanced                    ! Setting the text color if defined
  if ( len_trim(This%Command) /= 0 )    This%Command = 'set ' // This%Keyword // "label "// This%Command        ! Setting the command string
  This%Presence         =       ( len_trim(This%Command) /= 0 )                                                 ! Setting the presence indicator
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_AxisLabel_Command]: This%Command  = ',a )") This%Command
    write(DbgUnit,"(12x,'[Set_AxisLabel_Command]: This%Presence = ',l3)") This%Presence
    write(DbgUnit,"(12x,'[Set_AxisLabel_Command]: Exiting',/)")
  end if
End Subroutine

End Module