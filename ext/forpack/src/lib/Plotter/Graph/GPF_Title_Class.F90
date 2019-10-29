Module GPF_Title_Class

  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_Offset_Class          ,only:  GPF_Offset_Type
  use GPF_Font_Class            ,only:  GPF_Font_Type
  use GPF_Colorspec_Class       ,only:  GPF_Colorspec_Type

  implicit none

  private
  public  ::  GPF_Title_Type

! ==============================================================================================================
!   THE VIEW COMMAND
! ==============================================================================================================
! The set title command produces a plot title that is centered at the top of the plot. set title is a special
! case of set label.
! Syntax:
!       set title {"<title-text>"} {offset <offset>} {font "<font>{,<size>}"}
!                 {{textcolor | tc} {<colorspec> | default}} {{no}enhanced}
!       show title
! If <offset> is specified by either x,y or x,y,z the title is moved by the given offset. It may be preceded by
! first, second, graph, screen, or character to select the coordinate system. See coordinates (p. 22) for
! details. By default, the character coordinate system is used. For example, "set title offset 0,-1" will
! change only the y offset of the title, moving the title down by roughly the height of one character. The size
! of a character depends on both the font and the terminal.
! <font> is used to specify the font with which the title is to be written; the units of the font <size> depend
! upon which terminal is used.
! textcolor <colorspec> changes the color of the text. <colorspec> can be a linetype, an rgb color, or a
! palette mapping. See help for colorspec (p. 34) and palette (p. 137).
! noenhanced requests that the title not be processed by the enhanced text mode parser, even if enhanced
! text mode is currently active.
! set title with no parameters clears the title.
! See syntax (p. 41) for details about the processing of backslash sequences and the distinction between
! single- and double-quotes.
! ==============================================================================================================

  Type  ,extends(GPF_Command_Type) ::  GPF_Title_Type
    private
    character(:)  ,allocatable  ::  Text                                            !< Title text
    character(:)  ,allocatable  ::  Enhanced                                        !< Title enhancement text (either "", "enhanced" or "noenhanced")
    type(GPF_Offset_Type)       ::  Offset                                          !< Title Offset object
    type(GPF_Font_Type)         ::  Font                                            !< Title Font object
    type(GPF_Colorspec_Type)    ::  Color                                           !< Title Color object
  contains
    private
    procedure   ,public   ::  Initialize    =>  InitializeTitle
    procedure   ,public   ::  Set_Command   =>  SetTitleCommand
  End Type

  Interface

    Module Subroutine InitializeTitle( This, Text, Offset, FontName, FontSize, Color, Enhanced, Debug )
      class(GPF_Title_Type)                                 ,intent(out)    ::  This                            !< Passed-object dummy argument corresponding to the Title object
      character(*)                                ,optional ,intent(in)     ::  Text                            !< Title text
      character(*)                                ,optional ,intent(in)     ::  Offset                          !< Title offset
      character(*)                                ,optional ,intent(in)     ::  FontName                        !< Title font name
      character(*)                                ,optional ,intent(in)     ::  FontSize                        !< Title font size
      character(*)                                ,optional ,intent(in)     ::  Color                           !< Title color
      logical                                     ,optional ,intent(in)     ::  Enhanced                        !< Title enhanced indicator
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Subroutine SetTitleCommand( This )
      class(GPF_Title_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Title object
    End Subroutine

  End Interface

End Module