! ==============================================================================================================
!   COMMAND DESCRIPTION
! ==============================================================================================================
! This terminal produces files in the Adobe Portable Document Format (PDF), useable for printing or display
! with tools like Acrobat Reader
! Syntax:
!       set term pdf
!               {monochrome|color|colour}
!               {{no}enhanced}
!               {fname "<font>"} {fsize <fontsize>}
!               {font "<fontname>{,<fontsize>}"} {fontscale <scale>}
!               {linewidth <lw>} {rounded|butt}
!               {solid|dashed} {dl <dashlength>}}
!               {size <XX>{unit},<YY>{unit}}
! The default is to use a different color for each line type. Selecting monochome will use black for all
! linetypes, in which case you probably want to select dashed to distinguish line types. Even in in mono
! mode you can still use explicit colors for filled areas or linestyles.
! where <font> is the name of the default font to use (default Helvetica) and <fontsize> is the font size
! (in points, default 12). For help on which fonts are available or how to install new ones, please see the
! documentation for your local installation of pdflib.
! The enhanced option enables enhanced text processing features (subscripts, superscripts and mixed fonts).
! See enhanced (p. 23).
! The width of all lines in the plot can be increased by the factor <n> specified in linewidth. Similarly
! dashlength is a multiplier for the default dash spacing.
! rounded sets line caps and line joins to be rounded; butt is the default, butt caps and mitered joins.
! The default size for PDF output is 5 inches by 3 inches. The size option changes this to whatever the user
! requests. By default the X and Y sizes are taken to be in inches, but other units are possible (currently only
! cm).
! ==============================================================================================================
SubModule(GPF_Term_PDF_Class) GPF_Term_PDF_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure GetName
  use GPF_Parameters            ,only:  KEY_pdf
  Name    =   KEY_pdf
End Procedure

Module Procedure InitializeTermPDF

  character(*)                                              ,parameter  ::  ProcName = "InitializeTermPDF"
  character(*)                                              ,parameter  ::  Keyword='term'                  !< Keyword of current command
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling This%SetColor" )
  call This%SetColor( Color )

  if (Dbg) call Logger%Write( "Calling GPF_Font_Type" )
  call This%Font%Initialize( FontName, FontSize )

  if (Dbg) call Logger%Write( "Calling This%SetEnhanced" )
  call This%SetEnhanced( Enhanced )

  if (Dbg) call Logger%Write( "Calling This%Set_Keyword" )
  call This%Set_Keyword( Keyword )

  if (Dbg) call Logger%Write( "Calling This%Set_Command" )
  call This%Set_Command()

  if (Dbg) call Logger%Write( "Command = ", trim(This%Command) )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetCommandTermPDF
  This%Command  =   'set '  //  &
    This%Keyword            //      &
    This%GetName()//" "     //      &
    This%Color              //      &
    This%Enhanced           //      &
    " dashed "              //      &
    This%Font%Command
End Procedure

End SubModule