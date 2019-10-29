
! ==============================================================================================================
!   COMMAND DESCRIPTION
! ==============================================================================================================
! The pngcairo terminal device generates output in png. The actual drawing is done via cairo, a 2D graphics
! library, and pango, a library for laying out and rendering text.
! Syntax:
!       set term pngcairo
!                   {{no}enhanced} {mono|color} {solid|dashed}
!                   {{no}transparent} {{no}crop} {background <rgbcolor>
!                   {font <font>} {fontscale <scale>}
!                   {linewidth <lw>} {rounded|butt} {dashlength <dl>}
!                   {size <XX>{unit},<YY>{unit}}
! This terminal supports an enhanced text mode, which allows font and other formatting commands (sub-
! scripts, superscripts, etc.) to be embedded in labels and other text strings. The enhanced text mode syntax
! is shared with other gnuplot terminal types. See enhanced (p. 23) for more details.
! The width of all lines in the plot can be modified by the factor <lw>.
! rounded sets line caps and line joins to be rounded; butt is the default, butt caps and mitered joins.
! The default size for the output is 640 x 480 pixels. The size option changes this to whatever the user
! requests. By default the X and Y sizes are taken to be in pixels, but other units are possible (currently cm
! and inch). A size given in centimeters or inches will be converted into pixels assuming a resolution of 72 dpi.
! Screen coordinates always run from 0.0 to 1.0 along the full length of the plot edges as specified by the size
! option.
! <font> is in the format "FontFace,FontSize", i.e. the face and the size comma-separated in a single string.
! FontFace is a usual font face name, such as ’Arial’. If you do not provide FontFace, the pngcairo terminal
! will use ’Sans’. FontSize is the font size, in points. If you do not provide it, the pngcairo terminal will use a
! size of 12 points.
! For example :
! set term pngcairo font "Arial,12"
! set term pngcairo font "Arial" # to change the font face only
! set term pngcairo font ",12" # to change the font size only
! set term pngcairo font "" # to reset the font name and size
! The fonts are retrieved from the usual fonts subsystems. Under Windows, those fonts are to be found and
! configured in the entry "Fonts" of the control panel. Under UNIX, they are handled by "fontconfig".
! Pango, the library used to layout the text, is based on utf-8. Thus, the pngcairo terminal has to convert
! from your encoding to utf-8. The default input encoding is based on your ’locale’. If you want to use another
! encoding, make sure gnuplot knows which one you are using. See encoding (p. 110) for more details.
! Pango may give unexpected  with fonts that do not respect the unicode mapping. With the Symbol
! font, for example, the pngcairo terminal will use the map provided by http://www.unicode.org/ to trans-
! late character codes to unicode. Note that "the Symbol font" is to be understood as the Adobe Symbol
! font, distributed with Acrobat Reader as "SY
!  .PFB". Alternatively, the OpenSymbol font, distributed
! with OpenOffice.org as "opens .ttf", offers the same characters. Microsoft has distributed a Symbol font
! ("symbol.ttf"), but it has a different character set with several missing or moved mathematic characters. If
! you experience problems with your default setup (if the demo enhancedtext.dem is not displayed properly
! for example), you probably have to install one of the Adobe or OpenOffice Symbol fonts, and remove the
! Microsoft one. Other non-conform fonts, such as "wingdings" have been observed working.
! The rendering of the plot cannot be altered yet. To obtain the best output possible, the rendering involves two
! mechanisms : antialiasing and oversampling. Antialiasing allows to display non-horizontal and non-vertical
! lines smoother. Oversampling combined with antialiasing provides subpixel accuracy, so that gnuplot can
! draw a line from non-integer coordinates. This avoids wobbling effects on diagonal lines (’plot x’ for example).
! ==============================================================================================================
! @TODO: The following variables are to be implemented as component of the GPF_Term_PNGCairo_Type type:

!     character(:)        ,allocatable                    ::  Transparent                                     !< (either "", "notransparent" or "notransparent")
!     character(:)        ,allocatable                    ::  Crop                                            !< (either "", "nocrop" or "crop")
!     character(:)        ,allocatable                    ::  Background                                      !< (either "" or "background <rgbcolor>")
!     character(:)        ,allocatable                    ::  FontScale                                       !< (either "", "fontscale <scale>")
!     character(:)        ,allocatable                    ::  LineWidth                                       !< (either "", "linewidth <lw>")
!     character(:)        ,allocatable                    ::  Rounded                                         !< (either "", "butt" or "rounded")
!     character(:)        ,allocatable                    ::  DashLength                                      !< (either "", "dashlength <dl>")
! ==============================================================================================================
!   character(*)                                ,optional ,intent(in)     ::  LineType                        !< Terminal default type of line
!   logical                                     ,optional ,intent(in)     ::  Transparent                     !< Terminal transparency indicator (either "", "notransparent" or "notransparent")
!   logical                                     ,optional ,intent(in)     ::  Crop                            !< Terminal cropping indicator (either "", "nocrop" or "crop")
!   character(*)                                ,optional ,intent(in)     ::  Background                      !< Terminal background color (either "" or "background <rgbcolor>")
!   character(*)                                ,optional ,intent(in)     ::  FontScale                       !< (either "", "fontscale <scale>")
!   character(*)                                ,optional ,intent(in)     ::  LineWidth                       !< (either "", "linewidth <lw>")
!   logical                                     ,optional ,intent(in)     ::  Rounded                         !< (either "", "butt" or "rounded")
!   character(*)                                ,optional ,intent(in)     ::  DashLength                      !< (either "", "dashlength <dl>")
! ==============================================================================================================


SubModule(GPF_Term_PNGCairo_Class) GPF_Term_PNGCairo_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         CONSTRUCTOR PROCEDURES                                             *
! **************************************************************************************************************
! **************************************************************************************************************

Module Procedure GetName
  use GPF_Parameters            ,only:  KEY_pngcairo
  Name    =   KEY_pngcairo
End Procedure

Module Procedure InitializeTermPNGCairo

  character(*)                                              ,parameter  ::  ProcName = "InitializeTermPNGCairo"
  character(*)                                              ,parameter  ::  Keyword='term'
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling This%SetEnhanced" )
  call This%SetEnhanced( Enhanced )

  if (Dbg) call Logger%Write( "Calling This%SetColor" )
  call This%SetColor( Color )

  if (Dbg) call Logger%Write( "Calling SetTerminalLineType" )
  call SetTerminalLineType( This )

  if (Dbg) call Logger%Write( "Calling This%Font%Initialize" )
  call This%Font%Initialize( FontName, FontSize )

  if (Dbg) call Logger%Write( "Calling SetTerminalSize" )
  call SetTerminalSize( This, Size )

  if (Dbg) call Logger%Write( "Calling This%Set_Keyword" )
  call This%Set_Keyword( Keyword )

  if (Dbg) call Logger%Write( "Calling This%Set_Command" )
  call This%Set_Command( )

  if (Dbg) call Logger%Write( "Command = ", trim(This%Command) )

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure SetCommandTermPNGCairo
  This%Command  =   'set '  //  &
    This%Keyword            //  &
    This%GetName()//" "     //      &
    This%Color              //  &
    This%Enhanced           //  &
    This%LineType           //  &
    This%Font%Command       //  &
    This%Size_
End Procedure


Module Procedure SetTerminalColor
  use Utilities_Library         ,only:  PresentAndFalse
  This%Color  =   'color'
  if ( PresentAndFalse(Color) ) This%Color = 'mono'
  This%Color  =   This%Color//' '
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
!                                       PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine SetTerminalLineType( This )
  class(GPF_Term_PNGCairo_Type)                         ,intent(inout)  ::  This
  character(*)                                              ,parameter  ::  LineType_Default='dashed '
  This%LineType   =   LineType_Default
End Subroutine

Subroutine SetTerminalSize( This, Size )
  type(GPF_Term_PNGCairo_Type)                          ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  Size
  This%Size_    =   ""
  if ( present(Size) ) This%Size_ = "size " // Size
End Subroutine

End SubModule