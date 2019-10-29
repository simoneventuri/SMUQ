Module GPF_Term_PNGCairo_Class

  use GPF_Term_Class            ,only:  GPF_Term_Type
  use GPF_Font_Class            ,only:  GPF_Font_Type

  implicit none

  private
  public  ::  GPF_Term_PNGCairo_Type


  Type  ,extends(GPF_Term_Type) ::  GPF_Term_PNGCairo_Type
    private
    character(:)  ,allocatable  ::  LineType                                        !< Terminal default type of line (either "", "dashed" or "solid")
    type(GPF_Font_Type)         ::  Font                                            !< Terminal Font object
    character(:)  ,allocatable  ::  Size_                                           !< (either "" or "size <XX>{unit},<YY>{unit}")
  contains
    private
    procedure   ,public   ::  Initialize    =>  InitializeTermPNGCairo
    procedure   ,public   ::  Set_Command   =>  SetCommandTermPNGCairo
    procedure   ,public   ::  GetName
    procedure   ,public   ::  SetColor      =>  SetTerminalColor
  End Type

  Interface

    Module Subroutine InitializeTermPNGCairo( This, Color, FontName, FontSize, Enhanced, Extension, Title, Dashed, Persist, Size, Debug )
      class(GPF_Term_PNGCairo_Type)                         ,intent(inout)  ::  This                            !< Terminal object to be constructed
      logical                                     ,optional ,intent(in)     ::  Color                           !< Terminal color indicator (either true=>'color' or false=>'monochrome')
      character(*)                                ,optional ,intent(in)     ::  FontName                        !< Terminal font name
      character(*)                                ,optional ,intent(in)     ::  FontSize                        !< Terminal font size
      logical                                     ,optional ,intent(in)     ::  Enhanced                        !< Terminal enhancement
      character(*)                                ,optional ,intent(in)     ::  Extension                       !< Terminal extension (Only used for POSTSCRIPT: either 'eps' or 'ps')
      character(*)                                ,optional ,intent(in)     ::  Title                           !< Window Title (Only wxt)
      logical                                     ,optional ,intent(in)     ::  Dashed                          !< Dashed mode indicator (Only wxt)
      logical                                     ,optional ,intent(in)     ::  Persist                         !< Persist mode indicator (Only wxt)
      character(*)                                ,optional ,intent(in)     ::  Size                            !< (either "" or "size <XX>{unit},<YY>{unit}")
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine

    Module Pure Function GetName( This ) result(Name)
      class(GPF_Term_PNGCairo_Type)                         ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Name
    End Function

    Module Subroutine SetCommandTermPNGCairo( This )
      class(GPF_Term_PNGCairo_Type)                         ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Terminal object
    End Subroutine

    Module Subroutine SetTerminalColor( This, Color )
      class(GPF_Term_PNGCairo_Type)                         ,intent(inout)  ::  This                            !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  Color                           !< Terminal Color indicator
    End Subroutine

  End Interface

End Module