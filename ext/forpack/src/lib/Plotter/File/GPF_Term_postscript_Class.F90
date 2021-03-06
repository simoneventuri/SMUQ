Module GPF_Term_POSTSCRIPT_Class

  use GPF_Term_Class            ,only:  GPF_Term_Type
  use GPF_Font_Class            ,only:  GPF_Font_Type

  implicit none

  private

  public  ::  GPF_Term_POSTSCRIPT_Type

  Type  ,extends(GPF_Term_Type) ::  GPF_Term_POSTSCRIPT_Type
    private
    character(:)  ,allocatable  ::  Extension                                       !< Terminal extension
    type(GPF_Font_Type)         ::  Font                                            !< Terminal Font object
  contains
    private
    procedure   ,public   ::  Initialize    =>  InitializeTermPOSTSCRIPT
    procedure   ,public   ::  Set_Command   =>  SetCommandTermPOSTSCRIPT
    procedure   ,public   ::  GetName
  End Type

  Interface

    Module Subroutine InitializeTermPOSTSCRIPT( This, Color, FontName, FontSize, Enhanced, Extension, Title, Dashed, Persist, Size, Debug )
      class(GPF_Term_POSTSCRIPT_Type)                       ,intent(inout)  ::  This                            !< Terminal object to be constructed
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
      class(GPF_Term_POSTSCRIPT_Type)                     ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Name
    End Function

    Module Subroutine SetCommandTermPOSTSCRIPT( This )
      class(GPF_Term_POSTSCRIPT_Type)                       ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Terminal object
    End Subroutine

  End Interface

End Module