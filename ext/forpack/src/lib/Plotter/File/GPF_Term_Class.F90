Module GPF_Term_Class

  use GPF_Command_Class         ,only:  GPF_Command_Type

  implicit none

  private
  public  ::  GPF_Term_Type

! @TODO: The public attribute of setter and getter procedure could be removed if those functions
!       could somehow be accessible from the extended type. Check whether the standard allows such a behavior.
!       With current compiler version (ifort version 13.0.0), extended type cannot accesse private components
!       and type-bound procedures.

  Type  ,abstract ,extends(GPF_Command_Type)  ::  GPF_Term_Type
!     private
    character(:)  ,allocatable  ::  Color                     !< Terminal color mode
    character(:)  ,allocatable  ::  Enhanced                  !< Terminal enhancement
  contains
    private
    procedure(Initialization) ,public, deferred ::  Initialize
    procedure(NameGetter)     ,public, deferred ::  GetName
    procedure   ,public   ::  SetEnhanced   =>  SetTerminalEnhanced
    procedure   ,public   ::  SetColor      =>  SetTerminalColor
  End Type

  Abstract Interface

    Subroutine Initialization( This, Color, FontName, FontSize, Enhanced, Extension, Title, Dashed, Persist, Size, Debug )
      import  ::  GPF_Term_Type
      class(GPF_Term_Type)                                  ,intent(inout)  ::  This                            !< Terminal object to be constructed
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

    Pure Function NameGetter( This ) result(Name)
      import  ::  GPF_Term_Type
      class(GPF_Term_Type)                                  ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Name
    End Function

  End Interface

  Interface

    Module Subroutine SetTerminalEnhanced( This, Enhanced )
      class(GPF_Term_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  Enhanced                        !< Enhancement indicator
    End Subroutine

    Module Subroutine SetTerminalColor( This, Color )
      class(GPF_Term_Type)                                  ,intent(inout)  ::  This                            !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  Color                           !< Terminal Color indicator
    End Subroutine

  End Interface

End Module