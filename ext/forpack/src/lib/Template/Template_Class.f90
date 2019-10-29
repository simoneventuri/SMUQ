Module Template_Class

  implicit none

  private
  public                ::  Template_Type

  Type                  ::  Template_Type
    private
    logical             ::  Initialized = .False.
  contains
    procedure   ,public ::  Initialize  =>    InitializeTemplate
  End type

  Interface
    Module Subroutine InitializeTemplate( This )
      class(Template_Type)                                  ,intent(inout)  ::  This
    End Subroutine
  End Interface

End Module