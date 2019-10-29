Module GPF_TermFactory_Class

  use GPF_Term_Class            ,only:  GPF_Term_Type

  implicit none

  private
  public  ::  GPF_TermFactory_Type

  Type    ::  GPF_TermFactory_Type
  contains
    private
    procedure   ,public ,nopass ::  Build   =>  BuildTerm
  End Type

  Interface
    Module Subroutine BuildTerm( Term, Extension, TermType, Debug )
      class(GPF_Term_Type)  ,allocatable                    ,intent(out)    ::  Term
      character(*)                                          ,intent(in)     ::  Extension                       !< Terminal Extension
      character(*)                                ,optional ,intent(in)     ::  TermType                        !< Terminal Type
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine
  End Interface

End Module