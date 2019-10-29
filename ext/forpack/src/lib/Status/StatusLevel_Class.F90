Module StatusLevel_Class

  implicit none

  private
  public  ::  StatusLevel_Type
  public  ::  NewStatusLevel

  Type    ::  StatusLevel_Type
    private
    character(:)  ,allocatable  ::  File
    character(:)  ,allocatable  ::  Line
    character(:)  ,allocatable  ::  Proc
    character(:)  ,allocatable  ::  Desc
  contains
    private
    procedure   ,public   ::  GetLine   =>  GetStatusLevelLine
  End Type

  Interface

    Module Pure Function NewStatusLevel( File, Line, Proc, Desc ) result(This)
      character(*)                                ,optional ,intent(in)     ::  File
      integer                                     ,optional ,intent(in)     ::  Line
      character(*)                                ,optional ,intent(in)     ::  Proc
      character(*)                                ,optional ,intent(in)     ::  Desc
      type(StatusLevel_Type)                                                ::  This
    End Function

    Pure Module Function GetStatusLevelLine( This ) result(Line)
      class(StatusLevel_Type)                               ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  Line
    End Function

  End Interface

End Module