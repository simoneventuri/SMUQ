Module URL_CLass

  implicit none

  Type    ::  URL_Type
    private
    character(:)  ,allocatable    ::  Name
    character(:)  ,allocatable    ::  Root
    character(:)  ,allocatable    ::  Param(:,:)
  contains
    private
    procedure ,public   ::  Initialize  =>  InitializeURL
    procedure ,public   ::  Get         =>  GetURL
    generic   ,public   ::  AddParam    =>  AddParamURL_CHAR, AddParamURL_INT32, AddParamURL_LOGICAL
    procedure           ::  AddParamURL_CHAR
    procedure           ::  AddParamURL_INT32
    procedure           ::  AddParamURL_LOGICAL
  End Type

  Interface

    Pure Module Subroutine InitializeURL( This, Name, Root )
      class(URL_Type)                                       ,intent(inout)  ::  This
      character(*)                                ,optional ,intent(in)     ::  Name
      character(*)                                ,optional ,intent(in)     ::  Root
    End Subroutine

    Pure Module Subroutine AddParamURL_CHAR( This, Name, Value )
      class(URL_Type)                                       ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Name
      character(*)                                          ,intent(in)     ::  Value
    End Subroutine

    Pure Module Subroutine AddParamURL_INT32( This, Name, Value )
      use iso_fortran_env ,only:  INT32
      class(URL_Type)                                       ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Name
      integer(INT32)                                        ,intent(in)     ::  Value
    End Subroutine

    Pure Module Subroutine AddParamURL_LOGICAL( This, Name, Value )
      class(URL_Type)                                       ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Name
      logical                                               ,intent(in)     ::  Value
    End Subroutine

    Pure Module Function GetURL( This ) result(URL)
      class(URL_Type)                                       ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  URL
    End Function

  End Interface

End Module