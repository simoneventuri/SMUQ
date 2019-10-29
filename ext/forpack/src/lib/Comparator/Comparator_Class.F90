Module Comparator_Class

  use iso_fortran_env       ,only:  REAL64

  implicit none

  private
  public  ::  Comparator_Type

  Type          ::  Comparator_Type
    logical                     ::  RelativeDifference  =   .False.
    logical                     ::  AbsoluteDifference  =   .False.
    real(REAL64)                ::  RelTol              =   1.0E-06_REAL64
    real(REAL64)                ::  AbsTol              =   1.0E-25_REAL64
    logical                     ::  Passed              =   .True.
    character(:)  ,allocatable  ::  ProcName
  contains
    private
    procedure ,public ::  Initialize    =>  InitializeComparator
    procedure ,public ::  SetProperties =>  SetComparatorProperties
    procedure ,public ::  Compare       =>  CompareComparator
  End Type

  Interface

    Module Subroutine InitializeComparator( This, RelTol, AbsTol, ProcName )
      class(Comparator_Type)                                  ,intent(out)    ::  This
      real(REAL64)                                  ,optional ,intent(in)     ::  RelTol
      real(REAL64)                                  ,optional ,intent(in)     ::  AbsTol
      character(*)                                  ,optional ,intent(in)     ::  ProcName
    End Subroutine

    Module Subroutine SetComparatorProperties( This, RelTol, AbsTol, ProcName )
      class(Comparator_Type)                                  ,intent(inout)  ::  This
      real(REAL64)                                  ,optional ,intent(in)     ::  RelTol
      real(REAL64)                                  ,optional ,intent(in)     ::  AbsTol
      character(*)                                  ,optional ,intent(in)     ::  ProcName
    End Subroutine

    Module Subroutine CompareComparator( This, Expected, Found, Description, RelTol, AbsTol, ProcName )
      class(Comparator_Type)                                  ,intent(inout)  ::  This
      real(REAL64)                                            ,intent(in)     ::  Expected
      real(REAL64)                                            ,intent(in)     ::  Found
      real(REAL64)                                  ,optional ,intent(in)     ::  RelTol
      real(REAL64)                                  ,optional ,intent(in)     ::  AbsTol
      character(*)                                  ,optional ,intent(in)     ::  Description
      character(*)                                  ,optional ,intent(in)     ::  ProcName
    End Subroutine

  End Interface

End Module