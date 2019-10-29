Module IterStat_Class

  use Timer_Class           ,only:  Timer_Type
  use iso_fortran_env       ,only:  REAL64

  implicit none

  private
  public        ::  IterStat_Type

  Type                  ::  IterStat_Type
    private
    Type(Timer_Type)    ::  Timer
    integer             ::  N           =   0
    integer             ::  Freq        =   1
    real(REAL64)        ::  Percentage  =   0.0_REAL64
    real(REAL64)        ::  CurrentTime =   0.0_REAL64
    real(REAL64)        ::  TotalTime   =   0.0_REAL64
    real(REAL64)        ::  LeftTime    =   0.0_REAL64
  contains
    procedure ,public   ::  Initialize  =>  InitializeIterStat
    procedure ,public   ::  Update      =>  UpdateIterStat
    procedure ,public   ::  DoIt        =>  NeedIterStatUpdate
    procedure ,public   ::  GetInfo     =>  GetIterStatInfo
    procedure ,public   ::  GetString   =>  GetIterStatString
  End type

  Interface

    Module Subroutine InitializeIterStat( This, N, Freq, EvalMax )
      class(IterStat_Type)                                  ,intent(out)    ::  This
      integer                                               ,intent(in)     ::  N
      integer                                     ,optional ,intent(in)     ::  Freq
      integer                                     ,optional ,intent(in)     ::  EvalMax  !< Maximum number of evaluation (if defined, will override Freq)
    End Subroutine

    Module Subroutine UpdateIterStat( This, i )
      class(IterStat_Type)                                  ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  i
    End Subroutine

    Module Function GetIterStatString( This, i, Update ) result(String)
      class(IterStat_Type)                                  ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  i
      logical                                     ,optional ,intent(in)     ::  Update
      character(:)  ,allocatable                                            ::  String
    End Function

    Module Pure Function NeedIterStatUpdate( This, i ) result(Indicator)
      class(IterStat_Type)                                  ,intent(in)     ::  This
      integer                                               ,intent(in)     ::  i
      logical                                                               ::  Indicator
    End Function


    Module Subroutine GetIterStatInfo( This, i, Percentage, LeftTime, String )
      class(IterStat_Type)                                  ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  i
      real(REAL64)                                ,optional ,intent(out)    ::  Percentage
      real(REAL64)                                ,optional ,intent(out)    ::  LeftTime
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  String
    End Subroutine

  End Interface

End Module
