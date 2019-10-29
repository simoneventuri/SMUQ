Module FitModelTest_Module

  use Test_Utilities_Module       ,only:  Plot, AddNoise
  use Logger_Class                ,only:  Logger, LogLevel_INFO, LogLevel_DEBUG
  use Parameters_Library          ,only:  rkp
  use Fitter_Library              ,only:  Fitter_Type, FitModel_Type

  implicit none

  private
  public  ::  Test_Constant
  public  ::  Test_Linear
  public  ::  Test_Quadratic
  public  ::  Test_Arrhenius
  public  ::  Test_NASA9

  Interface
    Module Subroutine Test_Constant( LogLevel )
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine
    Module Subroutine Test_Linear( LogLevel )
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine
    Module Subroutine Test_Quadratic( LogLevel )
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine
    Module Subroutine Test_Arrhenius( LogLevel )
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine
    Module Subroutine Test_NASA9( LogLevel )
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine
  End Interface

End Module