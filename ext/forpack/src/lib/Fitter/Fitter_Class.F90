Module Fitter_Class

  use iso_fortran_env         ,only:  rkp => REAL64
  use FitterInterval_Class    ,only:  FitterInterval_Type
  use FitterConstrain_Class   ,only:  FitterConstrain_Type
  use FitModel_Class          ,only:  FitModel_Type

  implicit none

  private
  public                ::  Fitter_Type

  Type                                                                  ::  Fitter_Type
    integer                                                             ::  NIntervals
    type(FitterInterval_Type)   ,allocatable                            ::  Intervals(:)
    integer                                                             ::  NConstrains
    type(FitterConstrain_Type)  ,allocatable                            ::  Constrains(:)
    integer                                                             ::  NPtsTab         =       0       !< Number of points per tabulated data
    integer                                                             ::  NPtsFit         =       0       !< Number of points per fitted data
    integer                                                             ::  NCoef           =       0       !< Number of fitting coefficients
    integer                                                             ::  iX_Ini                          !< Index of initial X of fitting range (in the tabulated temperature index list)
    integer                                                             ::  iX_Fin                          !< Index of final   X of fitting range (in the tabulated temperature index list)
    integer     ,dimension( : )         ,allocatable                    ::  iX_Fit_To_Tab                   !< X-coordinatesindex correspondance from tabulated to fitted data
    real(rkp)   ,dimension( : )         ,allocatable                    ::  X_Tab                           !< Tabulated X-coordinates (DIM=NPtsTab)
    real(rkp)   ,dimension( : )         ,allocatable                    ::  X_Fit                           !< Fitted X-coordinates (DIM=NPtsFit)
    real(rkp)   ,dimension( : )         ,allocatable                    ::  Y_Tab                           !< Tabulated Y-coordinates (DIM=NPtsTab)
    real(rkp)   ,dimension( : )         ,allocatable                    ::  Y_Fit                           !< Fitted Y-coordinates (DIM=NPtsFit)
    real(rkp)   ,dimension( : )         ,allocatable                    ::  Y_Err                           !< Fitting error on Y-coordinates [%] (DIM=NPtsFit)
    real(rkp)                                                           ::  Y_ErrMax                        !< Max. fitting error on Y-coordinates [%]
    real(rkp)                                                           ::  RMSD                            !< Root-Mean-Square Deviation between tabulated and fitted data
    real(rkp)                                                           ::  NRMSD                           !< Normalized root-Mean-Square Deviation between tabulated and fitted data
    real(rkp)                                                           ::  CV_RMSD                         !< Coefficient of variation of the RMSD
    logical                                                             ::  Success                         !< Fitting success indicator
    logical                                                             ::  Defined_Y_Tab   =       .False.
    logical                                                             ::  Defined_X_Ini   =       .False.
    logical                                                             ::  Defined_X_Fin   =       .False.
    logical                                                             ::  Defined_iX_Ini  =       .False.
    logical                                                             ::  Defined_iX_Fin  =       .False.
  contains
    private
    procedure ,public   ::  Initialize    =>  InitializeFitter
    procedure ,public   ::  Evaluate      =>  EvaluateFitModel
    procedure ,public   ::  Fit           =>  FitData
!     procedure ,public   ::  SetProperties =>  SetFitterProperties
    procedure ,public   ::  Output_Summary
    procedure ,public   ::  SetXTab
    procedure ,public   ::  SetYTab
    procedure ,public   ::  SetModelParam
    procedure ,public   ::  SetFitModel
    procedure ,public   ::  ReadyToUse
    procedure ,public   ::  SetFitRange
    procedure ,public   ::  GetParameters
    procedure ,public   ::  GetNumberOfParameters
    procedure ,public   ::  Get_Matrix
    procedure ,public   ::  GetNParamMaxPerInterval
    procedure           ::  SolveLinearSystem
    procedure           ::  Set_Local_Index
    procedure           ::  Allocate_Fit_Data
    procedure           ::  CheckFitPreConditions
    procedure           ::  SetIntervals
    procedure           ::  Initialize_Constrains
  End Type

  Interface

    Module Subroutine InitializeFitter( This, X_Tab, Y_Tab, Intervals, FitModel, X_Ini, X_Fin, iX_Ini, iX_Fin, Debug )
      class(Fitter_Type)                                    ,intent(out)    ::  This              !< Passed-object dummy argument
      real(rkp)                                   ,optional ,intent(in)     ::  X_Tab(:)          !< Tabulated X-coordinates to be stored in the object
      real(rkp)                                   ,optional ,intent(in)     ::  Y_Tab(:)          !< Tabulated Y-coordinates to be stored in the object
      real(rkp)                                   ,optional ,intent(in)     ::  Intervals(:)      !< Fitting intervals
      class(FitModel_Type)                        ,optional ,intent(in)     ::  FitModel          !< Polymorphic FitModel object
      real(rkp)                                   ,optional ,intent(in)     ::  X_Ini             !< Value of initial temperature of fitting range
      real(rkp)                                   ,optional ,intent(in)     ::  X_Fin             !< Value of final temperature of fitting range
      integer                                     ,optional ,intent(in)     ::  iX_Ini            !< Index of initial temperature of fitting range
      integer                                     ,optional ,intent(in)     ::  iX_Fin            !< Index of final temperature of fitting range
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
    End Subroutine

!     Module Subroutine SetFitterProperties( This, X_Tab, Y_Tab, Intervals, FitModel, X_Ini, X_Fin, iX_Ini, iX_Fin, Debug )
!       class(Fitter_Type)                                    ,intent(inout)  ::  This              !< Passed-object dummy argument
!       real(rkp)                                   ,optional ,intent(in)     ::  X_Tab(:)          !< Tabulated X-coordinates to be stored in the object
!       real(rkp)                                   ,optional ,intent(in)     ::  Y_Tab(:)          !< Tabulated Y-coordinates to be stored in the object
!       real(rkp)                                   ,optional ,intent(in)     ::  Intervals(:)      !< Fitting intervals
!       class(FitModel_Type)                        ,optional ,intent(in)     ::  FitModel          !< Polymorphic FitModel object
!       real(rkp)                                   ,optional ,intent(in)     ::  X_Ini             !< Value of initial temperature of fitting range
!       real(rkp)                                   ,optional ,intent(in)     ::  X_Fin             !< Value of final temperature of fitting range
!       integer                                     ,optional ,intent(in)     ::  iX_Ini            !< Index of initial temperature of fitting range
!       integer                                     ,optional ,intent(in)     ::  iX_Fin            !< Index of final temperature of fitting range
!       logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
!     End Subroutine

    Module Subroutine SetXTab( This, X_Tab, Debug )
      class(Fitter_Type)                                    ,intent(inout)  ::  This              !< Passed-object dummy argument
      real(rkp)                                             ,intent(in)     ::  X_Tab(:)          !< Tabulated X-coordinates to be stored in the object
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
    End Subroutine

    Module Subroutine SetYTab( This, Y_Tab, Debug )
      class(Fitter_Type)                                    ,intent(inout)  ::  This              !< Passed-object dummy argument
      real(rkp)                                             ,intent(in)     ::  Y_Tab(:)          !< Tabulated X-coordinates to be stored in the object
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
    End Subroutine

    Module Subroutine SetModelParam( This, Param, Interval, Debug )
      class(Fitter_Type)                                    ,intent(inout)  ::  This              !< Passed-object dummy argument
      real(rkp)                                             ,intent(in)     ::  Param(:)
      integer                                     ,optional ,intent(in)     ::  Interval
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
    End Subroutine

    Module Subroutine SetIntervals( This, Intervals, FitModel, Debug )
      class(Fitter_Type)                                    ,intent(inout)  ::  This              !< Passed-object dummy argument
      real(rkp)                                   ,optional ,intent(in)     ::  Intervals(:)      !< Fitting intervals
      class(FitModel_Type)                        ,optional ,intent(in)     ::  FitModel          !< Polymorphic FitModel object
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
    End Subroutine

    Module Subroutine SetFitModel( This, FitModel, Debug )
      class(Fitter_Type)                                    ,intent(inout)  ::  This              !< Passed-object dummy argument
      class(FitModel_Type)                                  ,intent(in)     ::  FitModel          !< Polymorphic FitModel object
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
    End Subroutine

    Module Subroutine Initialize_Constrains( This, Debug )
      class(Fitter_Type)                                    ,intent(inout)  ::  This              !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
    End Subroutine

    Module Subroutine FitData( This, Y_Tab, Param, Param_, Debug )
      class(Fitter_Type)                                    ,intent(inout)  ::  This              !< Passed-object dummy argument
      real(rkp)                                   ,optional ,intent(in)     ::  Y_Tab(:)          !< Tabulated X-coordinates to be stored in the object
      real(rkp)   ,allocatable                    ,optional ,intent(out)    ::  Param(:)          !< Fit parameters (Solution)
      real(rkp)                                   ,optional ,intent(out)    ::  Param_(:)         !< Fit parameters (Solution)
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
    End Subroutine

    Pure Module Subroutine Output_Summary( This, Summary )
      class(Fitter_Type)                                    ,intent(in)     ::  This              !< Passed-object dummy argument
      character(:)  ,allocatable                            ,intent(out)    ::  Summary(:)        !< Character string corresponding to  header of the fitting error summary
    End Subroutine

    Pure Module Subroutine Allocate_Matrix( This, Matrix )
      class(Fitter_Type)                                    ,intent(in)     ::  This              !< Passed-object dummy argument
      real(rkp)   ,allocatable                              ,intent(out)    ::  Matrix(:,:)       !< Matrix used to construct the ThermoCoef object
    End Subroutine

    Pure Module Function GetParameters( This, Interval ) result(Param)
      class(Fitter_Type)                                    ,intent(in)     ::  This              !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  Interval
      real(rkp)   ,allocatable                                              ::  Param(:)
    End Function


    Pure Module Subroutine Get_Matrix( This, Matrix )
      class(Fitter_Type)                                    ,intent(in)     ::  This              !< Passed-object dummy argument
      real(rkp) ,dimension(:,:)                             ,intent(out)    ::  Matrix            !< Matri
    End Subroutine

    Pure Module Function GetNumberOfParameters( This, Interval ) result(NParam)
      class(Fitter_Type)                                    ,intent(in)     ::  This              !< Passed-object dummy argument
      integer                                     ,optional ,intent(in)     ::  Interval          !< Interval for which the number of parameter is requested
      integer                                                               ::  NParam            !< Number of parameters
    End Function

    Pure Module Function GetNParamMaxPerInterval( This ) result( NParamMax )
      class(Fitter_Type)                                    ,intent(in)     ::  This              !< Passed-object dummy argument
      integer                                                               ::  NParamMax         !< Maximum number of parameters
    End Function

    Module Subroutine Allocate_Fit_Data( This, Debug )
      class(Fitter_Type)                                    ,intent(inout)  ::  This              !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
    End Subroutine

    Module Subroutine SetFitRange( This, X_Ini, X_Fin, iX_Ini, iX_Fin, Debug )
      class(Fitter_Type)                                    ,intent(inout)  ::  This              !< Passed-object dummy argument
      real(rkp)                                   ,optional ,intent(in)     ::  X_Ini             !< Value of initial temperature of fitting range
      real(rkp)                                   ,optional ,intent(in)     ::  X_Fin             !< Value of final temperature of fitting range
      integer                                     ,optional ,intent(in)     ::  iX_Ini            !< Index of initial temperature of fitting range
      integer                                     ,optional ,intent(in)     ::  iX_Fin            !< Index of final temperature of fitting range
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
    End Subroutine

    Module Subroutine CheckFitPreConditions( This, Debug )
      class(Fitter_Type)                                    ,intent(inout)  ::  This              !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
    End Subroutine

    Module Subroutine SolveLinearSystem( This, Debug )
      class(Fitter_Type)                                    ,intent(inout)  ::  This              !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
    End Subroutine

    Module Subroutine Set_Local_Index( This, x, Debug )
      class(Fitter_Type)                                    ,intent(inout)  ::  This              !< Passed-object dummy argument
      real(rkp)                                             ,intent(in)     ::  x(:)              !> X-coordinates corresponding to the tabulated X value over the fitting range (DIM=NPtsFit)
      logical                                     ,optional ,intent(in)     ::  Debug             !< Debugging indicator
    End Subroutine

    Module Function EvaluateFitModel( This, x, Extrapolate ) result(y)
      class(Fitter_Type)                                    ,intent(in)     ::  This              !< Passed-object dummy argument
      real(rkp)                                             ,intent(in)     ::  x(:)              !< x-coordinates where to evaluate the curve
      logical                                     ,optional ,intent(in)     ::  Extrapolate       !< Extrapolation indicator (By default model extrapolation is performed)
      real(rkp)     ,dimension( size(x) )                                   ::  y                 !< y-coordinates of the curve evaluated at the input x
    End Function

    Module Function ReadyToUse( This ) result(Ready)
      class(Fitter_Type)                                    ,intent(in)     ::  This              !< Passed-object dummy argument
      logical                                                               ::  Ready
    End Function

  End Interface

End Module