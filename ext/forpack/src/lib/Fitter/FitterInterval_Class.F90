Module FitterInterval_Class

  use iso_fortran_env   ,only:  rkp => REAL64
  use FitModel_Class    ,only:  FitModel_Type

  implicit none

  private
  public                ::  FitterInterval_Type

  Type                                                                  ::  FitterInterval_Type
    logical                                                             ::  Bounded =   .False.               !< Indicator  whether current interval is bounded (ie, are Xmin and Xmax given a value ?)
    integer                                                             ::  Index   =   1                     !< Index of current interval in the list of all intervals
    real(rkp)                                                           ::  Xmin    =   0.0_rkp               !< Minimum x-value on current interval (Only set if 'Bounded' is true)
    real(rkp)                                                           ::  Xmax    =   0.0_rkp               !< Maximum x-value on current interval (Only set if 'Bounded' is true)
    integer                                                             ::  is                                !<
    integer                                                             ::  ie                                !<
    integer                                                             ::  iIniGlo                           !< Index of the global unknown associated to the first local unknown
    integer                                                             ::  iFinGlo                           !< Index of the global unknown associated to the last local unknown
    integer                                                             ::  M                                 !< Number of fitting positions (size of the input 'x')
    integer                                                             ::  N                                 !< Number of fit coeficients
    real(rkp)     ,dimension(:)   ,allocatable                          ::  x                                 !< X-coordinates corresponding to the tabulated X value over the current interval
    real(rkp)     ,dimension(:)   ,allocatable                          ::  y                                 !< Y-coordinates corresponding to the tabulated Y value over the current interval
    class(FitModel_Type)  ,allocatable                                  ::  FitModel                          !< Polymorphic 'FitModel' object
  contains
    procedure ,public   ::  SetFitModel
    procedure ,public   ::  InitializeLinearSystem
    procedure ,public   ::  Compute_A_b
    procedure ,public   ::  Update_A_b
    procedure ,public   ::  Set_MappingLocalToGlobalUnknows
    procedure ,public   ::  GetSummary
    procedure ,private  ::  Set_Local_Index
  End Type

  Interface
    Module Subroutine SetFitModel( This, FitModel, Debug )
      class(FitterInterval_Type)                            ,intent(inout)  ::  This                              !< Passed-object dummy argument
      class(FitModel_Type)                                  ,intent(in)     ::  FitModel                          !< Polymorphic 'FitModel' object
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine
    Module Subroutine Set_Local_Index( This, x )
      class(FitterInterval_Type)                            ,intent(inout)  ::  This                              !< Passed-object dummy argument
      real(rkp)     ,dimension(:)                           ,intent(in)     ::  x                                 !> X-coordinates corresponding to the tabulated X value over the fitting range (DIM=NPtsFit)
    End Subroutine
    Module Subroutine InitializeLinearSystem( This, x, y, Debug )

      class(FitterInterval_Type)                            ,intent(inout)  ::  This                              !< Passed-object dummy argument
      real(rkp)     ,dimension(:)                           ,intent(in)     ::  x                                 !> X-coordinates corresponding to the tabulated X value over the fitting range (DIM=NPtsFit)
      real(rkp)     ,dimension(:)                           ,intent(in)     ::  y                                 !> Y-coordinates corresponding to the tabulated Y value over the fitting range (DIM=NPtsFit)
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine
    Module Subroutine Update_A_b( This, A, b, Debug )
      class(FitterInterval_Type)                            ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)     ,dimension(:,:)                         ,intent(inout)  ::  A                                 !
      real(rkp)     ,dimension(:)                           ,intent(inout)  ::  B                                 !
      logical                                     ,optional ,intent(in)     ::  Debug                         !< Debugging indicator
    End Subroutine
    Module Subroutine Compute_A_b( This, A, b )
      class(FitterInterval_Type)                            ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)     ,dimension(:,:) ,allocatable            ,intent(out)    ::  A                                 ! Local LHS matrix: Contribution of current interval to the LHS matrix of the linear system A x = b
      real(rkp)     ,dimension(:)   ,allocatable            ,intent(out)    ::  b                                 ! Local RHS vector: Contribution of current interval to the RHS vector of the linear system A x = b
    End Subroutine
    Module Subroutine Set_MappingLocalToGlobalUnknows( This, Debug )
      class(FitterInterval_Type)                            ,intent(inout)  ::  This                              !< Passed-object dummy argument
      logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
    End Subroutine
    Pure Module Function GetSummary( This ) result(Summary)
      class(FitterInterval_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
      character(:)  ,allocatable                                            ::  Summary
    End Function
  End Interface

End Module