Module FitModel_Class

  use iso_fortran_env   ,only:  rkp => REAL64

  implicit none

  private
  public        ::  FitModel_Type

  Type  ,abstract                                               ::  FitModel_Type
    logical                                                     ::  Initialized   = .False.
    logical                                                     ::  Param_Defined = .False.
    character(:)  ,allocatable                                  ::  Name                                      !< Name of the fit model
    integer                                                     ::  NParam  = 0                               !< Number of parameters of the fit model
    real(rkp)   ,dimension(:) ,allocatable                      ::  Param                                     !< Parameters of the model. These parameters corresponds to the fit coefficients, ie. to the unknowns 'x' of the (constrained) linear least-square system A x = b.
  contains
    private
    procedure                       ,public                     ::  Initialize  =>  InitializeFitModel
    procedure                       ,public                     ::  Free        =>  FreeFitModel
    generic                         ,public                     ::  Eval        =>  Evaluate_0d, Evaluate_1d, Evaluate_NoParam_0d, Evaluate_NoParam_1d
    procedure                       ,public                     ::  DeLinearize
    procedure                       ,public                     ::  Set_Param
    procedure                       ,public                     ::  Get_Param
    procedure                       ,public                     ::  Consistent_Param
    procedure ,non_overridable      ,public                     ::  Compute_RHS_Vector
    procedure ,non_overridable      ,public                     ::  Compute_LHS_Matrix
    procedure ,non_overridable      ,public                     ::  Compute_LHS_RowVector
    procedure ,non_overridable      ,public                     ::  Compute_LHS_Jacobian_Vector
!   Deferred procedure to be implemented by the extented types
    procedure(Getter_Name)          ,deferred ,nopass ,public   ::  Get_Name
    procedure(Getter_NParam)        ,deferred ,nopass ,public   ::  Get_NParam                    !< Gets the number of parameters associated to current Model
    procedure(Getter_RHS_Vector)    ,deferred ,nopass ,public   ::  Get_RHS_Vector                !< computes the column vector associated to the RHS vector b of the linear system A x = b.
    procedure(Evaluation_0d)        ,deferred ,nopass ,public   ::  Evaluate_0d
    procedure(Getter_LHS_RowVector) ,deferred         ,public   ::  Get_LHS_RowVector             !< gets the a row of the LHS matrix associated to the linear system A x = b
    procedure(Getter_LHS_RowVector_Jacobian),deferred ,public   ::  Get_LHS_RowVector_Jacobian    !< Used to applied a constrain on the derivative of the FitModel at the interface between 2 intervals.
!   Private procedures
    procedure ,non_overridable                                  ::  InitializeFitModel
    procedure ,non_overridable                        ,public   ::  Evaluate_NoParam_0d   ! @COMPILER_BUG gcc-6.3.1: In function 'Fitter_Type%Evaluate' the call to 'This%Intervals(k)%FitModel%Eval(x)' cause an ICE. Calling diretcly this procedure is a workaround
    procedure ,non_overridable                                  ::  Evaluate_NoParam_1d
    procedure ,non_overridable                                  ::  Evaluate_1d
  End Type

  Abstract Interface
    Pure Function Getter_Name() result(Name)
      character(:)      ,allocatable                          ::  Name
    End Function
    Pure Elemental Function Getter_NParam() result(NParam)
      integer                                                 ::  NParam
    End Function
    Pure Function Getter_LHS_RowVector( This, x ) result(A_Row)
      use iso_fortran_env   ,only:  rkp => REAL64
      import  FitModel_Type
      class(FitModel_Type)                    ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)                               ,intent(in)     ::  x                                 !> X coordinates
      real(rkp)     ,dimension( This%NParam )                 ::  A_Row                             !> Vector corresponding to a row of the LHS matrix associated to the linear system A x = b
    End Function
    Pure Elemental Function Getter_RHS_Vector( y ) result(b)
      use iso_fortran_env   ,only:  rkp => REAL64
      real(rkp)                               ,intent(in)     ::  y                                 !< Y coordinates values corresponding to the tabulated Y value over the fitting range (DIM=NPtsTab)
      real(rkp)                                               ::  b                                 !< RHS vector of the linear system A x = b
    End Function
    Pure Function Getter_LHS_RowVector_Jacobian( This, x ) result(dAdx_Row)
      use iso_fortran_env   ,only:  rkp => REAL64
      import  FitModel_Type
      class(FitModel_Type)                    ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)                               ,intent(in)     ::  x                                 !> X coordinates
      real(rkp)     ,dimension( This%NParam )                 ::  dAdx_Row                          !> Derivative of a single row of the LHS matrix of the linear system A x = b wrt the x-coordinate
    End Function
    Pure Function Evaluation_0d( x, Param ) result(y)
      use iso_fortran_env   ,only:  rkp => REAL64
      real(rkp)                             ,intent(in)     ::  x
      real(rkp) ,dimension(:)               ,intent(in)     ::  Param
      real(rkp)                                             ::  y
    End Function
  End Interface

  Interface
    Pure Module Subroutine InitializeFitModel( This, Param )
      class(FitModel_Type)                                  ,intent(inout)  ::  This                              !< Passed-object dummy argument
      real(rkp) ,dimension(:)                     ,optional ,intent(in)     ::  Param
    End Subroutine
    Pure Module Subroutine FreeFitModel( This )
      class(FitModel_Type)                                ,intent(inout)  ::  This                              !< Passed-object dummy argument
    End Subroutine
    Pure Module Subroutine Compute_LHS_Matrix( This, x, A )
      class(FitModel_Type)                                  ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)     ,dimension(:)                           ,intent(in)     ::  x                                 !< X coordinates values corresponding to the tabulated x value over the fitting range (DIM=NPtsTab)
      real(rkp)     ,dimension(:,:) ,allocatable            ,intent(out)    ::  A                                 !< LHS matrix of the linear system A x = b
    End Subroutine
    Pure Module Subroutine Compute_LHS_RowVector( This, x, A_Row )
      class(FitModel_Type)                                  ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
      real(rkp)     ,dimension(:) ,allocatable              ,intent(out)    ::  A_Row                             !> Vector corresponding to a row of the LHS matrix associated to the linear system A x = b
    End Subroutine
    Pure Module Subroutine Compute_RHS_Vector( This, y, b )
      class(FitModel_Type)                                  ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)     ,dimension(:)                           ,intent(in)     ::  y                                 !< Y coordinates values corresponding to the tabulated Y value over the fitting range (DIM=NPtsTab)
      real(rkp)     ,dimension(:) ,allocatable              ,intent(out)    ::  b                                 !< RHS vector of the linear system A x = b
    End Subroutine
    Pure Module Subroutine Compute_LHS_Jacobian_Vector( This, x, dAdx_Row )
      class(FitModel_Type)                                  ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
      real(rkp)     ,dimension(:)   ,allocatable            ,intent(out)    ::  dAdx_Row                          !> Derivative of a single row of the LHS matrix of the linear system A x = b wrt the x-coordinate
    End Subroutine
    Pure Module Subroutine DeLinearize( This )
      class(FitModel_Type)                                  ,intent(inout)  ::  This                              !< Passed-object dummy argument
    End Subroutine
    Pure Module Subroutine Set_Param( This, Param, Status )
      class(FitModel_Type)                                  ,intent(inout)  ::  This                              !< Passed-object dummy argument
      real(rkp)   ,dimension(:)                             ,intent(in)     ::  Param
      integer                                     ,optional ,intent(out)    ::  Status
    End Subroutine
    Pure Module Function Get_Param( This ) result(Param)
      class(FitModel_Type)                                  ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)   ,dimension( This%NParam )                                 ::  Param                             !< Array of parameters associated to current FitModel
    End Function
    Pure Module Function Evaluate_NoParam_0d( This, x ) result(y)
      class(FitModel_Type)                                  ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)                                             ,intent(in)     ::  x                                 !< x coordinates
      real(rkp)                                                             ::  y                                 !< Y coordinates to be computed (DIM=NPtsFit)
    End Function
    Pure Module Function Evaluate_NoParam_1d( This, x ) result(y)
      class(FitModel_Type)                                  ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)     ,dimension(:)                           ,intent(in)     ::  x                                 ! X coordinates (DIM=NPtsFit)
      real(rkp)     ,dimension(size(X))                                     ::  y                                 ! Y coordinates to be computed (DIM=NPtsFit)
    End Function
    Pure Module Function Evaluate_1d( This, x, Param ) result(y)
      class(FitModel_Type)                                  ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)     ,dimension(:)                           ,intent(in)     ::  x                                 ! X coordinates (DIM=NPtsFit)
      real(rkp)     ,dimension(:)                           ,intent(in)     ::  Param
      real(rkp)     ,dimension(size(X))                                     ::  y                                 ! Y coordinates to be computed (DIM=NPtsFit)
    End Function
    Pure Module Function Consistent_Param( This, Param ) result(Consistent)
      class(FitModel_Type)                                  ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)     ,dimension(:)                           ,intent(in)     ::  Param
      logical                                                               ::  Consistent
    End Function
  End Interface

End Module