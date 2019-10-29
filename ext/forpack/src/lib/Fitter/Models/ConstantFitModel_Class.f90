Module ConstantFitModel_Class

  use iso_fortran_env   ,only:  rkp => REAL64
  use FitModel_Class    ,only:  FitModel_Type

  implicit none

  private
  public        ::  ConstantFitModel_Type

  Type  ,extends(FitModel_Type)                                 ::  ConstantFitModel_Type
  contains
    procedure   ,nopass                                         ::  Get_Name
    procedure   ,nopass                                         ::  Get_NParam
    procedure   ,nopass                                         ::  Evaluate_0d
    procedure   ,nopass                                         ::  Get_RHS_Vector
    procedure                                                   ::  Get_LHS_RowVector
    procedure                                                   ::  Get_LHS_RowVector_Jacobian
  End Type

  Interface
    Pure Module Function Get_Name() result(Name)
      character(:)  ,allocatable                                            ::  Name                              !< Name of the fitting function
    End Function
    Pure Elemental Module Function Get_NParam() result(NParam)
      integer                                                               ::  NParam                            !< Number of coefficient of the fitting function
    End Function
    Pure Module Function Evaluate_0d( x, Param ) result(y)
      real(rkp)                                             ,intent(in)     ::  x                                 !< x coordinates
      real(rkp) ,dimension(:)                               ,intent(in)     ::  Param                             !< Parameters of current model (correspond to the fit coefficients)
      real(rkp)                                                             ::  y                                 !< Y coordinates to be computed (DIM=NPtsFit)
    End Function
    Pure Module Function Get_LHS_RowVector( This, x ) result(A_Row)
      class(ConstantFitModel_Type)                          ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
      real(rkp)     ,dimension( This%NParam )                               ::  A_Row                             !> Vector corresponding to a row of the LHS matrix associated to the linear system A x = b
    End Function
    Pure Elemental Module Function Get_RHS_Vector( y ) result(b)
      real(rkp)                                             ,intent(in)     ::  y                                 !< Y coordinates values corresponding to the tabulated Y value over the fitting range (DIM=NPtsTab)
      real(rkp)                                                             ::  b                                 !< RHS vector of the linear system A x = b
    End Function
    Pure Module Function Get_LHS_RowVector_Jacobian( This, x ) result(dAdx_Row)
      class(ConstantFitModel_Type)                          ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)                                             ,intent(in)     ::  x                                 !> X coordinates
      real(rkp)     ,dimension( This%NParam )                               ::  dAdx_Row                          !> Derivative of a single row of the LHS matrix of the linear system A x = b wrt the x-coordinate
    End Function
  End Interface

End Module