Module MultiVarPolyFitModel_Class

  use Parameters_Library  ,only:  rkp

  implicit none

  private
  public  ::  MultiVarPolyFitModel_Type

  Type                          ::  MultiVarPolyFitModel_Type
!     logical                     ::  Initialized   = .False.
    logical                     ::  Param_Defined = .False.
    character(:)  ,allocatable  ::  Name
!   ============================
    integer                     ::  iStat
    integer                     ::  NVars   =   0
    integer                     ::  NParam  =   0
    integer       ,allocatable  ::  Coef(:,:)     ! Dim=(NVars,NParam)
    character(:)  ,allocatable  ::  TermNames(:)  ! Dim=(NParam)
    character(:)  ,allocatable  ::  VarNames(:)   ! Dim=(NVars)
    real(rkp)     ,allocatable  ::  Param(:)      ! Dim=(NParam)
!   ============================
    integer                     ::  NOBS
    integer ,allocatable        ::  VORDER(:), ROW_PTR(:)
    logical                     ::  INITIALIZED = .False.
    logical                     ::  TOL_SET     = .False.
    logical                     ::  RSS_SET     = .False.
    real(rkp) ,allocatable      ::  D(:), RHS(:), R(:), TOL(:), RSS(:)
    real(rkp)                   ::  SSERR, TOLY
!   ============================
  contains
    private
    procedure ,public ::  Initialize  =>  InitializeMultiVarPolyFitModel
    procedure ,public ::  Fit         =>  FitData
    procedure ,public ::  ComputeTerms
    procedure ,public ::  GetFunctionString
    procedure ,public ::  SetTabulatedData
    procedure ,public ::  GetParam
    procedure   ,public   ::  GetNumberOfParameters

    generic   ,public ::  Eval        =>  Evaluate_0d, Evaluate_1d
    procedure         ::  Evaluate_0d   ! deferred ,nopass
    procedure         ::  Evaluate_1d
!     procedure         ::  Evaluate_NoParam_1d
!   ============================
    procedure ,public ::    AddPoint
    procedure ,public ::    ResetPoints
  End Type

  Interface

    Module Subroutine InitializeMultiVarPolyFitModel( This, NVars, NParam, VarNames, Coef, Debug )
      class(MultiVarPolyFitModel_Type)                      ,intent(out)    ::  This
      integer                                               ,intent(in)     ::  NVars
      integer                                               ,intent(in)     ::  NParam
      character(*)  ,dimension(:)                 ,optional ,intent(in)     ::  VarNames
      integer       ,dimension(:,:)               ,optional ,intent(in)     ::  Coef
      logical                                     ,optional ,intent(in)     ::  Debug
    End Subroutine

    Module Subroutine SetTabulatedData( This, x, y, Weight )
      class(MultiVarPolyFitModel_Type)                      ,intent(inout)  ::  This
      real(rkp)   ,dimension(:,:)                           ,intent(in)     ::  x             ! Dim=(NVars,NPoints)
      real(rkp)   ,dimension(:)                             ,intent(in)     ::  y             ! Dim=(NPoints)
      real(rkp)                                   ,optional ,intent(in)     ::  Weight(:)     ! Dim=(NPoints)

    End Subroutine

    Module Subroutine FitData( This, x, y, Weight, Param, Param_ )
      class(MultiVarPolyFitModel_Type)                      ,intent(inout)  ::  This
      real(rkp)                                   ,optional ,intent(in)     ::  x(:,:)        ! Dim=(NVars,NPoints)
      real(rkp)                                   ,optional ,intent(in)     ::  y(:)          ! Dim=(NPoints)
      real(rkp)                                   ,optional ,intent(in)     ::  Weight(:)     ! Dim=(NPoints)
      real(rkp)   ,allocatable                    ,optional ,intent(out)    ::  Param(:)      ! Dim=(NParam)
      real(rkp)                                   ,optional ,intent(out)    ::  Param_(:)     !< Fit parameters (Solution)
    End Subroutine

    Module Subroutine ResetPoints( This )
      class(MultiVarPolyFitModel_Type)                      ,intent(inout)  ::  This
    End Subroutine

    Pure Module Function GetParam( This ) result(Param)
      class(MultiVarPolyFitModel_Type)                      ,intent(in)     ::  This                              !< Passed-object dummy argument
      real(rkp)   ,dimension( This%NParam )                                 ::  Param                             !< Array of parameters associated to current FitModel
    End Function


    Pure Module Function GetNumberOfParameters( This ) result(NParam)
      class(MultiVarPolyFitModel_Type)                      ,intent(in)     ::  This                              !< Passed-object dummy argument
      integer                                                               ::  NParam            !< Number of parameters
    End Function

    Module Pure Function ComputeTerms( This, Xv ) result(Xt)
      class(MultiVarPolyFitModel_Type)                      ,intent(in)     ::  This
      real(rkp)   ,dimension(:)                             ,intent(in)     ::  Xv          ! X values associated to each variable. Dim=(NVars)
      real(rkp)   ,dimension(This%NParam)                                   ::  Xt          ! X values associated to each term. Dim=(NParam)
    End Function

    Module Pure Function GetFunctionString( This ) result(String)
      class(MultiVarPolyFitModel_Type)                      ,intent(in)     ::  This
      character(:)  ,allocatable                                            ::  String
    End Function




    Pure Module Function Evaluate_0d( This, x, Param ) result(y)
      class(MultiVarPolyFitModel_Type)                      ,intent(in)     ::  This
      real(rkp)                                             ,intent(in)     ::  x(:)      !< X coordinates. Dim=(NVars)
      real(rkp)                                   ,optional ,intent(in)     ::  Param(:)  !< Fit parameter to be used for the evaluation Dim=(NParam)
      real(rkp)                                                             ::  y         !< Y coordinates.
    End Function

    Pure Module Function Evaluate_1d( This, x, Param ) result(y)
      class(MultiVarPolyFitModel_Type)                      ,intent(in)     ::  This
      real(rkp)                                             ,intent(in)     ::  x(:,:)          !< X coordinates. Dim=(NVars,NPoints)
      real(rkp)                                   ,optional ,intent(in)     ::  Param(:)        !< Fit parameter to be used for the evaluation Dim=(NParam)
      real(rkp)                                                             ::  y(size(x,2))    !< Y coordinates. Dim=(NPoints)
    End Function





    Module Subroutine AddPoint( This, xi, y, Weight )
      class(MultiVarPolyFitModel_Type)                      ,intent(inout)  ::  This
      real(rkp)                                             ,intent(in)     ::  xi(:)   ! X associated to terms
      real(rkp)                                             ,intent(in)     ::  y
      real(rkp)                                   ,optional ,intent(in)     ::  Weight
    End Subroutine


















    Module Subroutine REGCF( This, BETA, NREQ, IFAULT )
      type(MultiVarPolyFitModel_Type)                       ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  NREQ
      integer                                               ,intent(out)    ::  IFAULT
      real(rkp) ,dimension(:)                               ,intent(out)    ::  BETA
    End Subroutine

    Module Subroutine TOLSET( This, EPS )
      type(MultiVarPolyFitModel_Type)                       ,intent(inout)  ::  This
      real(rkp)                                   ,optional ,intent(in)     ::  EPS
    End Subroutine

    Module Subroutine SING( This, LINDEP, IFAULT )
      type(MultiVarPolyFitModel_Type)                       ,intent(inout)  ::  This
      integer                                               ,intent(out)    ::  IFAULT
      logical ,dimension(:)                                 ,intent(out)    ::  LINDEP
    End Subroutine

    Module Subroutine SS( This )
      type(MultiVarPolyFitModel_Type)                       ,intent(inout)  ::  This
    End Subroutine

    Module Subroutine COV( This, NREQ, VAR, COVMAT, DIMCOV, STERR, IFAULT )
      type(MultiVarPolyFitModel_Type)                       ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  NREQ
      real(rkp)                                             ,intent(out)    ::  VAR
      real(rkp) ,dimension(:)                               ,intent(out)    ::  COVMAT
      integer                                               ,intent(in)     ::  DIMCOV
      real(rkp) ,dimension(:)                               ,intent(out)    ::  STERR
      integer                                               ,intent(out)    ::  IFAULT
    End Subroutine

    Module Subroutine INV( This, NREQ, RINV)
      type(MultiVarPolyFitModel_Type)                       ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  NREQ
      real(rkp) ,dimension(:)                               ,intent(out)    ::  RINV
    End Subroutine

    Module Subroutine PARTIAL_CORR( This, INVAR, CORMAT, DIMC, YCORR, IFAULT)
      type(MultiVarPolyFitModel_Type)                       ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  INVAR, DIMC
      integer                                               ,intent(out)    ::  IFAULT
      real(rkp) ,dimension(:)                               ,intent(out)    ::  CORMAT, YCORR
    End Subroutine

    Module Subroutine VMOVE( This, FROM, DEST, IFAULT)
      type(MultiVarPolyFitModel_Type)                       ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  FROM, DEST
      integer                                               ,intent(out)    ::  IFAULT
    End Subroutine

    Module Subroutine REORDR( This, LIST, N, POS1, IFAULT)
      type(MultiVarPolyFitModel_Type)                       ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  N, POS1
      integer ,dimension(:)                                 ,intent(in)     ::  LIST
      integer                                               ,intent(out)    ::  IFAULT
    End Subroutine

    Module Subroutine HDIAG( This, XROW, NREQ, HII, IFAULT)
      type(MultiVarPolyFitModel_Type)                       ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  NREQ
      real(rkp) ,dimension(:)                               ,intent(in)     ::  XROW
      real(rkp)                                             ,intent(out)    ::  HII
      integer                                               ,intent(out)    ::  IFAULT
    End Subroutine

    Module Subroutine VARPRD( This, X, NREQ, FN_VAL)
      type(MultiVarPolyFitModel_Type)                       ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  NREQ
      real(rkp) ,dimension(:)                               ,intent(in)     ::  X
      real(rkp)                                             ,intent(out)    ::  FN_VAL
    End Subroutine

    Module Subroutine BKSUB2( This, X, B, NREQ)
      type(MultiVarPolyFitModel_Type)                       ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  NREQ
      real(rkp) ,dimension(:)                               ,intent(in)     ::  X
      real(rkp) ,dimension(:)                               ,intent(out)    ::  B
    End Subroutine

  End Interface


End Module