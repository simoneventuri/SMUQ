SubModule(MultiVarPolyFitModel_Class) MultiVarPolyFitModel_SubClass

! Include file needed for: _ASSIGN_ALLOCATABLE_CHARACTER_ in InitializeMultiVarPolyFitModel
# include "forpack-include.inc"

  use Parameters_Library  ,only:  Zero, One
  use Logger_Class        ,only:  Logger, LogLevel_NOLOGS, LogLevel_DEBUG

  implicit none

  logical                                                               ::  DefaultDebug = .False.       !< Global debugging indicator

  contains

Module Procedure InitializeMultiVarPolyFitModel

  use String_Library      ,only:  Convert_To_String, VecTrim
  use Utilities_Library   ,only:  GetOptArgValue

  character(*)                                              ,parameter  ::  ProcName='InitializeMultiVarPolyFitModel'
  logical                                                               ::  Dbg
  integer                                                               ::  iP, iV, k, i, j, o, N
  integer                                                               ::  MaxOrder
  character(:)  ,allocatable                                            ::  VarName, sVar, sTerm

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  This%NVars    =   NVars
  This%NParam   =   NParam


  allocate( This%Param(This%NParam) ); This%Param = Zero

! ==============================================================================================================
!    SETTING THE TERM COEFFICIENTS
! ==============================================================================================================
!   Setting the coefficients associated to each term of the
!   polynomial model. The following terms are added:
!   - the constant term
!   - the uncoupled terms up to the order equal to the number of variables
!   - the coupled terms
!   Here is a representation of the Coef matrix

!     x1  x2  x3                            f(x1,x2,x3,...) =
!     ----------
!     0   0   0     Constant term                 1
!     ----------    Uncoupled terms
!     1   0   0     * 1st order terms           + x1
!     0   1   0        associated to            + x2
!     0   0   1           to i=1                + x3
!        ...                                    + ...
!     2   0   0     * 2nd order terms           + x1^2
!     0   2   0        associated to            + x2^2
!     0   0   2          to i=2                 + x3^2
!        ...                                    + ...
!     3   0   0     * 3rd order terms           + x1^3
!     0   3   0        associated to            + x2^3
!     0   0   3          to i=3                 + x3^3
!        ...                                    + ...
!     ----------    Coupled terms
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Setting term coefficients" )
  if ( present(Coef) ) then
    allocate( This%Coef, source = Coef )
  else
    if ( allocated( This%Coef) ) deallocate(This%Coef)
    allocate( This%Coef(This%NVars,This%NParam) )
    This%Coef   =   0

!     MaxOrder      =   2
!     if (Dbg) call Logger%Write( "Experimental:" )
!   Setting the constant
    k                   =   1
    This%Coef(:,k)      =   0 ! Constant
!     if (Dbg) call Logger%Write( "-> k = ", k, "This%Coef(:,k) = ", This%Coef(:,k), F2="i3" )

!   Setting the uncoupled terms: {xi,xi^2,xi^3...} for i=1:NVars
    do i = 1,This%NVars
      do j = 1,This%NVars
        k               =   k + 1
        This%Coef(j,k)  =   i
!         if (Dbg) call Logger%Write( "-> k = ", k, "This%Coef(:,k) = ", This%Coef(:,k), F2="i3" )
      end do
    end do

!     do i = 1,This%NVars
!       do j = 1,This%NVars
!         if ( i == j ) cycle
!         k               =   k + 1
!         This%Coef(i,k)  =   i
!         This%Coef(j,k)  =   1
!         if (Dbg) call Logger%Write( "-> k = ", k, "This%Coef(:,k) = ", This%Coef(:,k), F2="i3" )
!       end do
!     end do

    if ( allocated( This%Coef) ) deallocate(This%Coef)
    allocate( This%Coef(This%NVars,This%NParam) )
    select case (This%NVars)
    case (1)
      This%Coef(1,1)   =   0 ! 1
      This%Coef(1,2)   =   1 ! x1
      This%Coef(1,3)   =   2 ! x1^2
!       do k = 0,MaxOrder
!         This%Coef(1,k-1)   =   k
!       end do
    case (2)
      This%Coef(1:2,1)   =   [0,0] ! 1
      This%Coef(1:2,2)   =   [1,0] ! x1
      This%Coef(1:2,3)   =   [0,1] ! x2
      This%Coef(1:2,4)   =   [2,0] ! x1^2
      This%Coef(1:2,5)   =   [0,2] ! x2^2
      This%Coef(1:2,6)   =   [1,1] ! x1 x x2
!     case (3)                            ! Term      Order
!       This%Coef(1:3,1)   =   [0,0,0]    ! 1         0
!       This%Coef(1:3,2)   =   [1,0,0]    ! x1        1
!       This%Coef(1:3,3)   =   [0,1,0]    ! x2        1
!       This%Coef(1:3,4)   =   [0,0,1]    ! x3        1
!       This%Coef(1:3,5)   =   [2,0,0]    ! x1^2      2
!       This%Coef(1:3,6)   =   [0,2,0]    ! x2^2      2
!       This%Coef(1:3,7)   =   [0,0,2]    ! x3^2      2
!       This%Coef(1:3,8)   =   [3,0,0]    ! x1^3      3
!       This%Coef(1:3,9)   =   [0,3,0]    ! x2^3      3
!       This%Coef(1:3,10)  =   [0,0,3]    ! x3^3      3
!       This%Coef(1:3,11)  =   [1,1,0]    !           2
!       This%Coef(1:3,12)  =   [1,0,1]    !           2
!       This%Coef(1:3,13)  =   [0,1,1]    !           2
!       This%Coef(1:3,14)  =   [2,1,0]    !           3
!       This%Coef(1:3,15)  =   [2,0,1]    !           3
!       This%Coef(1:3,16)  =   [1,2,0]    !           3
!       This%Coef(1:3,17)  =   [0,2,1]    !           3
!       This%Coef(1:3,18)  =   [0,1,2]    !           3
!       This%Coef(1:3,19)  =   [1,0,2]    !           3
!       This%Coef(1:3,20)  =   [1,1,1]    !           3

    end select
!     if (Dbg) then
!       call Logger%Write( "Correct:" )
!       do k = 1,size(This%Coef,2)
!         call Logger%Write( "-> k = ", k, "This%Coef(:,k) = ", This%Coef(:,k), F2="i3" )
!       end do
!      end if
  end if
  if ( Dbg ) then
    do k = 1,size(This%Coef,2)
      call Logger%Write( "-> k = ", k, "This%Coef(:,k) = ", This%Coef(:,k), F2="i3" )
    end do
  end if
! ==============================================================================================================


! ==============================================================================================================
!    SETTING VARIABLES NAMES
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Setting variables names" )
  allocate( Character(10) :: This%VarNames(This%NVars) )
  if ( present(VarNames) ) then
    do iV = 1,This%NVars
      This%VarNames(iV) = trim(VarNames(iV))
    end do
  else
    do iV = 1,This%NVars
      This%VarNames(iV) = "x"//Convert_To_String(iV)
    end do
  end if
!   This%VarNames   =   VecTrim(This%VarNames)

# ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
    _ASSIGN_ALLOCATABLE_CHARACTER_(This%VarNames,VecTrim(This%VarNames))
# else
    This%VarNames   =   VecTrim(This%VarNames)
# endif

  if (Dbg) call Logger%Write( "-> This%VarNames = ", This%VarNames )
! ==============================================================================================================


! ==============================================================================================================
!    SETTING TERMS NAMES
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Setting terms names" )
  allocate( Character(10) :: This%TermNames(This%NParam) )
  This%TermNames(:) = ""
  do iP = 1,This%NParam
    sTerm       =   ""
    do iV = 1,This%NVars
      VarName   =   trim(This%VarNames(iV))
      k         =   This%Coef(iV,iP)
      sVar      =   ""
      select case (k)
        case (1);  sVar = VarName
        case (2:); sVar = VarName//"^"//Convert_To_String(k)
      end select
      if ( sVar == "" ) cycle
      if ( sTerm/="") sTerm   =   sTerm // "*"
      sTerm   =   sTerm // sVar
    end do
    This%TermNames(iP)  =   sTerm
  end do

# ifdef WORKAROUND_GFORTRAN_ASSIGN_ALLOCATABLE_CHARACTER
    _ASSIGN_ALLOCATABLE_CHARACTER_(This%TermNames,VecTrim(This%TermNames))
# else
    This%TermNames   =   VecTrim(This%TermNames)
# endif
  if (Dbg) call Logger%Write( "-> This%TermNames = ", This%TermNames )
! ==============================================================================================================


! ==============================================================================================================
!    ALLOCATING DATA
! ==============================================================================================================
  if (Dbg) call Logger%Write( "Allocating data" )
  N           =   This%NParam * (This%NParam - 1) / 2
  allocate( This%R(      N          ) )
  allocate( This%D(      This%NParam) )
  allocate( This%RHS(    This%NParam) )
  allocate( This%TOL(    This%NParam) )
  allocate( This%RSS(    This%NParam) )
  allocate( This%VORDER( This%NParam) )
  allocate( This%ROW_PTR(This%NParam) )
  This%TOL          =   Zero
  This%RSS          =   Zero
  This%ROW_PTR      =   0
  This%VORDER       =   0
  This%VORDER(:)    =   [(i-1,i=1,This%NParam)]
  This%ROW_PTR(1)   =   1   ! row_ptr(i) is the position of element This%R(i,i+1) in array R().
  do i = 2, This%NParam-1
    This%ROW_PTR(i) =   This%ROW_PTR(i-1) + This%NParam - i + 1
  end do
  This%ROW_PTR(This%NParam) = 0
  This%Initialized  =   .True.
  This%TOL_SET      =   .False.
  call This%ResetPoints()
! ==============================================================================================================

  if (Dbg) call Logger%Exiting()

End Procedure


Module Procedure ResetPoints
  This%RSS_SET  =   .False.
  This%NOBS     =   0
  This%R        =   Zero
  This%D        =   Zero
  This%RHS      =   Zero
  This%SSERR    =   Zero
End Procedure


Module Procedure SetTabulatedData
  real(rkp)                                                 ,parameter  ::  DefaultWeight = One
  integer                                                               ::  i
  real(rkp)                                                             ::  Weight_, xi(This%NParam)
  call This%ResetPoints()
  Weight_   =   DefaultWeight
  do i = 1,size(y)
    xi(:)   =   This%ComputeTerms( x(:,i) )
    if ( present(Weight) ) Weight_ = Weight(i)
    call This%AddPoint( xi(:), y(i), Weight=Weight_ )
  end do
End Procedure

Module Procedure FitData
  real(rkp)   ,allocatable                                              ::  AllocParam(:)

  if ( present(x) .and. present(y) ) call This%SetTabulatedData( x, y, Weight=Weight )

  call regcf( This, This%Param, This%NParam, This%iStat )

  if ( present(Param) ) Param = This%GetParam()

  if ( present(Param_) ) then
  Block
    use Parameters_Library    ,only:  Zero
    integer                     ::  i
    real(rkp)   ,allocatable    ::  AllocParam(:)
    AllocParam  =   This%GetParam()
    Param_(:)   =   Zero
    do i = 1,min( size(Param_), size(AllocParam) )
      Param_(i) =   AllocParam(i)
    end do
  End Block
  end if


End Procedure

Module Procedure GetParam
  Param   =   This%Param
End Procedure


Module Procedure ComputeTerms
  integer                                                               ::  iP, iV
  do iP = 1,This%NParam
    Xt(iP) =   One
    do iV = 1,This%NVars
      Xt(iP) =   Xt(iP) * Xv(iV)**This%Coef(iV,iP)
    end do
  end do
End Procedure

Module Procedure GetFunctionString
  use String_Library      ,only:  Convert_To_String
  integer                                                               ::  iP, iV
  character(:)  ,allocatable                                            ::  Number, str, Fmt, FctName
!   Fmt       =   "f8.4"
  Fmt       =   "es15.8"
  FctName   =   "f("
  do iV = 1,This%NVars
    FctName =   FctName // trim(This%VarNames(iV))
    if ( iV /= This%NVars ) FctName = FctName // ","
  end do
  FctName =   FctName // ") = "
  iP      =   1
  Number  =   Convert_To_String(abs(This%Param(iP)),Fmt=Fmt )
  str     =   ""
  if ( This%Param(iP) < 0 ) str = "- "
  String  =   str // Number

  do iP = 2,This%NParam
    Number  =   Convert_To_String(abs(This%Param(iP)),Fmt=Fmt )
    Number  =   trim( adjustl( Number ))
    str     =   " + "
    if ( This%Param(iP) < 0 ) str = " - "
    String  =   String // str // Number//"*"//trim(This%TermNames(iP))
  end do
  String    =   FctName // String
End Procedure



Module Procedure Evaluate_0d
  real(rkp)   ,dimension(This%NParam)                                   ::  xi
  real(rkp)   ,dimension(This%NParam)                                   ::  Param_
  if ( present(Param) ) then
    Param_  =   Param
  else
    Param_  =   This%Param
  end if
  xi  =   This%ComputeTerms( x(:) )
  y   =   dot_product( Param_, xi )
End Procedure

Module Procedure Evaluate_1d
  integer                                                               ::  i
  do i = 1,size(y)
    y(i)    =   This%Eval( x(:,i), Param=Param )
  end do
End Procedure


Module Procedure GetNumberOfParameters
  NParam  =   This%NParam
End Procedure


End SubModule