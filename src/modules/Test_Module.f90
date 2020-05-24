module Test_Module

use Input_Library
use Parameters_Library
use StringRoutines_Module
use StatisticsRoutines_Module
use ArrayIORoutines_Module
use ComputingRoutines_Module
use CommandRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverLAR_Class                                            ,only:    LinSolverLAR_Type
use LinSolverOMP_Class                                            ,only:    LinSolverOMP_Type
use LinSolverOLS_Class                                            ,only:    LinSolverOLS_Type
use CVKFold_Class                                                 ,only:    CVKFold_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use StopWatch_Class                                               ,only:    StopWatch, StopWatch_Type

implicit none

private

public                                                                ::    Test

logical, parameter                                                    ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Test(Input, Prefix)

  class(InputSection_Type), intent(in)                                ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='Test'
  integer                                                             ::    StatLoc=0
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  real(rkp), dimension(8,2)                                           ::    Response
  type(SMUQFile_Type)                                                 ::    File
  character(:), allocatable                                           ::    FileName
  real(rkp), allocatable, dimension(:,:)                              ::    VarR2D
  real(rkp), allocatable, dimension(:,:)                              ::    System
  real(rkp), allocatable, dimension(:)                                ::    Goal
  type(LinSolverOLS_Type)                                             ::    Solver
  real(rkp), allocatable, dimension(:)                                ::    Coefficients
  real(rkp)                                                           ::    CVerror
  integer                                                             ::    i
  type(StopWatch_Type)                                                ::    MainStopWatch
  real(rkp), allocatable, dimension(:,:)                              ::    Q
  real(rkp), allocatable, dimension(:,:)                              ::    R
  real(rkp), allocatable, dimension(:,:)                              ::    InvR
  type(CVKFold_Type)                                                  ::    CV

  FileName = '/home/rstkwsk2/workspace/runs/tests/ols_verification/case/longley.dat'
  call File%Construct(File=FileName)
  call ImportArray(File=File, Array=VarR2D, RowMajor=.true.)

  System = VarR2D(:,1:size(VarR2D,2)-1)

  Goal = VarR2D(:,size(VarR2D,2))

  allocate(Coefficients(size(System,2)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Coefficients', ProcName=ProcName, stat=StatLoc)
  Coefficients = Zero

  call CV%Construct(NbFolds=1000)

  call Solver%Construct(CVMethod=CV)

  call Solver%Solve(System=System, Goal=Goal, Coefficients=Coefficients, CVError=CVError)

  write(*,*) Coefficients
  write(*,*) CVError

  Q = System
  
  allocate(R(size(System,2),size(System,2)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='R', ProcName=ProcName, stat=StatLoc)
  R = Zero 
  InvR = R

  call ComputeQR(Q=Q, R=R)
  Q = System
  call computeQInvR(Q=Q, InvR=InvR)


  call Solver%SolveQR(System=System, Goal=Goal, Q=Q, R=R, Coefficients=Coefficients, CVError=CVError)

  write(*,*) Coefficients
  write(*,*) CVError

  call Solver%SolveQInvR(System=System, Goal=Goal, Q=Q, InvR=InvR, Coefficients=Coefficients, CVError=CVError)

  write(*,*) Coefficients
  write(*,*) CVError

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
