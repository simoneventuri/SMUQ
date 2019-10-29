! -*-f90-*-
!!--------------------------------------------------------------------------------------------------------------------------------
!!
!! Stochastic Modeling & Uncertainty Quantification (SMUQ)
!!
!! Copyright (C) 2016 Venturi, Simone & Rostkowski, Przemyslaw (University of Illinois at Urbana-Champaign)
!!
!! This program is free software; you can redistribute it and/or modify it under the terms of the Version 2.1 GNU Lesser General
!! Public License as published by the Free Software Foundation.
!!
!! This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
!!
!! You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to the Free 
!! Software Foundation, Inc. 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
!!
!!--------------------------------------------------------------------------------------------------------------------------------

module LARMethod_Class

use Input_Library
use Parameters_Library
use StatisticsRoutines_Module
use ComputingRoutines_Module
use QRUpdate_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use LinSolverSparse_Class                                         ,only:    LinSolverSparse_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use LinkedList1D_Class                                            ,only:    LinkedList1D_Type

implicit none

private

public                                                                ::    LARMethod_Type

type, abstract, extends(LinSolverSparse_Type)                         ::    LARMethod_Type
  logical                                                             ::    LASSO=.false.
  real(rkp)                                                           ::    Tolerance=Zero
contains
  procedure, public                                                   ::    BuildMetaModels
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  ! Assumes that the first column is the mean
  subroutine BuildMetaModels( This, System, Goal, ModelSet, CoefficientsSet, AddDrop, Tolerance, ConstantModel, Debug )

    class(LARMethod_Type), intent(in)                                 ::    This
    real(rkp), dimension(:,:), target, intent(inout)                  ::    System
    real(rkp), dimension(:), intent(inout)                            ::    Goal
    integer, allocatable, dimension(:), optional, intent(out)         ::    ModelSet
    integer, allocatable, dimension(:), optional, intent(out)         ::    AddDrop
    real(rkp), allocatable, dimension(:), optional, intent(out)       ::    CoefficientsSet
    real(rkp), optional, intent(in)                                   ::    Tolerance
    logical, optional, intent(out)                                    ::    ConstantModel
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='BuildMetaModels'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:)                              ::    Residual
    real(rkp), allocatable, dimension(:)                              ::    DescentDir
    real(rkp)                                                         ::    DescentStep
    real(rkp), allocatable, dimension(:,:)                            ::    Q1
    real(rkp), allocatable, target, dimension(:,:)                    ::    R
    real(rkp), allocatable, dimension(:)                              ::    WORK
    real(rkp)                                                         ::    c
    real(rkp)                                                         ::    ci
    real(rkp)                                                         ::    di
    real(rkp)                                                         ::    CorrMax
    real(rkp)                                                         ::    CorrMaxTemp
    integer                                                           ::    CorrMaxLoc
    real(rkp), allocatable, dimension(:)                              ::    s
    real(rkp), allocatable, dimension(:)                              ::    InvRpS
    real(rkp)                                                         ::    VarR0D
    real(rkp)                                                         ::    LASSOWORK
    integer                                                           ::    VarI0D
    real(rkp), allocatable, dimension(:)                              ::    u
    integer                                                           ::    M
    integer                                                           ::    N
    integer                                                           ::    P
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    im1
    integer                                                           ::    iim1
    integer                                                           ::    iii
    integer                                                           ::    Drop
    logical                                                           ::    NoDrop=.true.
    real(rkp), allocatable, dimension(:)                              ::    CoefficientsTemp
    real(rkp)                                                         ::    GoalMean
    real(rkp)                                                         ::    GoalVariance
    real(rkp), allocatable, dimension(:)                              ::    Mean
    real(rkp), allocatable, dimension(:)                              ::    Norm
    integer, allocatable, dimension(:)                                ::    ActiveSet
    logical, allocatable, dimension(:)                                ::    Active
    integer                                                           ::    NbActive
    integer                                                           ::    NewActive
    real(rkp), allocatable, dimension(:)                              ::    Correlations
    real(rkp)                                                         ::    MinCorr=1e-10
    real(8), external                                                 ::    DNRM2
    type(LinkedList0D_Type)                                           ::    AddDropLoc
    integer                                                           ::    DropIndex
    real(rkp)                                                         ::    DropSign
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    real(rkp)                                                         ::    WNorm
    real(rkp)                                                         ::    ToleranceLoc
    integer                                                           ::    ConstantIndex

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    ToleranceLoc = Zero
    if ( present(Tolerance) ) ToleranceLoc = Tolerance

    M = size(System,1)
    N = size(System,2)

    P = min(M-1,N)

    ! standardizing predictors and centering goal

    allocate(Mean(N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Mean', ProcName=ProcName, stat=StatLoc )

    allocate(Norm(N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Norm', ProcName=ProcName, stat=StatLoc )

    allocate(Residual, source=Goal, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Residual', ProcName=ProcName, stat=StatLoc )

    allocate(CoefficientsTemp(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='CoefficientsSetTemp', ProcName=ProcName, stat=StatLoc )
    CoefficientsTemp = Zero

    allocate(DescentDir(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DescentDir', ProcName=ProcName, stat=StatLoc )
    DescentDir = Zero

    allocate(s(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='s', ProcName=ProcName, stat=StatLoc )
    s = Zero

    allocate(u(M), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='u', ProcName=ProcName, stat=StatLoc )
    u = Zero

    allocate(InvRpS(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='InfoInvxS', ProcName=ProcName, stat=StatLoc )
    InvRpS = Zero

    allocate(Q1(M,P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Q1', ProcName=ProcName, stat=StatLoc )
    Q1 = Zero

    allocate(R(P,P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='R', ProcName=ProcName, stat=StatLoc )
    R = Zero

    allocate(ActiveSet(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='ActiveSet', ProcName=ProcName, stat=StatLoc )
    ActiveSet = 0

    allocate(Active(N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Active(N)', ProcName=ProcName, stat=StatLoc )
    Active = .false.

    allocate(Correlations(N), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Correlations', ProcName=ProcName, stat=StatLoc )
    Correlations = Zero

    allocate(VarR1D(P), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
    VarR1D = Zero

    ConstantIndex = 0

    i = 1
    do i = 1, N
      Mean(i) = ComputeMean( Values=System(:,i) )
      System(:,i) = System(:,i) - Mean(i)
      Norm(i) = DNRM2(M,System(:,i),1)
      if ( abs((Norm(i)**2/real(M,rkp))/Mean(i)) > 1e-10 ) then
        System(:,i) = System(:,i)/ Norm(i)
      else
        Norm(i) = One
        if ( ConstantIndex == 0 ) ConstantIndex = i
        Active(i) = .true.
        System(:,i) = Zero
      end if
    end do

    GoalMean = ComputeMean( Values=Goal )
    GoalVariance = ComputeSampleVar( Values=Goal, Mean=GoalMean )
    Goal = Goal - GoalMean

    if ( present(AddDrop) .and. ConstantIndex /= 0 ) call AddDropLoc%Append( Value=ConstantIndex )

    if ( present(ConstantModel) ) ConstantModel = .false.

    if ( abs((GoalVariance*real((M-1),rkp)/real(M,rkp))/GoalMean) < 1e-10 .and. ConstantIndex /= 0 ) then
      if ( present(ModelSet) ) then
        allocate(ModelSet(1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='ModelSet', ProcName=ProcName, stat=StatLoc )
        ModelSet = ConstantIndex
      end if
      if ( present(CoefficientsSet) ) then
        allocate(CoefficientsSet(1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Coefficients', ProcName=ProcName, stat=StatLoc )
        CoefficientsSet = GoalMean / Mean(ConstantIndex)
      end if
      if ( present(ConstantModel) ) ConstantModel = .true.
    else
      if ( GoalVariance <= Zero ) GoalVariance = tiny(One)

      NbActive = 0
      NewActive = 0
      NoDrop = .true.

      i = 0
      do 
        if ( NoDrop ) i = i + 1
        ! Computing current correlations with residual
        im1 = i - 1

        ii = 1
        do ii = 1, N
          if ( Active(ii) .or. ii == NewActive ) cycle
          Correlations(ii) = dot_product(System(:,ii),Residual)
        end do

        if ( i == 1 .and. NoDrop ) then
          CorrMaxLoc = maxloc(abs(Correlations),1)
          CorrMax = abs(Correlations(CorrMaxLoc))
          NewActive = CorrMaxLoc
        end if

        if ( CorrMax < MinCorr ) exit

        if ( NoDrop ) then
          if ( present(AddDrop) ) call AddDropLoc%Append( Value=NewActive )
          Active(NewActive) = .true.
          ActiveSet(i) = NewActive
          NbActive = NbActive+1
          s(i) = sign(One,Correlations(NewActive))

          ! updating QR
          if ( i > 1 ) then
            call DGEMV( 'T', M, im1, 1.d0, Q1(:,1:im1), M, System(:,ActiveSet(i)), 1, 0.d0, VarR1D(1:im1), 1 )

            WNorm = ComputeNorm(Vector=VarR1D(1:im1), Norm=2)
            VarR0D = dsqrt(One - WNorm**2)

            ii = 1
            do ii = 1, M
              Q1(ii,i) = (System(ii,ActiveSet(i)) - sum(Q1(ii,1:im1)*VarR1D(1:im1))) / VarR0D
            end do

            R(1:im1,i) = VarR1D(1:im1)
            R (i,i) = VarR0D
          else
            Q1(:,1) = System(:,ActiveSet(i))
            R(1,1) = One
          end if

          ! keeping record of invRpS saves a lot of computations
          if ( i == 1 ) then
            invRpS(1) = s(1) / R(1,1)
          else
            invRpS(i) =  ( s(i) - dot_product( R(1:im1,i), invRpS(1:im1) ) ) / R(i,i)
          end if
        else
          if ( present(AddDrop) ) call AddDropLoc%Append( Value=-(Drop) )
        end if

        ! computing step direction
        ! inv(A'A)*s = VarR1D
        if ( i == 1 ) then
          VarR1D(1) = invRpS(1) / R(1,1)
        else
          VarR1D(i) = invRpS(i) / R(i,i)
          ii = 1
          do ii = 1, im1
            VarI0D = i-ii
            iim1 = ii-1
            VarR1D(VarI0D) = ( invRpS(VarI0D) - dot_product( R(VarI0D,VarI0D+1:i), VarR1D(VarI0D+1:i) ) ) / R(VarI0D,VarI0D)
          end do
        end if

        ! (s'* inv(A'A) *s)^(-1/2)
        c = One / dsqrt( dot_product( s(1:i), VarR1D(1:i) ) )

        ! descent direction inv(A'A) * s * c
        DescentDir(1:i) = VarR1D(1:i)*c

        ! getting descent step size
        ! u 
        ii = 1
        do ii = 1, M
          !reusing VarR1D for computation of u
          VarR1D(1:i) = System(ii,ActiveSet(1:i))
          u(ii) = dot_product( VarR1D(1:i), DescentDir(1:i) )
        end do

        if ( i == P ) then
          DescentStep = CorrMax / c
        else
          NewActive = 0
          DescentStep = huge(DescentStep)
          ii = 1
          do ii = 1, N
            if ( Active(ii) ) cycle
            ci = Correlations(ii)
            di = dot_product( System(:,ii), u )
            VarR0D = (CorrMax-ci) / (c-di)
            if ( VarR0D > Zero .and. VarR0D < DescentStep ) then
                DescentStep = VarR0D
                NewActive = ii
                CorrMaxTemp = ci - DescentStep * di
            end if
            VarR0D = (CorrMax+ci) / (c+di)
            if ( VarR0D > Zero .and. VarR0D < DescentStep ) then
                DescentStep = VarR0D
                NewActive = ii
                CorrMaxTemp = ci - DescentStep * di
            end if
          end do
          
        end if

        VarR1D(1:i) = DescentStep * DescentDir(1:i)

        if ( This%LASSO ) then
          VarR0D = huge(One)

          do ii = 1, i
            LASSOWORK = - CoefficientsTemp(ii) / DescentDir(ii)
            if ( LASSOWORK > Zero .and. VarR0D > LASSOWORK ) then
              Drop = ii
              VarR0D = LASSOWORK
            end if
          end do
          if ( VarR0D < DescentStep ) then
            DropIndex = ActiveSet(Drop)
            DropSign = s(Drop)
            DescentStep = VarR0D
            Active(ActiveSet(Drop)) = .false.
            if ( Drop == i ) then
              continue
            else
              ActiveSet(Drop:i-1) = ActiveSet(Drop+1:i)
              s(Drop:i-1) = s(Drop+1:i)
              CoefficientsTemp(Drop:i-1) = CoefficientsTemp(Drop+1:i)
            end if
            ActiveSet(i) = 0
            NewActive = 0
            s(i) = 0
            CoefficientsTemp(i) = Zero

            ! updating QR decomposition after column deletion
            call dqrdec( M, i, i, Q1(:,1:i), M, R(1:i,1:i), i, Drop, VarR1D(1:max(1,i-Drop)) )

            ! Updating inv(R')*s
            iii = Drop
            if ( Drop == 1 ) then
              invRpS(1) = s(1) / R(1,1)
              iii = Drop+1
            end if
            ii = 1
            do ii = iii, i-1
              iim1 = ii - 1
              invRpS(ii) =  ( s(ii) - dot_product( R(1:iim1,ii), invRpS(1:iim1) ) ) / R(ii,ii)
            end do

            VarR1D(1:i) = DescentStep*DescentDir(1:i)
            if ( Drop == i ) then
              continue
            else
              VarR1D(Drop:i-1) = VarR1D(Drop+1:i)
            end if
            DescentDir(i) = Zero
            VarR1D(i) = Zero

            NbActive = NbActive - 1
            i = i - 1
            NoDrop = .false.
          else
            NoDrop = .true.
          end if
        end if

        CoefficientsTemp(1:i) = CoefficientsTemp(1:i) + VarR1D(1:i)

        if ( i == P ) exit

        Residual = Residual - u*DescentStep

        if ( NoDrop ) then
          if ( NewActive == 0 ) exit
          Correlations(NewActive) = CorrMaxTemp
        else
          CorrMaxTemp = dot_product( System(:,DropIndex), Residual )
          Correlations(DropIndex) = CorrMaxTemp
        end if

        CorrMax = abs(CorrMaxTemp)
        Correlations(ActiveSet(1:i)) = CorrMax*s(1:i)

      end do

      ! destandardizing predictors and decentering goal
      if ( present(CoefficientsSet) ) then
        CoefficientsTemp(1:NbActive) = CoefficientsTemp(1:NbActive) / Norm(ActiveSet(1:NbActive))
        if ( ConstantIndex /= 0 ) then
          allocate(CoefficientsSet(NbActive+1), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='CoefficientsSet', ProcName=ProcName, stat=StatLoc )
          CoefficientsSet(2:) = CoefficientsTemp(1:NbActive)
          CoefficientsSet(1) = (GoalMean - dot_product(CoefficientsSet(2:),Mean(ActiveSet(1:NbActive)))) / Mean(ConstantIndex)
        else
          allocate(CoefficientsSet(NbActive), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='CoefficientsSet', ProcName=ProcName, stat=StatLoc )
          CoefficientsSet = CoefficientsTemp(1:NbActive)
        end if    
      end if

      if ( present(ModelSet) ) then
        if ( ConstantIndex /= 0 ) then
          allocate(ModelSet(NbActive+1), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='ModelSet', ProcName=ProcName, stat=StatLoc )
          ModelSet(1) = 1
          ModelSet(2:) = ActiveSet(1:NbActive)
        else
          allocate(ModelSet(NbActive), stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='ModelSet', ProcName=ProcName, stat=StatLoc )
          ModelSet = ActiveSet(1:NbActive)
        end if
      end if

    end if

    i = 1
    do i = 1, N
      System(:,i) = System(:,i)*Norm(i)+Mean(i)
    end do
    Goal = Goal + GoalMean

    if ( present(AddDrop) ) then
      call AddDropLoc%Get( Values=AddDrop )
      call AddDropLoc%Purge()
    end if

    deallocate(VarR1D, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

    deallocate(Mean, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Mean', ProcName=ProcName, stat=StatLoc )

    deallocate(Norm, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Norm', ProcName=ProcName, stat=StatLoc )

    deallocate(Q1, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Q1', ProcName=ProcName, stat=StatLoc )

    deallocate(R, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='R', ProcName=ProcName, stat=StatLoc )

    deallocate(Residual, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Residual', ProcName=ProcName, stat=StatLoc )

    deallocate(u, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='u', ProcName=ProcName, stat=StatLoc )

    deallocate(invRpS, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='InfoInvxS', ProcName=ProcName, stat=StatLoc )

    deallocate(DescentDir, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='DescentDir', ProcName=ProcName, stat=StatLoc )

    deallocate(s, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='s', ProcName=ProcName, stat=StatLoc )

    deallocate(CoefficientsTemp, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='CoefficientsTemp', ProcName=ProcName, stat=StatLoc )

    deallocate(ActiveSet, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='ActiveSet', ProcName=ProcName, stat=StatLoc )

    deallocate(Active, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Active', ProcName=ProcName, stat=StatLoc )

    deallocate(Correlations, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Correlations', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
