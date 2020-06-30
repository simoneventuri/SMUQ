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

module DistProb_Class

use Input_Library
use Parameters_Library
use StringRoutines_Module
use QuadPack_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    DistProb_Type
public                                                                ::    Fun

type, abstract                                                        ::    DistProb_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  real(rkp)                                                           ::    A=One
  real(rkp)                                                           ::    B=One
  logical                                                             ::    TruncatedLeft=.false.
  logical                                                             ::    TruncatedRight=.false.
contains
  private
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    GetA
  procedure, public                                                   ::    GetB
  procedure, public                                                   ::    IsTruncatedLeft
  procedure, public                                                   ::    IsTruncatedRight
  procedure, public                                                   ::    GetMoment
  procedure, public                                                   ::    GetMean
  procedure, public                                                   ::    GetVariance
  procedure, public                                                   ::    ComputeMomentNumerical
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(Initialize_DistProb), deferred, public                    ::    Initialize
  procedure(Reset_DistProb), deferred, public                         ::    Reset
  procedure(ConstructInput_DistProb), deferred, private               ::    ConstructInput
  procedure(GetInput_DistProb), deferred, public                      ::    GetInput
  procedure(CDF_DistProb), deferred, public                           ::    CDF
  procedure(InvCDF_DistProb), deferred, public                        ::    InvCDF
  procedure(PDF_DistProb), deferred, public                           ::    PDF
  procedure(SetDefaults_DistProb), deferred, public                   ::    SetDefaults
  procedure(WriteInfo_DistProb), deferred, public                     ::    WriteInfo
  procedure(Copy_DistProb), deferred, public                          ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_DistProb(This)
    import                                                            ::    DistProb_Type
    class(DistProb_Type), intent(inout)                               ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_DistProb(This)
    import                                                            ::    DistProb_Type
    class(DistProb_Type), intent(inout)                               ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_DistProb(This)
    import                                                            ::    DistProb_Type
    class(DistProb_Type), intent(inout)                               ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_DistProb(This, Input, Prefix)
    import                                                            ::    DistProb_Type
    import                                                            ::    InputSection_Type
    class(DistProb_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_DistProb(This, Name, Prefix, Directory)
    import                                                            ::    DistProb_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_DistProb
    class(DistProb_Type), intent(in)                                  ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Directory
    character(*), optional, intent(in)                                ::    Prefix
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF_DistProb(This, X)
    use                                                               ::    Parameters_Library
    import                                                            ::    DistProb_Type
    real(rkp)                                                         ::    CDF_DistProb
    class(DistProb_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    X
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF_DistProb(This, P)
    use                                                               ::    Parameters_Library
    import                                                            ::    DistProb_Type
    real(rkp)                                                         ::    InvCDF_DistProb
    class(DistProb_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    P
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_DistProb(This, X)
    use                                                               ::    Parameters_Library
    import                                                            ::    DistProb_Type
    real(rkp)                                                         ::    PDF_DistProb
    class(DistProb_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    X
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInfo_DistProb(This, File)
    import                                                            ::    DistProb_Type
    import                                                            ::    SMUQFile_Type
    class(DistProb_Type), intent(in)                                  ::    This
    type(SMUQFile_Type), intent(inout)                                ::    File
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_DistProb(LHS, RHS)
    import                                                            ::    DistProb_Type
    class(DistProb_Type), intent(out)                                 ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Fun(X)  
    real(8)                                                           ::    Fun
    real(8), intent(in)                                               ::    X
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  ! !!------------------------------------------------------------------------------------------------------------------------------
  ! function PDF_R1D(This, X)

  !   real(rkp), allocatable, dimension(:)                              ::    PDF_R1D

  !   class(DistProb_Type), intent(in)                                  ::    This
  !   real(rkp), dimension(:), intent(in)                               ::    X

  !   character(*), parameter                                           ::    ProcName='PDF_R1D'
  !   integer                                                           ::    StatLoc=0
  !   integer                                                           ::    i
  !   integer                                                           ::    NbX

  !   NbX = size(X,1)
    
  !   allocate(PDF_R1D(NbX), stat=StatLoc)
  !   if (StatLoc /= 0) call Error%Allocate(Name='PDF_R1D', ProcName=ProcName, stat=StatLoc)
    
  !   i = 1
  !   do i = 1, NbX
  !     PDF_R1D(i) = This%PDF(X=X(i))
  !   end do

  ! end function
  ! !!------------------------------------------------------------------------------------------------------------------------------

  ! !!------------------------------------------------------------------------------------------------------------------------------
  ! function CDF_R1D(This, X)

  !   real(rkp), allocatable, dimension(:)                              ::    CDF_R1D

  !   class(DistProb_Type), intent(in)                                  ::    This
  !   real(rkp), dimension(:), intent(in)                               ::    X

  !   character(*), parameter                                           ::    ProcName='CDF_R1D'
  !   integer                                                           ::    StatLoc=0
  !   integer                                                           ::    i
  !   integer                                                           ::    NbX

  !   NbX = size(X,1)
    
  !   allocate(CDF_R1D(NbX), stat=StatLoc)
  !   if (StatLoc /= 0) call Error%Allocate(Name='CDF_R1D', ProcName=ProcName, stat=StatLoc)
    
  !   i = 1
  !   do i = 1, NbX
  !     CDF_R1D(i) = This%CDF(X=X(i))
  !   end do

  ! end function
  ! !!------------------------------------------------------------------------------------------------------------------------------

  ! !!------------------------------------------------------------------------------------------------------------------------------
  ! function InvCDF_R1D(This, P)

  !   real(rkp), allocatable, dimension(:)                              ::    InvCDF_R1D

  !   class(DistProb_Type), intent(in)                                  ::    This
  !   real(rkp), dimension(:), intent(in)                               ::    P

  !   character(*), parameter                                           ::    ProcName='InvCDF_R1D'
  !   integer                                                           ::    StatLoc=0
  !   integer                                                           ::    i
  !   integer                                                           ::    NbP

  !   NbP = size(P,1)
    
  !   allocate(InvCDF_R1D(NbP), stat=StatLoc)
  !   if (StatLoc /= 0) call Error%Allocate(Name='InvCDF_R1D', ProcName=ProcName, stat=StatLoc)
    
  !   i = 1
  !   do i = 1, NbP
  !     InvCDF_R1D(i) = This%InvCDF(P=P(i))
  !   end do

  ! end function
  ! !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName(This)

    character(:), allocatable                                         ::    GetName
    class(DistProb_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetA(This)

    real(rkp)                                                         ::    GetA

    class(DistProb_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetA'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (.not. This%TruncatedLeft) call Error%Raise(Line='Distribution was never left truncated', ProcName=ProcName)

    GetA = This%A

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetB(This)

    real(rkp)                                                         ::    GetB

    class(DistProb_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetB'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (.not. This%TruncatedRight) call Error%Raise(Line='Distribution was never right truncated', ProcName=ProcName)

    GetB = This%B

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsTruncatedLeft(This)

    logical                                                           ::    IsTruncatedLeft

    class(DistProb_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='IsTruncatedLeft'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    IsTruncatedLeft = This%TruncatedLeft

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsTruncatedRight(This)

    logical                                                           ::    IsTruncatedRight

    class(DistProb_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='IsTruncatedRight'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    IsTruncatedRight = This%TruncatedRight

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMean(This)

    real(rkp)                                                         ::    GetMean

    class(DistProb_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetMean'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetMean = This%GetMoment(Moment=1)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetVariance(This)

    real(rkp)                                                         ::    GetVariance

    class(DistProb_Type), intent(in)                                  ::    This

    character(*), parameter                                           ::    ProcName='GetVariance'
    real(rkp)                                                         ::    Ex
    real(rkp)                                                         ::    Ex2

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetVariance = This%GetMoment(Moment=2) - (This%GetMoment(Moment=1))**2

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMoment(This, Moment)

    real(rkp)                                                         ::    GetMoment

    class(DistProb_Type), intent(in)                                  ::    This
    integer, intent(in)                                               ::    Moment

    character(*), parameter                                           ::    ProcName='GetMoment'

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (Moment < 0) call Error%Raise("Requested a distribution moment below 0", ProcName=ProcName)

    if (Moment > 0) then
      GetMoment = This%ComputeMomentNumerical(Moment=Moment)
    else
      GetMoment = One
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeMomentNumerical(This, Moment)

    real(rkp)                                                         ::    ComputeMomentNumerical

    class(DistProb_Type), intent(in)                                  ::    This
    integer, intent(in)                                               ::    Moment

    character(*), parameter                                           ::    ProcName='ComputeMomentNumerical'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    NumericalMoment
    procedure(Fun), pointer                                           ::    PIntegrand=>null()
    real(8)                                                           ::    Num
    real(8)                                                           ::    EpsAbs
    real(8)                                                           ::    EpsRel
    integer                                                           ::    Key
    real(8)                                                           ::    AbsErr
    integer                                                           ::    NEval
    integer                                                           ::    Limit
    integer                                                           ::    LenW
    integer, allocatable, dimension(:)                                ::    iWork
    real(8), allocatable, dimension(:)                                ::    Work
    integer                                                           ::    Last
    real(8)                                                           ::    A
    real(8)                                                           ::    B

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    AbsErr = Zero
    NEval=0
    EpsAbs = 0.0
    EpsRel = 1.d-3
    Key = 6
    Limit = 500
    LenW = 4*Limit

    if (This%TruncatedLeft) A = This%GetA()
    if (This%TruncatedRight) B = This%GetB()

    allocate(iWork(Limit), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='iWork', ProcName=ProcName, stat=StatLoc)
    allocate(Work(LenW), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Work', ProcName=ProcName, stat=StatLoc)

    PIntegrand => Integrand

    NumericalMoment = Zero

    if (This%TruncatedLeft .and. This%TruncatedRight) then
      call dqag(PIntegrand, A, B, EpsAbs, EpsRel, Key, NumericalMoment, AbsErr, NEval, StatLoc, Limit, LenW, Last, iWork, Work)
      if (StatLoc /= 0 .and. StatLoc /= 2) call Error%Raise("Dqag exited with non zero exit code : " //                        &
                                                                                                  ConvertToString(Value=StatLoc))
    elseif (This%TruncatedLeft) then
      call dqagi(PIntegrand, A, 1, EpsAbs, EpsRel, NumericalMoment, AbsErr, NEval, StatLoc, Limit, LenW, Last, iWork, Work)
      if (StatLoc /= 0 .and. StatLoc /= 2) call Error%Raise("Dqagi exited with non zero exit code : " //                       &
                                                                                                  ConvertToString(Value=StatLoc))
    elseif (This%TruncatedRight) then
      call dqagi(PIntegrand, B, -1, EpsAbs, EpsRel, NumericalMoment, AbsErr, NEval, StatLoc, Limit, LenW, Last, iWork, Work)
      if (StatLoc /= 0 .and. StatLoc /= 2) call Error%Raise("Dqagi exited with non zero exit code : " //                       &
                                                                                                  ConvertToString(Value=StatLoc))
    else
      call dqagi(PIntegrand, Zero, 2, EpsAbs, EpsRel, NumericalMoment, AbsErr, NEval, StatLoc, Limit, LenW, Last, iWork, Work)
      if (StatLoc /= 0 .and. StatLoc /= 2) call Error%Raise("Dqagi exited with non zero exit code : " //                       &
                                                                                                  ConvertToString(Value=StatLoc))
    end if

    if (NumericalMoment /= NumericalMoment) call Error%Raise("Detected a NaN result from integrator", ProcName=ProcName)

    nullify(PIntegrand)
    deallocate(iWork, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='iWork', ProcName=ProcName, stat=StatLoc)
    deallocate(Work, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='Work', ProcName=ProcName, stat=StatLoc)

    ComputeMomentNumerical = NumericalMoment

    contains

      function Integrand(X)

        real(8)                                                           ::    Integrand

        real(8), intent(in)                                               ::    X

        Integrand = X**Moment * This%PDF(X=X)

      end function

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
