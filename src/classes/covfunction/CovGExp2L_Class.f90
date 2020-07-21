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

module CovGExp2L_Class

use Input_Library
use Parameters_Library
use StringConversion_Module
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use CovFunction_Class                                             ,only:    CovFunction_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use SMUQString_Class                                              ,only:    SMUQString_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    CovGExp2L_Type

type, extends(CovFunction_Type)                                       ::    CovGExp2L_Type
  real(rkp)                                                           ::    L1
  real(rkp)                                                           ::    L2
  real(rkp)                                                           ::    Lr
  real(rkp)                                                           ::    Zs
  real(rkp)                                                           ::    Sigma
  real(rkp)                                                           ::    Tolerance
  character(:), allocatable                                           ::    CoordinateLabel
contains
  procedure, public                                                   ::    Reset
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Evaluate_1D
  procedure, nopass, private                                          ::    Lz
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(CovGExp2L_Type), intent(inout)                                ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc = 0

  This%Constructed=.false.

  This%L1 = One
  This%L2 = One
  This%Lr = One
  This%Sigma = One
  This%Zs = Zero
  This%Tolerance = 1e-10
  This%CoordinateLabel = ''

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(CovGExp2L_Type), intent(inout)                                ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  integer                                                             ::    StatLoc=0
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  real(rkp)                                                           ::    VarR0D
  character(:), allocatable                                           ::    VarC0D
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  integer                                                             ::    i
  logical                                                             ::    Found
  character(:), allocatable                                           ::    PrefixLoc
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  ParameterName = 'l1'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.true.)
  This%L1=VarR0D
  if (This%L1 < Zero) call Error%Raise(Line='Characteristic length 1 scale value below 0', ProcName=ProcName)

  ParameterName = 'l2'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.true.)
  This%L2=VarR0D
  if (This%L2 < Zero) call Error%Raise(Line='Characteristic length 2 scale value below 0', ProcName=ProcName)

  ParameterName = 'lr'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.true.)
  This%Lr=VarR0D
  if (This%Lr < Zero) call Error%Raise(Line='Characteristic length r scale value below 0', ProcName=ProcName)

  ParameterName = 'zs'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.true.)
  This%Zs=VarR0D

  ParameterName = 'sigma'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.true.)
  This%Sigma=VarR0D
  if (This%Sigma < Zero) call Error%Raise(Line='Sigma value below 0', ProcName=ProcName)

  ParameterName = 'tolerance'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%Tolerance=VarR0D

  ParameterName = 'coordinate_label'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
  This%CoordinateLabel=VarC0D

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This, Sigma, L1, L2, Lr, Zs, Coordinate, Tolerance)

  class(CovGExp2L_Type), intent(inout)                                ::    This
  real(rkp), intent(in)                                               ::    Sigma
  real(rkp), intent(in)                                               ::    L1
  real(rkp), intent(in)                                               ::    L2
  real(rkp), intent(in)                                               ::    Lr
  real(rkp), intent(in)                                               ::    Zs
  character(*), intent(in)                                            ::    Coordinate
  real(rkp), optional, intent(in)                                     ::    Tolerance

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  call This%Reset()

  This%Sigma = Sigma
  if (This%Sigma < Zero) call Error%Raise(Line='Sigma value below 0', ProcName=ProcName)

  This%L1 = L1
  if (This%L1 < Zero) call Error%Raise(Line='Characteristic length 1 scale value below 0', ProcName=ProcName)

  This%L2 = L2
  if (This%L2 < Zero) call Error%Raise(Line='Characteristic length 2 scale value below 0', ProcName=ProcName)

  This%Lr = Lr
  if (This%Lr < Zero) call Error%Raise(Line='Characteristic length r scale value below 0', ProcName=ProcName)

  This%Zs = Zs

  This%CoordinateLabel = trim(adjustl(Coordinate))

  if (present(Tolerance)) This%Tolerance = Tolerance

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(CovGExp2L_Type), intent(in)                                   ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  character(:), allocatable                                           ::    FileName
  type(SMUQFile_Type)                                                 ::    File
  type(InputSection_Type), pointer                                    ::    InputSection=>null()

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  call GetInput%AddParameter(Name='coordinate_label', Value=This%CoordinateLabel)
  call GetInput%AddParameter(Name='l1', Value=ConvertToString(This%L1))
  call GetInput%AddParameter(Name='l2', Value=ConvertToString(This%L2))
  call GetInput%AddParameter(Name='lr', Value=ConvertToString(This%Lr))
  call GetInput%AddParameter(Name='zs', Value=ConvertToString(This%Zs))
  call GetInput%AddParameter(Name='sigma', Value=ConvertToString(This%Sigma))

  call GetInput%AddParameter(Name='tolerance', Value=ConvertToString(This%Tolerance))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Evaluate_1D(This, Coordinates, CoordinateLabels, Covariance)

  class(CovGExp2L_Type), intent(in)                                   ::    This
  real(rkp), dimension(:,:), intent(in)                               ::    Coordinates
  type(SMUQString_Type), dimension(:), intent(in)                     ::    CoordinateLabels
  real(rkp), dimension(:,:), intent(inout)                            ::    Covariance

  character(*), parameter                                             ::    ProcName='Evaluate_1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  integer                                                             ::    ii
  real(rkp)                                                           ::    Zi
  real(rkp)                                                           ::    Zj
  real(rkp)                                                           ::    LZi
  real(rkp)                                                           ::    LZj
  integer                                                             ::    NbNodes
  integer                                                             ::    iCoordinate

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  NbNodes = size(Coordinates,1)

  i = 1
  iCoordinate = 0
  do i = 1, size(Coordinates,2)
    if (CoordinateLabels(i) == This%CoordinateLabel) then
      iCoordinate = i
      exit
    end if
  end do
  if (iCoordinate == 0) call Error%Raise('Did not find matching coordinate label: ' // This%CoordinateLabel,                 &
                                                                                                              ProcName=ProcName)

  if (size(Covariance,1) /= size(Covariance,2)) call Error%Raise('Passed non-square array', ProcName=ProcName)
  if (size(Covariance,1) /= NbNodes) call Error%Raise('Cov array dimensions and number of coordinates mismatch',      &
                                                                                                              ProcName=ProcName)

  Covariance = Zero

  i = 1
  do i = 1, NbNodes
    Zi = Coordinates(i,iCoordinate)
    LZi = This%Lz(Z=Zi, Zs=This%Zs, L1=This%L1, L2=This%L2, Lr=This%Lr)
    ii = 1
    do ii = i, NbNodes
      Zj = Coordinates(ii,iCoordinate)
      LZj = This%Lz(Z=Zj, Zs=This%Zs, L1=This%L1, L2=This%L2, Lr=This%Lr)
      Covariance(i,ii) = This%Sigma**2*dsqrt(Two*LZi*LZj/(LZi**2+LZj**2))*dexp(- (dabs(Zi-Zj)/dsqrt(LZi**2+LZj**2))**2)
      if (abs(Covariance(i,ii) / This%Sigma**2) < This%Tolerance) Covariance(i,ii) = Zero
    end do
    Covariance(i:NbNodes,i) = Covariance(i,NbNodes)
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Lz(Z, Zs, L1, L2, Lr)

  real(rkp)                                                           ::    Lz

  real(rkp), intent(in)                                               ::    Z
  real(rkp), intent(in)                                               ::    Zs
  real(rkp), intent(in)                                               ::    L1
  real(rkp), intent(in)                                               ::    L2
  real(rkp), intent(in)                                               ::    Lr

  if (Z < Zs) then
    Lz = L1
  else
    Lz = L1 + (L2-L1)*(One-dexp(-dabs(Z-Zs)/Lr))
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(CovGExp2L_Type), intent(out)                                  ::    LHS
  class(CovFunction_Type), intent(in)                                 ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (CovGExp2L_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%L1 = RHS%L1
        LHS%L2 = RHS%L2
        LHS%Lr = RHS%Lr
        LHS%Zs = RHS%Zs
        LHS%Sigma = RHS%Sigma
        LHS%CoordinateLabel = RHS%CoordinateLabel
        LHS%Tolerance = RHS%Tolerance
      end if
    
    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(CovGExp2L_Type), intent(inout)                                 ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
