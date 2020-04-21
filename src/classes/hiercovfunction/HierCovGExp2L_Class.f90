! -*-f90-*-
!!----------------------------------------------------------------------------------------------------------------------------------
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
!!----------------------------------------------------------------------------------------------------------------------------------

module HierCovGExp2L_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use CovFunction_Class                                             ,only:    CovFunction_Type
use HierCovFunction_Class                                         ,only:    HierCovFunction_Type
use CovGExp2L_Class                                               ,only:    CovGExp2L_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use IScalarValueClass                                             ,only:    IScalarValue_Type
use IScalarFixedClass                                             ,only:    IScalarFixed_Type
use IScalarValue_Factory_Class                                    ,only:    IScalarValue_Factory

implicit none

private

public                                                                ::    HierCovGExp2L_Type

type, extends(HierCovFunction_Type)                                   ::    HierCovGExp2L_Type
  class(IScalarValue_Type), allocatable                               ::    L1
  class(IScalarValue_Type), allocatable                               ::    L2
  class(IScalarValue_Type), allocatable                               ::    Lr
  class(IScalarValue_Type), allocatable                               ::    Zs
  class(IScalarValue_Type), allocatable                               ::    Sigma
  real(rkp)                                                           ::    Tolerance
  character(:), allocatable                                           ::    CoordinateLabel
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Generate
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize(This)

  class(HierCovGExp2L_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Initialize'

  if (.not. This%Initialized) then
    This%Name = 'HierCovGExp2L'
    This%Initialized = .true.
    call This%SetDefaults()
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(HierCovGExp2L_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc = 0

  This%Initialized=.false.
  This%Constructed=.false.

  if (allocated(This%L1)) deallocate(This%L1, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%L1', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%L2)) deallocate(This%L2, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%L2', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Lr)) deallocate(This%Lr, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Lr', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Zs)) deallocate(This%Zs, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Zs', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Sigma)) deallocate(This%Sigma, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Sigma', ProcName=ProcName, stat=StatLoc)

  call This%SetDefaults()

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(HierCovGExp2L_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

  This%Tolerance = 1e-10
  This%CoordinateLabel = ''

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(HierCovGExp2L_Type), intent(inout)                            ::    This
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
  logical                                                             ::    MandatoryLoc

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  MandatoryLoc = .false.

  SectionName = 'l1'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%L1, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SectionName = 'l2'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%L2, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SectionName = 'lr'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%Lr, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SectionName = 'zs'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%Zs, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SectionName = 'sigma'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%Sigma, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  ParameterName = 'tolerance'
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%Tolerance=VarR0D

  ParameterName = 'coordinate_label'
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
  if (Found) This%CoordinateLabel=VarC0D

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(HierCovGExp2L_Type), intent(in)                               ::    This
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

  SectionName = 'l1'
  if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
  call GetInput%AddSection(SectionName=IScalarValue_Factory%GetObjectInput(Object=This%L1, Name=SectionName,                      &
                                                                            Prefix=PrefixLoc, Directory=DirectorySub))

  SectionName = 'l2'
  if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
  call GetInput%AddSection(SectionName=IScalarValue_Factory%GetObjectInput(Object=This%L2, Name=SectionName,                      &
                                                                            Prefix=PrefixLoc, Directory=DirectorySub))

  SectionName = 'lr'
  if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
  call GetInput%AddSection(SectionName=IScalarValue_Factory%GetObjectInput(Object=This%Lr, Name=SectionName,                      &
                                                                            Prefix=PrefixLoc, Directory=DirectorySub))

  SectionName = 'zs'
  if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
  call GetInput%AddSection(SectionName=IScalarValue_Factory%GetObjectInput(Object=This%Zs, Name=SectionName,                      &
                                                                            Prefix=PrefixLoc, Directory=DirectorySub))

  SectionName = 'sigma'
  if (ExternalFlag) DirectorySub = DirectoryLoc // '/' // SectionName
  call GetInput%AddSection(SectionName=IScalarValue_Factory%GetObjectInput(Object=This%Sigma, Name=SectionName,                   &
                                                                            Prefix=PrefixLoc, Directory=DirectorySub))

  call GetInput%AddParameter(Name='tolerance', Value=ConvertToString(This%Tolerance))

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Generate(This, Input, CovFunction)

  class(HierCovGExp2L_Type), intent(in)                               ::    This
  type(Input_Type), intent(in)                                        ::    Input
  class(CovFunction_Type), allocatable, intent(out)                   ::    CovFunction

  character(*), parameter                                             ::    ProcName='ConstructInput'
  integer                                                             ::    StatLoc=0
  real(rkp)                                                           ::    L1
  real(rkp)                                                           ::    L2
  real(rkp)                                                           ::    Lr
  real(rkp)                                                           ::    Zs
  real(rkp)                                                           ::    Sigma

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  L1 = This%L1%GetValue(Input=Input)

  L2 = This%L2%GetValue(Input=Input)

  Lr = This%Lr%GetValue(Input=Input)

  Zs = This%Zs%GetValue(Input=Input)

  Sigma = This%Sigma%GetValue(Input=Input)

  allocate(CovGExp2L_Type :: CovFunction)

  select type (CovFunction)
    type is (CovGExp2L_Type)
      call CovFunction%Construct(Sigma=Sigma, L1=L1, L2=L2, Lr=Lr, Zs=Zs, Coordinate=This%CoordinateLabel,                     &
                                                                                                      Tolerance=This%Tolerance)
    class default
      call Error%Raise("Something went wrong", ProcName=ProcName)
  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(HierCovGExp2L_Type), intent(out)                              ::    LHS
  class(HierCovFunction_Type), intent(in)                             ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (HierCovGExp2L_Type)
      call LHS%Reset()
      LHS%Initialized = RHS%Initialized
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        allocate(LHS%L1, source=RHS%L1, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%L1', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%L2, source=RHS%L2, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%L2', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%Lr, source=RHS%Lr, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Lr', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%Zs, source=RHS%Zs, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Zs', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%Sigma, source=RHS%Sigma, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Sigma', ProcName=ProcName, stat=StatLoc)
        LHS%CoordinateLabel = RHS%CoordinateLabel
        LHS%Tolerance = RHS%Tolerance
      end if
    
    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(HierCovGExp2L_Type), intent(inout)                           ::    This

  character(*), parameter                                           ::    ProcName='Finalizer'
  integer                                                           ::    StatLoc=0

  if (allocated(This%L1)) deallocate(This%L1, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%L1', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%L2)) deallocate(This%L2, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%L2', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Lr)) deallocate(This%Lr, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Lr', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Zs)) deallocate(This%Zs, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Zs', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Sigma)) deallocate(This%Sigma, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Sigma', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
