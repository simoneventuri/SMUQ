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

module IScalarNormalized_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use IScalarValue_Class                                            ,only:    IScalarValue_Type
use Input_Class                                                   ,only:    Input_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    IScalarNormalized_Type

type, extends(IScalarValue_Type)                                      ::    IScalarNormalized_Type
  character(:), allocatable                                           ::    Dependency
  type(SMUQString_Type), allocatable, dimension(:)                    ::    NormDependency
  integer                                                             ::    NbDependencies
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    GetValue
  procedure, public                                                   ::    GetCharValue
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type
  
logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize(This)

  class(IScalarNormalized_Type), intent(inout)                        ::    This

  character(*), parameter                                             ::    ProcName='Initialize'
  if (.not. This%Initialized) then
    This%Name = 'IScalarNormalized'
    This%Initialized = .true.
    call This%SetDefaults()
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(IScalarNormalized_Type), intent(inout)                        ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  if (allocated(This%NormDependency)) deallocate(This%NormDependency, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%NormDependency', ProcName=ProcName, stat=StatLoc)
  This%NbDependencies = 0
  
  call This%SetDefaults()

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(IScalarNormalized_Type), intent(inout)                        ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

  This%Dependency=''

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(IScalarNormalized_Type), intent(inout)                        ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    VarI0D
  logical                                                             ::    Found
  integer                                                             ::    i 
  integer                                                             ::    ii

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  ParameterName = 'dependency'
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
  This%Dependency = VarC0D

  ParameterName = 'normalization_dependencies'
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
  call ConvertToStrings(Value=VarC0D, Strings=This%NormDependency, Separator=' ')

  This%NbDependencies = size(This%NormDependency,1)

  i = 1
  ii = 0
  do i = 1, This%NbDependencies
    if (This%NormDependency(i) /= This%Dependency) cycle
    ii = i 
    exit 
  end do

  if ( ii == 0 ) call Error%Raise('Main dependency must be one of the ones in the normalization factor list', ProcName=ProcName)

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(IScalarNormalized_Type), intent(in)                           ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  call GetInput%AddParameter(Name='dependency', Value=This%Dependency)
  call GetInput%AddParameter(Name='normalization_dependencies', Value=ConvertToString(Values=This%NormDependency, Separator=' '))

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetValue(This, Input)

  real(rkp)                                                           ::    GetValue

  class(IScalarNormalized_Type), intent(in)                           ::    This
  type(Input_Type), intent(in)                                        ::    Input

  character(*), parameter                                             ::    ProcName='GetValue'
  integer                                                             ::    StatLoc=0
  real(rkp)                                                           ::    DenSum
  real(rkp)                                                           ::    VarR0D
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DenSum = Zero

  i = 1
  do i = 1, This%NbDependencies
    call Input%GetValue(Value=VarR0D, Label=This%NormDependency(i))
    DenSum = DenSum + VarR0D
  end do

  call Input%GetValue(Value=GetValue, Label=This%Dependency)
  GetValue = GetValue / DenSum

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetCharValue(This, Input, Format)

  character(:), allocatable                                           ::    GetCharValue

  class(IScalarNormalized_Type), intent(in)                           ::    This
  type(Input_Type), intent(in)                                        ::    Input
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='GetCharValue'
  character(:), allocatable                                           ::    FormatLoc
  integer                                                             ::    StatLoc=0
  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  GetCharValue =  ConvertToString(Value=This%GetValue(Input=Input), Format=FormatLoc)

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(IScalarNormalized_Type), intent(out)                          ::    LHS
  class(IScalarValue_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (IScalarNormalized_Type)
      call LHS%Reset()
      LHS%Initialized = RHS%Initialized
      LHS%Constructed = RHS%Constructed
      if (RHS%Constructed) then
        LHS%Dependency = RHS%Dependency
        allocate(LHS%NormDependency, source=RHS%NormDependency, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%NormDependency', ProcName=ProcName, stat=StatLoc)
        LHS%NbDependencies = RHS%NbDependencies
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(IScalarNormalized_Type), intent(inout)                         ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%NormDependency)) deallocate(This%NormDependency, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%NormDependency', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
  
