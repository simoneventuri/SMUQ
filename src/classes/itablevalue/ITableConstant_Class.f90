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

module ITableConstant_Class

use Input_Library
use Parameters_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use ITableValue_Class                                             ,only:    ITableValue_Type
use Input_Class                                                   ,only:    Input_Type
use IScalarValue_Class                                            ,only:    IScalarValue_Type
use IScalarValueContainer_Class                                   ,only:    IScalarValueContainer_Type
use IScalarValue_Factory_Class                                    ,only:    IScalarValue_Factory

implicit none

private

public                                                                ::    ITableConstant_Type

type, extends(ITableValue_Type)                                       ::    ITableConstant_Type
  class(IScalarValue_Type), allocatable                               ::    Constant
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

  class(ITableConstant_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='Initialize'
  if (.not. This%Initialized) then
    This%Name = 'ITableConstant'
    This%Initialized = .true.
    call This%SetDefaults()
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(ITableConstant_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Constant)) deallocate(This%Constant, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Constant', ProcName=ProcName, stat=StatLoc)

  This%Initialized = .false.
  This%Constructed = .false.

  call This%Initialize()

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(ITableConstant_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(ITableConstant_Type), intent(inout)                           ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  logical                                                             ::    Found
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  SectionName = 'constant'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%Constant, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(ITableConstant_Type), intent(in)                              ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  integer                                                             ::    i
  integer                                                             ::    NbCoeffs

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%Constant, Name='constant',                   &
                                                                        Prefix=PrefixLoc, Directory=DirectoryLoc))

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetValue(This, Input, Abscissa)

  real(rkp), allocatable, dimension(:)                                ::    GetValue

  class(ITableConstant_Type), intent(in)                              ::    This
  type(Input_Type), intent(in)                                        ::    Input
  real(rkp), dimension(:), intent(in)                                 ::    Abscissa

  character(*), parameter                                             ::    ProcName='GetValue'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  allocate(GetValue(size(Abscissa,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='GetValue', ProcName=ProcName, stat=StatLoc)
  GetValue = Zero

  GetValue = This%Constant%GetValue(Input=Input)

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetCharValue(This, Input, Abscissa, Format)

  type(String_Type), allocatable, dimension(:)                        ::    GetCharValue
  
  class(ITableConstant_Type), intent(in)                              ::    This
  type(Input_Type), intent(in)                                        ::    Input
  real(rkp), dimension(:), intent(in)                                 ::    Abscissa
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='GetCharValue'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  character(:), allocatable                                           ::    FormatLoc
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  allocate(GetCharValue(size(Abscissa,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='GetValue', ProcName=ProcName, stat=StatLoc)

  VarR1D = This%GetValue(Input=Input, Abscissa=Abscissa)

  i = 1
  do i = 1, size(VarR1D,1)
    GetCharValue(i) = ConvertToString(Value=VarR1D(i), Format=FormatLoc)
  end do

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(ITableConstant_Type), intent(out)                             ::    LHS
  class(ITableValue_Type), intent(in)                                 ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (ITableConstant_Type)
      call LHS%Reset()
      LHS%Initialized = RHS%Initialized
      LHS%Constructed = RHS%Constructed
      if (RHS%Constructed) then
        allocate(LHS%Constant, source=RHS%Constant, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Constant', ProcName=ProcName, stat=StatLoc)
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(ITableConstant_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Constant)) deallocate(This%Constant, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Constant', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------
  
end module
  