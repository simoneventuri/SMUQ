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

module IScalarFixed_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use IScalarValue_Class                                            ,only:    IScalarValue_Type
use Input_Class                                                   ,only:    Input_Type
use StringConversion_Module

implicit none

private

public                                                                ::    IScalarFixed_Type

type, extends(IScalarValue_Type)                                      ::    IScalarFixed_Type
  real(rkp)                                                           ::    Value
  character(:), allocatable                                           ::    Label
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    GetValue
  procedure, public                                                   ::    GetCharValue
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize(This)

  class(IScalarFixed_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Initialize'
  if (.not. This%Initialized) then
    This%Name = 'IScalarFixed'
    This%Initialized = .true.
    call This%SetDefaults()
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(IScalarFixed_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  call This%SetDefaults()

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(IScalarFixed_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

  This%Value = Zero
  
end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(IScalarFixed_Type), intent(inout)                             ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    ParameterName
  real(rkp)                                                           ::    VarR0D
  logical                                                             ::    Found
  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  ParameterName = 'value'
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.true.)
  This%Value = VarR0D

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This, Value)

  class(IScalarFixed_Type), intent(inout)                             ::    This
  real(rkp), intent(in)                                               ::    Value

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  This%Value = Value

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(IScalarFixed_Type), intent(in)                                ::    This
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
  call GetInput%AddParameter(Name='value', Value=ConvertToString(Value=This%Value))

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetValue(This, Input)

  real(rkp)                                                           ::    GetValue

  class(IScalarFixed_Type), intent(in)                                ::    This
  type(Input_Type), intent(in)                                        ::    Input

  character(*), parameter                                             ::    ProcName='GetValue'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetValue = This%Value

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetCharValue(This, Input, Format)

  character(:), allocatable                                           ::    GetCharValue

  class(IScalarFixed_Type), intent(in)                                ::    This
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

  class(IScalarFixed_Type), intent(out)                               ::    LHS
  class(IScalarValue_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (IScalarFixed_Type)
      call LHS%Reset()
      LHS%Initialized = RHS%Initialized
      LHS%Constructed = RHS%Constructed
      if (RHS%Constructed) then
        LHS%Value = RHS%Value
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
