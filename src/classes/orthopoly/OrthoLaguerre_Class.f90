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
! X~gamma(alpha,1), A = alpha-1
module OrthoLaguerre_Class

use Input_Library
use Logger_Class                                                  ,only:  Logger
use Error_Class                                                   ,only:  Error
use OrthoPoly_class
use ComputingRoutines_Module
use Parameters_Library
use StringConversion_Module
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    OrthoLaguerre_Type

type, extends(OrthoPoly_Type)                                         ::    OrthoLaguerre_Type
  real(rkp)                                                           ::    A=Zero
contains
  procedure, public                                                   ::    Reset
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Eval_N
  procedure, public                                                   ::    Eval_MN
  procedure, nopass, public                                           ::    NFactor
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(OrthoLaguerre_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed = .false.

  This%A = 0
  This%Normalized = .false.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(OrthoLaguerre_Type), intent(inout)                            ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  real(rkp)                                                           ::    VarR0D
  logical                                                             ::    Found
  character(:), allocatable                                           ::    ParameterName
  logical                                                             ::    VarL0D
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  ParameterName = 'alpha'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) then
    if (VarR0D < Zero) call Error%Raise(Line='Alpha setting below minimum of 1', ProcName=ProcName)
    This%A = VarR0D - One
  end if

  ParameterName = 'normalized'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
  if (Found) This%Normalized = VarL0D

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This, Alpha, Normalized)
  
  class(OrthoLaguerre_Type), intent(inout)                            ::    This
  real(rkp), optional, intent(in)                                     ::    Alpha
  logical, optional, intent(in)                                       ::    Normalized 

  character(*), parameter                                             ::    ProcName='ConstructCase1'

  call This%Reset()

  if (present(Alpha)) then
    if (Alpha < Zero) call Error%Raise(Line='Alpha setting below minimum of 1', ProcName=ProcName)
    This%A = Alpha - One
  end if

  if (present(Normalized)) This%Normalized = Normalized

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput
  class(OrthoLaguerre_Type), intent(in)                               ::    This
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

  call GetInput%AddParameter(Name='alpha', Value=ConvertToString(Value=This%A + One))
  call GetInput%AddParameter(Name='normalized', Value=ConvertToString(Value=This%Normalized))

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Eval_N(This, Order, X, Value, Normalized)

  class(OrthoLaguerre_Type), intent(inout)                            ::    This
  real(rkp), intent(in)                                               ::    X
  integer, intent(in)                                                 ::    Order
  real(rkp), intent(out)                                              ::    Value
  logical, optional, intent(in)                                       ::    Normalized

  character(*), parameter                                             ::    ProcName='Eval_N'
  real(rkp)                                                           ::    valnm1
  real(rkp)                                                           ::    valnp0
  real(rkp)                                                           ::    valnp1
  real(rkp)                                                           ::    nt
  integer                                                             ::    i
  logical                                                             ::    NormalizedLoc

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  NormalizedLoc = This%Normalized
  if (present(Normalized)) NormalizedLoc = Normalized

  if (X < Zero) call Error%Raise(Line='X argument below allowable minimum of 0', ProcName=ProcName)

  if (Order < -1) call Error%Raise("An order of below -1 was requested but is not supported")

  Value = Zero

  if (Order == -1) then
    Value = This%polyorderm1
  elseif (Order == 0) then
    Value = This%polyorder0
  else
    i = 1
    valnm1 = This%polyorderm1
    valnp0 = This%polyorder0
    do i = 1, Order
      nt = real(i-1,rkp)
      valnp1 = ((Two*nt+One+This%A-X)*valnp0-(nt+This%A)*valnm1)/(nt+One)
      valnm1 = valnp0
      valnp0 = valnp1
    end do
    Value = valnp1
  end if

  if (NormalizedLoc) Value = Value / This%NFactor(Order=Order, A=This%A)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Eval_MN(This, MinOrder, MaxOrder, X, Values, Normalized)

  class(OrthoLaguerre_Type), intent(inout)                            ::    This
  real(rkp), intent(in)                                               ::    X
  integer, intent(in)                                                 ::    MinOrder
  integer, intent(in)                                                 ::    MaxOrder
  real(rkp), dimension(:), intent(inout)                              ::    Values
  logical, optional, intent(in)                                       ::    Normalized

  character(*), parameter                                             ::    ProcName='Eval_MN'
  real(rkp)                                                           ::    valnm1
  real(rkp)                                                           ::    valnp0
  real(rkp)                                                           ::    valnp1
  real(rkp)                                                           ::    nt
  integer                                                             ::    i, i_offset, ii
  integer                                                             ::    StatLoc=0
  logical                                                             ::    NormalizedLoc

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  NormalizedLoc = This%Normalized
  if (present(Normalized)) NormalizedLoc = Normalized

  if (X < Zero) call Error%Raise(Line='X argument below allowable minimum of 0', ProcName=ProcName)

  if (MinOrder < -1) call Error%Raise("A starting order of below -1 was requested but is not supported")
  if (MinOrder > MaxOrder) call Error%Raise("Starting order was specified to be larger than the final order")

  if (size(Values,1) /= MaxOrder-MinOrder + 1) call Error%Raise('Incompatible values array', ProcName=ProcName)
  Values = Zero

  if (MinOrder == MaxOrder) then
    call This%Eval(Order=MinOrder, X=X, Value=Values(1), Normalized=NormalizedLoc) 
  else
    i_offset = 0
    if (MinOrder == -1)  then
      call This%Eval(Order=-1, X=X, Value=Values(1), Normalized=NormalizedLoc)
      call This%Eval(Order=0, X=X, Value=Values(2), Normalized=NormalizedLoc)
      i_offset = 2
    elseif (MinOrder == 0) then
      call This%Eval(Order=0, X=X, Value=Values(1), Normalized=NormalizedLoc)
      i_offset = 1
    end if
    i = 1
    valnm1 = This%polyorderm1
    valnp0 = This%polyorder0
    ii = 0
    do i = 1, MaxOrder
      nt = real(i-1,rkp)
      valnp1 = ((Two*nt+One+This%A-X)*valnp0-(nt+This%A)*valnm1)/(nt+One)
      valnm1 = valnp0
      valnp0 = valnp1
      if (i >= MinOrder) then
        Values(i+i_offset-ii) = valnp1
        if (NormalizedLoc) Values(i+i_offset-ii) = Values(i+i_offset-ii) / This%NFactor(Order=i, A=This%A)
      else
        ii = ii + 1
      end if
    end do

  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function NFactor(Order, A)

  real(rkp)                                                           ::    NFactor

  integer, intent(in)                                                 ::    Order
  real(rkp), intent(in)                                               ::    A

  character(*), parameter                                             ::    ProcName='NFactor'

  if (Order > 0) then
    NFactor = dsqrt(BinomialCoeff(real(Order,rkp) + A, Order))
  else
    NFactor = One
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(OrthoLaguerre_Type), intent(out)                              ::    LHS
  class(OrthoPoly_Type), intent(in)                                   ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (OrthoLaguerre_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed
      if (RHS%Constructed) then
        LHS%A = RHS%A
        LHS%Normalized = RHS%Normalized
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(OrthoLaguerre_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
