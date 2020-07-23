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

module OrthoMultiVar_Class

use Input_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Parameters_Library
use OrthoPoly_Class                                               ,only:    OrthoPoly_Type
use OrthoPolyContainer_Class                                      ,only:    OrthoPolyContainer_Type
use OrthoPoly_Factory_Class                                       ,only:    OrthoPoly_Factory    
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    OrthoMultiVar_Type

type                                                                  ::    OrthoMultiVar_Type
  logical                                                             ::    Constructed=.false.
  type(OrthoPolyContainer_Type), allocatable, dimension(:)            ::    OrthoPoly
  integer                                                             ::    NbDim=0
contains
  procedure, public                                                   ::    Reset
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Precompute
  generic, public                                                     ::    Eval                    =>    EvalObject0D,           &
                                                                                                          EvalObject1D
  procedure, private                                                  ::    EvalObject0D
  procedure, private                                                  ::    EvalObject1D
  generic, public                                                     ::    GetPolynomial           =>    GetOrthoPoly0D,         &
                                                                                                          GetOrthoPoly1D
  procedure, private                                                  ::    GetOrthoPoly0D
  procedure, private                                                  ::    GetOrthoPoly1D
  procedure, public                                                   ::    GetNbDim
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal=.false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(OrthoMultiVar_Type), intent(inout)                            ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed=.false.

  if (allocated(This%OrthoPoly)) deallocate(This%OrthoPoly, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%OrthoPoly', ProcName=ProcName, stat=StatLoc)
  This%NbDim = 0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  use StringConversion_Module

  class(OrthoMultiVar_Type), intent(inout)                            ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  type(InputSection_Type), pointer                                    ::    InputSection
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    VarI0D
  logical                                                             ::    VarL0D
  logical                                                             ::    Found
  integer                                                             ::    i
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  class(OrthoPoly_Type), allocatable                                  ::    OrthoPoly
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  This%NbDim = Input%GetNumberofSubSections()

  allocate(This%OrthoPoly(This%NbDim), stat=StatLoc)
  if (statLoc /= 0) call Error%Allocate(Name='This%OrthoPoly', ProcName=ProcName, stat=StatLoc)
  
  i = 1
  do i = 1, This%NbDim
    SectionName = 'orthopoly' // ConvertToString(Value=i)
    call InputVerifier%AddSection(Section=SectionName)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call OrthoPoly_Factory%Construct(Object=OrthoPoly, Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)

    call This%OrthoPoly(i)%Set(Object=OrthoPoly)
    deallocate(OrthoPoly, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='OrthoPoly', ProcName=ProcName, stat=StatLoc)
  end do

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This, Polynomials)
  
  class(OrthoMultiVar_Type), intent(inout)                            ::    This
  type(OrthoPolyContainer_Type), dimension(:), intent(in)             ::    Polynomials

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  call This%Reset()

  This%NbDim = size(Polynomials,1)

  allocate(This%OrthoPoly, source=Polynomials, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%OrthoPoly', ProcName=ProcName, stat=StatLoc)

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput

  class(OrthoMultiVar_Type), intent(in)                               ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SectionName
  integer                                                             ::    i
  class(OrthoPoly_Type), pointer                                      ::    OrthoPolyPointer=>null()


  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  i = 1
  do i = 1, This%NbDim
    OrthoPolyPointer => This%OrthoPoly(i)%GetPointer()
    SectionName = 'orthopoly' // ConvertToString(Value=i)
    if (ExternalFlag) DirectorySub = DirectoryLoc // 'orthopoly' // ConvertToString(Value=i) // '/'
    call GetInput%AddSection(Section=OrthoPoly_Factory%GetObjectInput(Object=OrthoPolyPointer, Name=SectionName,   &
                                                                                    Prefix=PrefixLoc, Directory=DirectorySub))
    nullify(OrthoPolyPointer)
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Precompute(This, X, MaxOrder, Values)

  class(OrthoMultiVar_Type), intent(inout)                            ::    This
  real(rkp), dimension(:), intent(in)                                 ::    X
  integer, dimension(:), intent(in)                                   ::    MaxOrder
  real(rkp), allocatable, dimension(:,:), intent(inout)               ::    Values

  character(*), parameter                                             ::    ProcName='Precompute'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0
  class(OrthoPoly_Type), pointer                                      ::    OrthoPolyPointer=>null()

  if (.not. This%Constructed) call Error%Raise(Line='The OrthoMultiVar object was never constructed', ProcName=ProcName)

  if (size(X,1) /= This%NbDim) call Error%Raise(Line='Size of the X vector does not match dimension of multivariate ' //     &
                                                                                    'orthogonal polynomial', ProcName=ProcName)

  if (size(X,1) /= This%NbDim) call Error%Raise(Line='Size of the MaxOrder vector does not match dimension of  ' //          &
                                                                        'multivariate orthogonal polynomial', ProcName=ProcName)

  if (allocated(Values)) deallocate(Values, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Values', ProcName=ProcName, stat=StatLoc)

  allocate(Values(maxval(MaxOrder)+1,This%NbDim), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Values', ProcName=ProcName, stat=StatLoc)
  Values = Zero

  i = 1
  do i = 1, This%NbDim
    OrthoPolyPointer => This%OrthoPoly(i)%GetPointer()
    call OrthoPolyPointer%Eval(MinOrder=0, MaxOrder=MaxOrder(i) , X=X(i), Values=Values(1:MaxOrder(i)+1,i))
    nullify(OrthoPolyPointer)
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine EvalObject0D(This, X, Indices, Value)

  class(OrthoMultiVar_Type), intent(inout)                            ::    This
  real(rkp), dimension(:), intent(in)                                 ::    X
  integer, dimension(:), intent(in)                                   ::    Indices
  real(rkp), intent(out)                                              ::    Value

  character(*), parameter                                             ::    ProcName='EvalObject0D'
  integer                                                             ::    i
  class(OrthoPoly_Type), pointer                                      ::    OrthoPolyPointer=>null()
  real(rkp)                                                           ::    VarR0D

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (size(Indices,1) /= This%NbDim) call Error%Raise(Line='Indices length does not match the dimension of the' //           & 
                                                                      ' multivariate orthogonal polynomial', ProcName=ProcName)

  Value = One
  i = 1
  do i = 1, This%NbDim
    OrthoPolyPointer => This%OrthoPoly(i)%GetPointer()
    call OrthoPolyPointer%Eval(Order=Indices(i), X=X(i), Value=VarR0D)
    Value = Value * VarR0D
    nullify(OrthoPolyPointer)
  end do

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine EvalObject1D(This, X, Indices, Values)

  class(OrthoMultiVar_Type), intent(inout)                            ::    This
  real(rkp), dimension(:), intent(in)                                 ::    X
  integer, dimension(:,:), intent(in)                                 ::    Indices
  real(rkp), dimension(:), intent(inout)                              ::    Values

  character(*), parameter                                             ::    ProcName='EvalObject1D'
  integer                                                             ::    NbTuples
  integer                                                             ::    i, ii
  integer, allocatable, dimension(:)                                  ::    MaxOrder
  real(rkp), allocatable, dimension(:,:)                              ::    Values0D
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (size(Indices,1) /= This%NbDim) call Error%Raise(Line='Indices length does not match the dimension of the' //           & 
                                                                      ' multivariate orthogonal polynomial', ProcName=ProcName)

  if (size(Values,1) /= size(Indices,2)) call Error%Raise('Incompatible values array', ProcName=ProcName)

  if (size(X,1) /= This%NbDim) call Error%Raise('Incompatible X array', ProcName=ProcName)

  NbTuples = size(Indices,2)

  allocate(MaxOrder(This%NbDim), stat=StatLoc)  
  if (StatLoc /= 0) call Error%Allocate(Name='MaxOrder', ProcName=ProcName, stat=StatLoc)
  MaxOrder = maxval(Indices, 2)

  call This%Precompute(X=X, MaxOrder=MaxOrder, Values=Values0D)

  Values = One

  i = 1
  do i = 1, NbTuples
    ii = 1
    do ii = 1, This%NbDim
      Values(i) = Values(i) * Values0D(Indices(ii,i)+1,ii)
    end do
  end do

  deallocate(MaxOrder, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='MaxOrder', ProcName=ProcName, stat=StatLoc)

  deallocate(Values0D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Values0D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetOrthoPoly0D(This, Num)

  class(OrthoPoly_Type), allocatable                                  ::    GetOrthoPoly0D

  class(OrthoMultiVar_Type), intent(in)                               ::    This
  integer, intent(in)                                                 ::    Num

  character(*), parameter                                             ::    ProcName='GetOrthoPoly0D'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (Num > This%NbDim) call Error%Raise(Line='Num specifier above dimension of the multivariate poly', ProcName=ProcName)
  if (Num < 1) call Error%Raise(Line='Num specifier below minimum of 1', ProcName=ProcName)

  allocate(GetOrthoPoly0D, source=This%OrthoPoly(Num)%GetPointer(), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='GetOrthoPoly0D', ProcName=ProcName, stat=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetOrthoPoly1D(This)

  class(OrthoPolyContainer_Type), allocatable, dimension(:)           ::    GetOrthoPoly1D

  class(OrthoMultiVar_Type), intent(in)                               ::    This

  character(*), parameter                                             ::    ProcName='GetOrthoPoly1D'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  allocate(GetOrthoPoly1D, source=This%OrthoPoly, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='GetOrthoPoly1D', ProcName=ProcName, stat=StatLoc)

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetNbDim(This)

  integer                                                             ::    GetNbDim

  class(OrthoMultiVar_Type), intent(in)                               ::    This

  character(*), parameter                                             ::    ProcName='GetNbDim'

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetNbDim = This%NbDim

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(OrthoMultiVar_Type), intent(out)                              ::    LHS
  class(OrthoMultiVar_Type), intent(in)                               ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  call LHS%Reset()

  LHS%Constructed = RHS%Constructed

  if (RHS%Constructed) then
    LHS%NbDim = RHS%NbDim
    allocate(LHS%OrthoPoly, source=RHS%OrthoPoly, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='LHS%OrthoPolyVec', ProcName=ProcName, stat=StatLoc)
  end if

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(OrthoMultiVar_Type), intent(inout)                             ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if(allocated(This%OrthoPoly)) deallocate(This%OrthoPoly, stat=StatLoc)
  if(StatLoc /= 0) call Error%Deallocate(Name='This%OrthoPoly', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
