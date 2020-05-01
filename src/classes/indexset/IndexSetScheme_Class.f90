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

module IndexSetScheme_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StringRoutines_Module
use IndexSet_Class                                                ,only:    IndexSet_Type
use IndexHyperbolic_Class                                         ,only:    IndexHyperbolic_Type
use IndexSet_Factory_Class                                        ,only:    IndexSet_Factory
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    IndexSetScheme_Type

type                                                                  ::    IndexSetScheme_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  class(IndexSet_Type), allocatable                                   ::    IndexSet
  integer                                                             ::    Order=1
  integer                                                             ::    MinOrder=1
  integer                                                             ::    MaxOrder=huge(1)
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    GetIndexSet
  procedure, public                                                   ::    GetIndexSetPointer
  procedure, public                                                   ::    GetOrder
  procedure, public                                                   ::    GetMaxOrder
  procedure, public                                                   ::    GetMinOrder
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(IndexSetScheme_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer(8)                                                        ::    SysTimeCount

    if (.not. This%Initialized) then
      This%Initialized = .true.
      This%Name = 'lhs'
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(IndexSetScheme_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%IndexSet%Reset()

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(IndexSetScheme_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%Order = 1
    This%MinOrder = 1
    This%MaxOrder = huge(1)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput (This, Input, Prefix)

    class(IndexSetScheme_Type), intent(inout)                         ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    Found
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'order'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
    This%Order = VarI0D

    ParameterName = 'max_order'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
    This%MaxOrder = VarI0D

    ParameterName = 'min_order'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
    This%MinOrder = VarI0D

    if (This%MinOrder < 0) call Error%Raise(Line='Min order set lower than 0', ProcName=ProcName)
    if (This%MaxOrder < This%MinOrder) call Error%Raise(Line='Max Order set lower than min order', ProcName=ProcName)
    if (This%Order > This%MaxOrder .or. This%Order < This%MinOrder) call Error%Raise(                                        &
                                                               Line='Specified order that exceeds min or max', ProcName=ProcName)  

    SectionName = 'index_set'
    if (Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call IndexSet_Factory%Construct(Object=This%IndexSet, Input=InputSection, Prefix=PrefixLoc)
    else
      allocate(IndexHyperbolic_Type :: This%IndexSet)
      select type (Object => This%IndexSet)
        type is (IndexHyperbolic_Type)
          call Object%Construct(NormQ=0.4_rkp)
        class default
          call Error%Raise('Something went wrong', ProcName=ProcName)
      end select
    end if

    This%Constructed = .true.

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1 (This, Order, MinOrder, MaxOrder, IndexSet)

    integer, optional, intent(in)                                     ::    Order
    integer, optional, intent(in)                                     ::    MinOrder
    integer, optional, intent(in)                                     ::    MaxOrder
    class(IndexSet_Type), optional, intent(in)                        ::    IndexSet

    class(IndexSetScheme_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    This%Order = 1
    if (present(Order)) This%Order = Order

    This%MinOrder = 1
    if (present(MinOrder)) This%MinOrder = MinOrder

    This%MaxOrder = huge(1)
    if (present(MaxOrder)) This%MaxOrder = MaxOrder

    if (This%MinOrder < 0) call Error%Raise(Line='Min order set lower than 0', ProcName=ProcName)
    if (This%MaxOrder < This%MinOrder) call Error%Raise(Line='Max Order set lower than min order', ProcName=ProcName)
    if (This%Order > This%MaxOrder .or. This%Order < This%MinOrder) call Error%Raise(                                        &
                                                               Line='Specified order that exceeds min or max', ProcName=ProcName)

    if (present(IndexSet)) then
      if (.not. IndexSet%IsConstructed()) call Error%Raise('Index set not constructed', ProcName=ProcName)
      allocate(This%IndexSet, source=IndexSet, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%IndexSet', ProcName=ProcName, stat=StatLoc)
    else
      allocate(IndexHyperbolic_Type :: This%IndexSet)
      select type (Object => This%IndexSet)
        type is (IndexHyperbolic_Type)
          call Object%Construct(NormQ=0.4_rkp)
        class default
          call Error%Raise('Something went wrong', ProcName=ProcName)
      end select
    end if

    This%Constructed = .true.

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput
    class(IndexSetScheme_Type), intent(in)                            ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    FileName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%AddParameter(Name='order', Value=ConvertToString(Value=This%Order))
    call GetInput%AddParameter(Name='min_order', Value=ConvertToString(Value=This%MinOrder))
    call GetInput%AddParameter(Name='max_order', Value=ConvertToString(Value=This%MaxOrder))

    if (ExternalFlag) DirectorySub = DirectoryLoc // '/index_set'
    call GetInput%AddSection(Section=IndexSet_Factory%GetObjectInput(Object=This%IndexSet, Name='index_set',                  &
                                                                         Prefix=PrefixLoc, Directory=DirectorySub))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetIndexSet(This)

    class(IndexSet_Type), allocatable                                 ::    GetIndexSet

    class(IndexSetScheme_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='GetIndexSet'
    integer                                                           ::    StatLoc=0

    allocate(GetIndexSet, source=This%IndexSet, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='GetIndexSet', ProcName=ProcName, stat=StatLoc)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetIndexSetPointer(This)

    class(IndexSet_Type), pointer                                     ::    GetIndexSetPointer

    class(IndexSetScheme_Type), target, intent(in)                    ::    This

    character(*), parameter                                           ::    ProcName='GetIndexSetPointer'

    GetIndexSetPointer => This%IndexSet

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetOrder(This)

    integer                                                           ::    GetOrder

    class(IndexSetScheme_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='GetOrder'

    GetOrder = This%Order

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMinOrder(This)

    integer                                                           ::    GetMinOrder

    class(IndexSetScheme_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='GetMinOrder'

    GetMinOrder = This%MinOrder

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMaxOrder(This)

    integer                                                           ::    GetMaxOrder

    class(IndexSetScheme_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='GetMaxOrder'

    GetMaxOrder = This%MaxOrder

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(IndexSetScheme_Type), intent(out)                           ::    LHS
    class(IndexSetScheme_Type), intent(in)                            ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    call LHS%Reset()
    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed

    if (RHS%Constructed) then
      LHS%Order = RHS%Order
      LHS%MinOrder = RHS%MinOrder
      LHS%MaxOrder = RHS%MaxOrder
      allocate(LHS%IndexSet, source=RHS%IndexSet, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='LHS%IndexSet', ProcName=ProcName, stat=StatLoc)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(IndexSetScheme_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%IndexSet)) deallocate(This%IndexSet, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%IndexSet', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
