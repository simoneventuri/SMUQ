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

module ITablePoly_Class

use Input_Library
use Parameters_Library
use StringConversion_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use ITableValue_Class                                             ,only:    ITableValue_Type
use IScalarValue_Class                                            ,only:    IScalarValue_Type
use IScalarValueContainer_Class                                   ,only:    IScalarValueContainer_Type
use IScalarValue_Factory_Class                                    ,only:    IScalarValue_Factory
use SMUQString_Class                                              ,only:    SMUQString_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    ITablePoly_Type

type, extends(ITableValue_Type)                                       ::    ITablePoly_Type
  type(IScalarValueContainer_Type), dimension(:), allocatable         ::    PolyCoeff
  integer                                                             ::    Order
contains
  procedure, public                                                   ::    Reset
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    GetValue
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(ITablePoly_Type), intent(inout)                               ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  if (allocated(This%PolyCoeff)) deallocate(This%PolyCoeff, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%PolyCoeff', ProcName=ProcName, stat=StatLoc)

  This%Constructed = .false.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(ITablePoly_Type), intent(inout)                               ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    VarI0D
  integer                                                             ::    i, ii
  logical                                                             ::    Found
  class(IScalarValue_Type), allocatable                               ::    PolyCoeff
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  ParameterName = 'order'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
  This%Order = VarI0D
  if (This%Order < 0) call Error%Raise(Line='Specified a polynomial of order less than 0', ProcName=ProcName)

  allocate(This%PolyCoeff(This%Order+1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='PolyCoeff', ProcName=ProcName, stat=StatLoc)

  SectionName = 'coefficients'    
  call InputVerifier%AddSection(Section=SectionName)

  i = 1
  do i = 1, This%Order + 1
    SubSectionName = SectionName // '>coefficient' // ConvertToString(i)
    call InputVerifier%AddSection(Section='coefficient' // ConvertToString(i), ToSubSection=SectionName)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
    call IScalarValue_Factory%Construct(Object=PolyCoeff, Input=InputSection, Prefix=PrefixLoc)
    call This%PolyCoeff(i)%Set(Object=PolyCoeff)
    deallocate(PolyCoeff, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='PolyCoeff', ProcName=ProcName, stat=StatLoc)
    nullify(InputSection)
  end do

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This, PolyCoeff)

  class(ITablePoly_Type), intent(inout)                               ::    This
  class(IScalarValueContainer_Type), dimension(:), intent(in)         ::    PolyCoeff

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  call This%Reset()

  allocate(This%PolyCoeff, source=PolyCoeff, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%PolyCoeff', ProcName=ProcName, stat=StatLoc)

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(ITablePoly_Type), intent(in)                                  ::    This
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
  class(IScalarValue_Type), pointer                                   ::    PolyCoeffPointer=>null()

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))
  call GetInput%AddParameter(Name='order', Value=ConvertToString(Value=This%Order))

  SectionName = 'coefficients'
  i = 1
  do i = 1, size(This%PolyCoeff,1)
    PolyCoeffPointer => This%PolyCoeff(i)%GetPointer()
    if (ExternalFlag) DirectorySub = DirectoryLoc // 'coefficient' // ConvertToString(Value=i) // '/'
    SubSectionName = 'coefficient' // ConvertToString(Value=i)
    call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Name=SubSectionName, Object=PolyCoeffPointer,          &
                                                          Prefix=PrefixLoc, Directory=DIrectoryLoc), To_SubSection=SectionName)
    nullify(PolyCoeffPointer)
  end do

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine GetValue(This, Input, Abscissa, Values)

  class(ITablePoly_Type), intent(in)                                  ::    This
  type(Input_Type), intent(in)                                        ::    Input
  real(rkp), dimension(:), intent(in)                                 ::    Abscissa
  real(rkp), dimension(:), intent(inout)                              ::    Values

  character(*), parameter                                             ::    ProcName='GetValue'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i, ii
  class(IScalarValue_Type), pointer                                   ::    PolyCoeffPointer=>null()

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  if (size(Values,1) /= size(Abscissa,1)) call Error%Raise('Incompatible values array', ProcName=ProcName)

  Values = Zero

  ii = 1
  do ii = 1, size(Abscissa,1)
    i = 1
    do i = 1, This%Order+1
      PolyCoeffPointer => This%PolyCoeff(i)%GetPointer()
      Values(ii) = Values(ii) + PolyCoeffPointer%GetValue(Input=Input)*Abscissa(ii)**(i-1)
      nullify(PolyCoeffPointer)
    end do
  end do

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(ITablePoly_Type), intent(out)                                 ::    LHS
  class(ITableValue_Type), intent(in)                                 ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (ITablePoly_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed
      if (RHS%Constructed) then
        LHS%Order = RHS%Order
        allocate(LHS%PolyCoeff, source=RHS%PolyCoeff, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%PolyCoeff', ProcName=ProcName, stat=StatLoc)
      end if

    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(ITablePoly_Type), intent(inout)                                ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%PolyCoeff)) deallocate(This%PolyCoeff, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%PolyCoeff', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
