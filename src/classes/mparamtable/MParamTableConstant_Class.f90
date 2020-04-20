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

module MParamTablePoly_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MParamTable_Class                                             ,only:    MParamTable_Type
use MParamScalar_Class                                            ,only:    MParamScalar_Type
use MParamScalarContainer_Class                                   ,only:    MParamScalarContainer_Type
use MParamScalarFixed_Class                                       ,only:    MParamScalarFixed_Type
use MParamScalar_Factory_Class                                    ,only:    MParamScalar_Factory
use Input_Class                                                   ,only:    Input_Type
use String_Library
use StringRoutines_Module


implicit none

private

public                                                                ::    MParamTablePoly_Type

type, extends(MParamTable_Type)                                       ::    MParamTablePoly_Type
  type(MParamScalarContainer_Type), dimension(:), allocatable         ::    PolyCoeff
  integer                                                             ::    Order
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
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Initialize(This)

  class(MParamTablePoly_Type), intent(inout)                          ::    This

  character(*), parameter                                             ::    ProcName='Initialize'
  if (.not. This%Initialized) then
    This%Name = 'mparamtablepoly'
    This%Initialized = .true.
    call This%SetDefaults()
  end if

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(MParamTablePoly_Type), intent(inout)                          ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  if (allocated(This%PolyCoeff)) deallocate(This%PolyCoeff, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%PolyCoeff', ProcName=ProcName, stat=StatLoc)

  This%Initialized = .false.
  This%Constructed = .false.

  call This%Initialize()

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine SetDefaults(This)

  class(MParamTablePoly_Type), intent(inout)                          ::    This

  character(*), parameter                                             ::    ProcName='SetDefaults'

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(MParamTablePoly_Type), intent(inout)                          ::    This
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
  class(MParamScalar_Type), allocatable                               ::    PolyCoeff
  type(MParamScalarFixed_Type)                                        ::    PolyCoeffScalar
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  ParameterName = 'order'
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
  This%Order = VarI0D
  if (This%Order < 0) call Error%Raise(Line='Specified a polynomial of order less than 0', ProcName=ProcName)

  allocate(This%PolyCoeff(This%Order+1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='PolyCoeff', ProcName=ProcName, stat=StatLoc)

  call PolyCoeffScalar%Construct(Value=Zero)

  SectionName = 'coefficients'    

  i = 1
  ii = 0
  do i = 1, This%Order + 1
    SubSectionName = SectionName // '>coefficient' // Convert_To_String(i)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
    call MParamScalar_Factory%Construct(Object=PolyCoeff, Input=InputSection, Prefix=PrefixLoc)
    call This%PolyCoeff(i)%Set(Object=PolyCoeff)
    deallocate(PolyCoeff, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='PolyCoeff', ProcName=ProcName, stat=StatLoc)
    nullify(InputSection)
  end do

  if (ii <= 0) call Error%Raise(Line='Specified polynomial but no coefficients were given', ProcName=ProcName)

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This, PolyCoeff)

  class(MParamTablePoly_Type), intent(inout)                          ::    This
  class(MParamScalarContainer_Type), dimension(:), intent(in)         ::    PolyCoeff

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  integer                                                             ::    StatLoc=0

  if (This%Constructed) call This%Reset()
  if (.not. This%Initialized) call This%Initialize()

  allocate(This%PolyCoeff, source=PolyCoeff, stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%PolyCoeff', ProcName=ProcName, stat=StatLoc)

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(MParamTablePoly_Type), intent(in)                             ::    This
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
  class(MParamScalar_Type), pointer                                   ::    PolyCoeffPointer=>null()

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
    if (ExternalFlag) DirectorySub = DirectoryLoc // '/coefficient' // ConvertToString(Value=i)
    SubSectionName = 'coefficient' // ConvertToString(Value=i)
    call GetInput%AddSection(Section=MParamScalar_Factory%GetObjectInput(Name=SubSectionName, Object=PolyCoeffPointer,          &
                                                          Prefix=PrefixLoc, Directory=DIrectoryLoc), To_SubSection=SectionName)
    nullify(PolyCoeffPointer)
  end do

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetValue(This, Input, Abscissa)

  real(rkp), allocatable, dimension(:)                                ::    GetValue

  class(MParamTablePoly_Type), intent(in)                             ::    This
  type(Input_Type), intent(in)                                        ::    Input
  real(rkp), dimension(:), intent(in)                                 ::    Abscissa

  character(*), parameter                                             ::    ProcName='GetValue'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i, ii
  class(MParamScalar_Type), pointer                                   ::    PolyCoeffPointer=>null()

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  allocate(GetValue(size(Abscissa,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='GetValue', ProcName=ProcName, stat=StatLoc)

  GetValue = Zero

  ii = 1
  do ii = 1, size(Abscissa,1)
    i = 1
    do i = 1, This%Order+1
      PolyCoeffPointer => This%PolyCoeff(i)%GetPointer()
      GetValue(ii) = GetValue(ii) + PolyCoeffPointer%GetValue(Input=Input)*Abscissa(ii)**(i-1)
      nullify(PolyCoeffPointer)
    end do
  end do

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetCharValue(This, Input, Abscissa, Format)

  type(String_Type), allocatable, dimension(:)                        ::    GetCharValue

  class(MParamTablePoly_Type), intent(in)                             ::    This
  type(Input_Type), intent(in)                                        ::    Input
  real(rkp), dimension(:), intent(in)                                 ::    Abscissa
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='GetCharValue'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i, ii
  class(MParamScalar_Type), pointer                                   ::    PolyCoeffPointer=>null()
  real(rkp)                                                           ::    VarR0D
  character(:), allocatable                                           ::    FormatLoc

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  allocate(GetCharValue(size(Abscissa,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='GetValue', ProcName=ProcName, stat=StatLoc)

  ii = 1
  do ii = 1, size(Abscissa,1)
    VarR0D = Zero
    i = 1
    do i = 1, This%Order+1
      PolyCoeffPointer => This%PolyCoeff(i)%GetPointer()
      VarR0D = VarR0D + PolyCoeffPointer%GetValue(Input=Input)*Abscissa(ii)**(i-1)
      nullify(PolyCoeffPointer)
    end do
    call GetCharValue(ii)%Set_Value(Value=ConvertToString(Value=VarR0D, Format=FormatLoc))
  end do

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(MParamTablePoly_Type), intent(out)                            ::    LHS
  class(MParamTable_Type), intent(in)                                 ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (MParamTablePoly_Type)
      call LHS%Reset()
      LHS%Initialized = RHS%Initialized
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

  type(MParamTablePoly_Type), intent(inout)                           ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%PolyCoeff)) deallocate(This%PolyCoeff, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%PolyCoeff', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
