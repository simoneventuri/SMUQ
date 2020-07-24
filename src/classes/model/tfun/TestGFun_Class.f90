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

module TestGFun_Class

use Input_Library
use Parameters_Library
use StringConversion_Module
use TestFunction_Class                                            ,only:    TestFunction_Type
use Output_Class                                                  ,only:    Output_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use IScalarValue_Class                                            ,only:    IScalarValue_Type
use IScalarValue_Factory_Class                                    ,only:    IScalarValue_Factory
use IScalarValueContainer_Class                                   ,only:    IScalarValueContainer_Type
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    TestGFun_Type

type, extends(TestFunction_Type)                                      ::    TestGFun_Type
  integer                                                             ::    NbParams=0
  type(IScalarValueContainer_Type), allocatable, dimension(:)         ::    Parameters
  real(rkp), allocatable, dimension(:)                                ::    c
contains
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, nopass, public                                           ::    ComputeGFun
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(TestGFun_Type), intent(inout)                                 ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed = .false.

  This%NbParams = 0

  if (allocated(This%c)) deallocate(This%c, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%c', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Parameters)) deallocate(This%Parameters, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Parameters', ProcName=ProcName, stat=StatLoc)
  This%NbParams = 0
  
  This%Label = 'gfunction'

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  use StringConversion_Module

  class(TestGFun_Type), intent(inout)                                 ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ProcessInput'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  logical                                                             ::    Found
  real(rkp)                                                           ::    VarR0D
  integer                                                             ::    VarI0D
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    i
  logical                                                             ::    MandatoryLoc
  class(IScalarValue_Type), allocatable                               ::    ScalarParameter
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  ParameterName = 'response_label'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
  This%Label = VarC0D

  ParameterName = 'nb_dimensions'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
  This%NbParams = VarI0D

  allocate(This%Parameters(This%NbParams), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Parameters', ProcName=ProcName, stat=StatLoc)

  SectionName = 'parameters'
  call InputVerifier%AddSection(Section=SectionName)

  if (allocated(ScalarParameter)) deallocate(ScalarParameter, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='ScalarParameter', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, This%NbParams
    SubSectionName = SectionName // '>x' // ConvertToString(Value=i)
    call InputVerifier%AddSection(Section='x' // ConvertToString(Value=i), ToSubSection=SectionName)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
    call IScalarValue_Factory%Construct(Object=ScalarParameter, Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)
    call This%Parameters(i)%Set(Object=ScalarParameter)
    deallocate(ScalarParameter, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='ScalarParameter', ProcName=ProcName, stat=StatLoc)
  end do

  allocate(This%c(This%NbParams), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='c', ProcName=ProcName, stat=StatLoc)
  SectionName = 'c'
  call InputVerifier%AddSection(Section=SectionName)
  if (Input%HasSection(SubSectionName=SectionName, CaseSensitive=.false.)) then
    i = 1
    do i = 1, This%NbParams
      ParameterName = 'c' // ConvertToString(Value=i)
      call InputVerifier%AddParameter(Parameter=ParameterName, ToSubSection=SectionName)
      call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
      This%c(i) = VarR0D
    end do
  else
    i = 1
    do i = 1, This%NbParams
      This%c(i) = (real(i,8) - Two) / Two
    end do
  end if

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput
  class(TestGFun_Type), intent(in)                                    ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    SubSectionName
  character(:), allocatable                                           ::    ParameterName
  integer                                                             ::    i
  class(IScalarValue_Type), pointer                                   ::    ScalarParameterPtr=>null()

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  call GetInput%AddParameter(Name='nb_dimensions', Value=ConvertToString(Value=This%NbParams))
  call GetInput%AddParameter(Name='response_label', Value=This%Label)

  SectionName='parameters'
  call GetInput%AddSection(SectionName=SectionName)
  i = 1
  do i = 1, This%NbParams
    SubSectionName = 'x' // ConvertToString(Value=i)
    if (ExternalFlag) DirectorySub = DirectoryLoc // SubSectionName // '/'
    ScalarParameterPtr => This%Parameters(i)%GetPointer()
    call GetInput%AddSection(Section=ScalarParameterPtr%GetInput(Name=SubSectionName, Prefix=PrefixLoc, Directory=DirectorySub),  &
                                                                                                         To_SubSection=SectionName)
    nullify(ScalarParameterPtr)
  end do

  SectionName='c'
  call GetInput%AddSection(SectionName=SectionName)
  i = 1
  do i = 1, This%NbParams
    call GetInput%AddParameter(Name='c' // ConvertToString(Value=i), Value=ConvertToString(Value=This%c(i)),                   &
                                                                                                        SectionName=SectionName)
  end do


end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Run(This, Input, Output)

  class(TestGFun_Type), intent(inout)                                 ::    This
  type(Input_Type), intent(in)                                        ::    Input
  type(Output_Type), intent(inout)                                    ::    Output

  character(*), parameter                                             ::    ProcName='ProcessInput'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:,:)                              ::    Ordinate
  real(rkp), allocatable, dimension(:)                                ::    X
  integer                                                             ::    i
  integer                                                             ::    ii
  integer                                                             ::    iii
  character(:), allocatable                                           ::    VarC0D
  class(IScalarValue_Type), pointer                                   ::    ScalarParameterPtr=>null()

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  allocate(Ordinate(1,1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Ordinate', ProcName=ProcName, stat=StatLoc)
  allocate(X(This%NbParams), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='X', ProcName=ProcName, stat=StatLoc)
  ii = 1
  do ii = 1, This%NbParams
    ScalarParameterPtr => This%Parameters(ii)%GetPointer()
    X(ii) = ScalarParameterPtr%GetValue(Input=Input)
    nullify(ScalarParameterPtr)
  end do
  Ordinate(1,1) = This%ComputeGFun(X, This%c)

  deallocate(X, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='X', ProcName=ProcName, stat=StatLoc)

  call Output%Construct(Values=Ordinate, Label=This%Label)

  deallocate(Ordinate, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Ordinate', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function ComputeGFun(X, c)

  real(rkp)                                                           ::    ComputeGFun

  real(rkp), dimension(:), intent(in)                                 ::    X
  real(rkp), dimension(:), intent(in)                                 ::    c

  character(*), parameter                                             ::    ProcName='ComputeGFun'
  integer                                                             ::    NbParams
  integer                                                             ::    i

  NbParams = size(X,1)

  ComputeGFun = One
  i = 1
  do i = 1, NbParams
      ComputeGFun = ComputeGFun * ((abs(Four*X(i)-Two)+c(i)) / (One+c(i)))
  end do

end function
!!--------------------------------------------------------------------------------------------------------------------------------


!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(TestGFun_Type), intent(out)                                   ::    LHS
  class(TestFunction_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)
    type is (TestGFun_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed
      if (RHS%Constructed) then
        LHS%Label = RHS%Label
        LHS%NbParams = RHS%NbParams
        allocate(LHS%Parameters, source=RHS%Parameters, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Parameters', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%c, source=RHS%c, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%c', ProcName=ProcName, stat=StatLoc)
      end if
    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)
  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(TestGFun_Type), intent(inout)                                  ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%c)) deallocate(This%c, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%c', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Parameters)) deallocate(This%Parameters, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Parameters', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
