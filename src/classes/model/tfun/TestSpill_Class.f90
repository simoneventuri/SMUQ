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
!Bliznyuk, N., Ruppert, D., Shoemaker, C., Regis, R., Wild, S., & Mugunthan, P. (2008). 
!Bayesian calibration and uncertainty analysis for computationally expensive models using optimization and radial basis function approximation. 
!Journal of Computational and Graphical Statistics, 17(2).
module TestSpill_Class

use Input_Library
use Parameters_Library
use StringConversion_Module
use ComputingRoutines_Module
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

public                                                                ::    TestSpill_Type

type, extends(TestFunction_Type)                                      ::    TestSpill_Type
  real(rkp), allocatable, dimension(:)                                ::    Location
  real(rkp), allocatable, dimension(:)                                ::    Time
  integer                                                             ::    NbTimes
  integer                                                             ::    NbLocations
  class(IScalarValue_Type), allocatable                               ::    M
  class(IScalarValue_Type), allocatable                               ::    D
  class(IScalarValue_Type), allocatable                               ::    L
  class(IScalarValue_Type), allocatable                               ::    Tau
contains
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, nopass, public                                           ::    ComputeSpill
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(TestSpill_Type), intent(inout)                                ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc=0

  This%Constructed = .false.

  if (allocated(This%Time)) deallocate(This%Time, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Time', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Location)) deallocate(This%Location, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Location', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%M)) deallocate(This%M, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%M', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%D)) deallocate(This%D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%D', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%L)) deallocate(This%L, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%L', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Tau)) deallocate(This%Tau, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Tau', ProcName=ProcName, stat=StatLoc)

  This%NbTimes = 0
  This%NbLocations = 0

  This%Label = 'spill'

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(TestSpill_Type), intent(inout)                                ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
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
  real(rkp), allocatable, dimension(:)                                ::    TimeRange
  logical                                                             ::    MandatoryLoc
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  Found = .false.

  ParameterName = 'response_label'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found, SectionName=SectionName)
  if (Found) This%Label = VarC0D

  ParameterName = 'time_range'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
  call ConvertToReals(String=VarC0D, Values=TimeRange)
  if (size(TimeRange,1) /= 2) call Error%Raise('Incompatible time range specification', ProcName=ProcName)
  if (TimeRange(1) <= 0) call Error%Raise(Line='Minimum time range at or below zero', ProcName=ProcName)
  if (TimeRange(2) < TimeRange(1)) call Error%Raise(Line='Minimum time larger than maximum', ProcName=ProcName)
      
  ParameterName = 'nb_times'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
  This%NbTimes = VarI0D

  allocate(This%Time(This%NbTimes), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Time', ProcName=ProcName, stat=StatLoc)
  call LinSpace(Min=TimeRange(1), Max=TimeRange(2), NbNodes=This%NbTimes, Values=This%Time)

  ParameterName = 'location'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
  call ConvertToReals(String=VarC0D, Values=This%Location, Separator=' ')
  if (any(This%Location > 3.0) .or. any(This%Location < 0.0)) call Error%Raise(Line='Location must be in between 0 and 3',        &
                                                                               ProcName=ProcName) 
  This%NbLocations = size(This%Location)
  if (THis%NbLocations <= 0) call Error%Raise('Must specify at least one location', ProcName=ProcName)

  SectionName = 'parameters'
  call InputVerifier%AddSection(Section=SectionName)

  SubSectionName = SectionName // '>m'
  call InputVerifier%AddSection(Section='m', ToSubSection=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%M, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SubSectionName = SectionName // '>d'
  call InputVerifier%AddSection(Section='d', ToSubSection=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%D, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SubSectionName = SectionName // '>l'
  call InputVerifier%AddSection(Section='l', ToSubSection=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%L, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  SubSectionName = SectionName // '>tau'
  call InputVerifier%AddSection(Section='tau', ToSubSection=SectionName)
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true.)
  call IScalarValue_Factory%Construct(Object=This%Tau, Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  use StringConversion_Module

  type(InputSection_Type)                                             ::    GetInput
  class(TestSpill_Type), intent(in)                                   ::    This
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
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    i
  type(InputSection_Type), pointer                                    ::    InputSection=>null()

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  call GetInput%AddParameter(Name='response_label', Value=This%Label)
  VarC0D = ConvertToString(This%Time(1)) // ' ' // ConvertToString(This%Time(2))
  call GetInput%AddParameter(Name='time_range', Value=VarC0D)
  call GetInput%AddParameter(Name='nb_times', Value=ConvertToString(Value=This%NbTimes))

  call GetInput%AddParameter(Name='location', Value=ConvertToString(Values=This%Location))

  SectionName='parameters'
  call GetInput%AddSection(SectionName=SectionName)
  SubSectionName = 'm'
  if (ExternalFlag) DirectorySub = DirectoryLoc // SubSectionName // '/'
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%M, Name=SubSectionName, Prefix=PrefixLoc,      &
                                                                       Directory=DirectorySub), To_SubSection=SectionName)

  SubSectionName = 'd'
  if (ExternalFlag) DirectorySub = DirectoryLoc // SubSectionName // '/'
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%D, Name=SubSectionName, Prefix=PrefixLoc,      &
                                                                       Directory=DirectorySub), To_SubSection=SectionName)

  SubSectionName = 'l'
  if (ExternalFlag) DirectorySub = DirectoryLoc // SubSectionName // '/'
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%L, Name=SubSectionName, Prefix=PrefixLoc,      &
                                                                       Directory=DirectorySub), To_SubSection=SectionName)

  SubSectionName = 'tau'
  if (ExternalFlag) DirectorySub = DirectoryLoc // SubSectionName // '/'
  call GetInput%AddSection(Section=IScalarValue_Factory%GetObjectInput(Object=This%Tau, Name=SubSectionName, Prefix=PrefixLoc,    &
                                                                       Directory=DirectorySub), To_SubSection=SectionName)

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine Run(This, Input, Output)

  class(TestSpill_Type), intent(inout)                                ::    This
  type(Input_Type), intent(in)                                        ::    Input
  type(Output_Type), intent(inout)                                    ::    Output

  character(*), parameter                                             ::    ProcName='ProcessInput'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:,:)                              ::    Ordinate
  real(rkp)                                                           ::    M
  real(rkp)                                                           ::    D
  real(rkp)                                                           ::    L
  real(rkp)                                                           ::    Tau
  integer                                                             ::    i
  integer                                                             ::    ii
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    NbTimes

  if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

  allocate(Ordinate(This%NbTimes*This%NbLocations,1), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='Ordinate', ProcName=ProcName, stat=StatLoc)

  M = This%M%GetValue(Input=Input)
  D = This%D%GetValue(Input=Input)
  L = This%L%GetValue(Input=Input)
  Tau = This%Tau%GetValue(Input=Input)

  i = 1
  do i = 1, This%NbLocations
    call This%ComputeSpill(M=M, D=D, L=L, Tau=Tau, Location=This%Location(i), Time=This%Time,                              &
                                                              Concentration=Ordinate((i-1)*This%NbTimes+1:i*This%NbTimes,1))
  end do
  call Output%Construct(Values=Ordinate, Label=This%Label)

  deallocate(Ordinate, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='Ordinate', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine ComputeSpill(M, D, L, Tau, Location, Time, Concentration)

  real(rkp), intent(in)                                               ::    M
  real(rkp), intent(in)                                               ::    D
  real(rkp), intent(in)                                               ::    L
  real(rkp), intent(in)                                               ::    Tau
  real(rkp), intent(in)                                               ::    Location
  real(rkp), dimension(:), intent(in)                                 ::    Time
  real(rkp), dimension(:), intent(inout)                              ::    Concentration

  character(*), parameter                                             ::    ProcName='ComputeSpill'
  real(rkp)                                                           ::    VarR0D
  integer                                                             ::    i

  if (size(Concentration) /= size(Time)) call Error%Raise(Line='Incompatible time and concentration arrays',                 &
                                                                                                              ProcName=ProcName)   

  i = 1
  do i = 1, size(Time)
    if (Tau < Time(i)) then
      VarR0D = M/dsqrt(Four*pi*D*(Time(i)-Tau))*dexp(-(Location-L)**2/(Four*D*(Time(i)-Tau))) 
    else
      VarR0D = Zero
    end if
    Concentration(i) = M/dsqrt(Four*pi*D*Time(i))*dexp(-Location**2/(Four*D*Time(i))) + VarR0D
    Concentration(i) = Concentration(i)*dsqrt(Four*pi)
  end do

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(TestSpill_Type), intent(out)                                  ::    LHS
  class(TestFunction_Type), intent(in)                                ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)
    type is (TestSpill_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed
      if(RHS%Constructed) then
        LHS%NbLocations = RHS%NbLocations
        LHS%NbTimes = RHS%NbTimes
        allocate(LHS%Location, source=RHS%Location, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Location', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%Time, source=RHS%Time, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Time', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%M, source=RHS%M, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%M', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%D, source=RHS%D, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%D', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%L, source=RHS%L, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%L', ProcName=ProcName, stat=StatLoc)
        allocate(LHS%Tau, source=RHS%Tau, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Tau', ProcName=ProcName, stat=StatLoc)
      end if
    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)
  end select

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(TestSpill_Type), intent(inout)                                 ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Time)) deallocate(This%Time, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Time', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Location)) deallocate(This%Location, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Location', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%M)) deallocate(This%M, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%M', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%D)) deallocate(This%D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%D', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%L)) deallocate(This%L, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%L', ProcName=ProcName, stat=StatLoc)

  if (allocated(This%Tau)) deallocate(This%Tau, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Tau', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
