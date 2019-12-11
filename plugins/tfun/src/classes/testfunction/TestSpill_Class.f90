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
!Bliznyuk, N., Ruppert, D., Shoemaker, C., Regis, R., Wild, S., & Mugunthan, P. (2008). 
!Bayesian calibration and uncertainty analysis for computationally expensive models using optimization and radial basis function approximation. 
!Journal of Computational and Graphical Statistics, 17(2).
module TestSpill_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use ComputingRoutines_Module
use TestFunction_Class                                            ,only:    TestFunction_Type
use Output_Class                                                  ,only:    Output_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type

implicit none

private

public                                                                ::    TestSpill_Type

type, extends(TestFunction_Type)                                      ::    TestSpill_Type
  real(rkp), allocatable, dimension(:)                                ::    Location
  real(rkp), allocatable, dimension(:)                                ::    Time
  integer                                                             ::    NbTimes
  integer                                                             ::    NbLocations
  real(rkp)                                                           ::    M
  real(rkp)                                                           ::    D
  real(rkp)                                                           ::    L
  real(rkp)                                                           ::    Tau
  character(:), allocatable                                           ::    M_Dependency
  character(:), allocatable                                           ::    D_Dependency
  character(:), allocatable                                           ::    L_Dependency
  character(:), allocatable                                           ::    Tau_Dependency
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, nopass, public                                           ::    ComputeSpill
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(TestSpill_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'spill'
      This%Initialized = .true.
    end if

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(TestSpill_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    if ( allocated(This%Time) ) deallocate(This%Time, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Time', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Location) ) deallocate(This%Location, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Location', ProcName=ProcName, stat=StatLoc )

    This%NbTimes = 0
    This%NbLocations = 0

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(TestSpill_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'
    integer                                                           ::    StatLoc=0

    This%M = 10.0
    This%D = 0.1
    This%L = 2
    This%Tau = 30.2
    This%M_Dependency = ''
    This%D_Dependency = ''
    This%L_Dependency = ''
    This%Tau_Dependency = ''
    This%Label = 'spill'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(TestSpill_Type), intent(inout)                              ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    i
    real(rkp), dimension(2)                                           ::    TimeRange
    logical                                                           ::    MandatoryLoc

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    Found = .false.

    ParameterName = 'label'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found, SectionName=SectionName )
    if ( Found ) This%Label = VarC0D

    ParameterName = 'time_range'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
    TimeRange = ConvertToReals(String=VarC0D)
    if ( TimeRange(1) <= 0 ) call Error%Raise( Line='Minimum time range at or below zero', ProcName=ProcName )
    if ( TimeRange(2) < TimeRange(1) ) call Error%Raise( Line='Minimum time larger than maximum', ProcName=ProcName )
        
    ParameterName = 'nb_times'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
    This%NbTimes = VarI0D

    This%Time = LinSpace(TimeRange(1), TimeRange(2), This%NbTimes)

    ParameterName = 'location'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    This%Location = ConvertToReals( String=VarC0D )
    if ( any(This%Location > 3.0) .or. any(This%Location < 0.0) ) call Error%Raise( Line='Location must be in between 0 and 3',   &
                                                                                                               ProcName=ProcName ) 
    This%NbLocations = size(This%Location)
    if ( THis%NbLocations <= 0 ) call Error%Raise( 'Must specify at least one location', ProcName=ProcName )

    SectionName = 'parameters'

    ParameterName = 'm_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) This%M_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'm'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%M = VarR0D

    ParameterName = 'd_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) This%D_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'd'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%D = VarR0D

    ParameterName = 'l_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) This%L_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'l'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%L = VarR0D

    ParameterName = 'tau_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) This%Tau_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'tau'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%Tau = VarR0D

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(TestSpill_Type), intent(in)                                 ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='label', Value=This%Label )
    VarC0D = ConvertToString(This%Time(1)) // ' ' // ConvertToString(This%Time(2))
    call GetInput%AddParameter( Name='time_range', Value=VarC0D )
    call GetInput%AddParameter( Name='nb_times', Value=ConvertToString(Value=This%NbTimes ) )

    call GetInput%AddParameter( Name='location', Value=ConvertToString(Values=This%Location) )

    SectionName='parameters'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%AddParameter( Name='m', Value=ConvertToString(Value=This%M), SectionName=SectionName )
    call GetInput%AddParameter( Name='d', Value=ConvertToString(Value=This%D), SectionName=SectionName )
    call GetInput%AddParameter( Name='l', Value=ConvertToString(Value=This%L), SectionName=SectionName )
    call GetInput%AddParameter( Name='tau', Value=ConvertToString(Value=This%Tau), SectionName=SectionName )
    if ( len_trim(This%M_Dependency) /= 0 ) call GetInput%AddParameter( Name='m_dependency', Value=This%M_Dependency,             &
                                                                                                         SectionName=SectionName )
    if ( len_trim(This%D_Dependency) /= 0 ) call GetInput%AddParameter( Name='d_dependency', Value=This%D_Dependency,             &
                                                                                                         SectionName=SectionName )
    if ( len_trim(This%L_Dependency) /= 0 ) call GetInput%AddParameter( Name='l_dependency', Value=This%L_Dependency,             &
                                                                                                         SectionName=SectionName )
    if ( len_trim(This%Tau_Dependency) /= 0 ) call GetInput%AddParameter( Name='tau_dependency', Value=This%Tau_Dependency,       &
                                                                                                         SectionName=SectionName )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run( This, Input, Output )

    class(TestSpill_Type), intent(inout)                              ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), intent(inout)                                  ::    Output

    character(*), parameter                                           ::    ProcName='ProcessInput'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                            ::    Ordinate
    real(rkp)                                                         ::    M
    real(rkp)                                                         ::    D
    real(rkp)                                                         ::    L
    real(rkp)                                                         ::    Tau
    integer                                                           ::    i
    integer                                                           ::    ii
    character(:), allocatable                                         ::    VarC0D
    type(InputDet_Type)                                               ::    InputLoc
    integer                                                           ::    NbTimes

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    select type (Input)
      type is (InputDet_Type)
        allocate(Ordinate(This%NbTimes*This%NbLocations,1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
        if ( len_trim(This%M_Dependency) /= 0 ) then
          call Input%GetValue( Value=M, Label=This%M_Dependency )
        else
          M = This%M
        end if
        if ( len_trim(This%D_Dependency) /= 0 ) then
          call Input%GetValue( Value=D, Label=This%D_Dependency )
        else
          D = This%D
        end if
        if ( len_trim(This%L_Dependency) /= 0 ) then
          call Input%GetValue( Value=L, Label=This%L_Dependency )
        else
          L = This%L
        end if
        if ( len_trim(This%Tau_Dependency) /= 0 ) then
          call Input%GetValue( Value=Tau, Label=This%Tau_Dependency )
        else
          Tau = This%Tau
        end if
        i = 1
        do i = 1, This%NbLocations
          call This%ComputeSpill( M=M, D=D, L=L, Tau=Tau, Location=This%Location(i), Time=This%Time,                              &
                                                                   Concentration=Ordinate((i-1)*This%NbTimes+1:i*This%NbTimes,1) )
        end do
        call Output%Construct( Values=Ordinate, Label=This%Label )
      type is (InputStoch_Type)
        allocate(Ordinate(This%NbTimes*This%NbLocations,Input%GetNbDegen()), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
        i = 1
        do i = 1, This%NbLocations
          ii = 1
          do ii = 1, Input%GetNbDegen()
            InputLoc = Input%GetDetInput(Num=ii)
            if ( len_trim(This%M_Dependency) /= 0 ) then
              call InputLoc%GetValue( Value=M, Label=This%M_Dependency )
            else
              M = This%M
            end if
            if ( len_trim(This%D_Dependency) /= 0 ) then
              call InputLoc%GetValue( Value=D, Label=This%D_Dependency )
            else
              D = This%D
            end if
            if ( len_trim(This%L_Dependency) /= 0 ) then
              call InputLoc%GetValue( Value=L, Label=This%L_Dependency )
            else
              L = This%L
            end if
            if ( len_trim(This%Tau_Dependency) /= 0 ) then
              call InputLoc%GetValue( Value=Tau, Label=This%Tau_Dependency )
            else
              Tau = This%Tau
            end if
            call This%ComputeSpill( M=M, D=D, L=L, Tau=Tau, Location=This%Location(i), Time=This%Time,                            &
                                                                  Concentration=Ordinate((i-1)*This%NbTimes+1:i*This%NbTimes,ii) )
          end do
        end do
        call Output%Construct( Values=Ordinate, Label=This%Label )
      class default
        call Error%Raise( Line='Update input type definitions', ProcName=ProcName )
    end select

    deallocate(Ordinate, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ComputeSpill( M, D, L, Tau, Location, Time, Concentration )

    real(rkp), intent(in)                                             ::    M
    real(rkp), intent(in)                                             ::    D
    real(rkp), intent(in)                                             ::    L
    real(rkp), intent(in)                                             ::    Tau
    real(rkp), intent(in)                                             ::    Location
    real(rkp), dimension(:), intent(in)                               ::    Time
    real(rkp), dimension(:), intent(inout)                            ::    Concentration

    character(*), parameter                                           ::    ProcName='ComputeSpill'
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    i

    if ( size(Concentration) /= size(Time) ) call Error%Raise( Line='Incompatible time and concentration arrays',                 &
                                                                                                               ProcName=ProcName )   

    i = 1
    do i = 1, size(Time)
      if ( Tau < Time(i) ) then
        VarR0D = M/dsqrt(Four*pi*D*(Time(i)-Tau))*dexp(-(Location-L)**2/(Four*D*(Time(i)-Tau))) 
      else
        VarR0D = Zero
      end if
      Concentration(i) = M/dsqrt(Four*pi*D*Time(i))*dexp(-Location**2/(Four*D*Time(i))) + VarR0D
      Concentration(i) = Concentration(i)*dsqrt(Four*pi)
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(TestSpill_Type), intent(out)                                ::    LHS
    class(TestFunction_Type), intent(in)                              ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
      type is (TestSpill_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if( RHS%Constructed ) then
          LHS%NbLocations = RHS%NbLocations
          LHS%NbTimes = RHS%NbTimes
          allocate(LHS%Location, source=RHS%Location, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Location', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%Time, source=RHS%Time, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Time', ProcName=ProcName, stat=StatLoc )
          LHS%M = RHS%M
          LHS%D = RHS%D
          LHS%L = RHS%L
          LHS%Tau = RHS%Tau
          LHS%M_Dependency = RHS%M_Dependency
          LHS%D_Dependency = RHS%D_Dependency
          LHS%L_Dependency = RHS%L_Dependency
          LHS%Tau_Dependency = RHS%Tau_Dependency
        end if
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(TestSpill_Type), intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%Time) ) deallocate(This%Time, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Time', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Location) ) deallocate(This%Location, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Location', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
