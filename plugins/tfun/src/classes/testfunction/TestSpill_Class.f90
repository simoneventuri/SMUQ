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
  integer                                                             ::    NbOutputs
  type(String_Type), allocatable, dimension(:)                        ::    AbscissaName
  type(String_Type), allocatable, dimension(:)                        ::    ResponseName
  type(String_Type), allocatable, dimension(:)                        ::    Label
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

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize( This, Debug )

    class(TestSpill_Type), intent(inout)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )
    if ( present(Debug) ) DebugLoc = Debug

    if ( .not. This%Initialized ) then
      This%Name = 'spill'
      This%Initialized = .true.
    end if

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset( This, Debug )

    class(TestSpill_Type), intent(inout)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized = .false.
    This%Constructed = .false.

    if ( allocated(This%Time) ) deallocate(This%Time, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Time', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Location) ) deallocate(This%Location, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Location', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Label) ) deallocate(This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%AbscissaName) ) deallocate(This%AbscissaName, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%AbscissaName', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ResponseName) ) deallocate(This%ResponseName, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ResponseName', ProcName=ProcName, stat=StatLoc )

    This%NbOutputs = 0

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults( This, Debug )

    class(TestSpill_Type), intent(inout)                              ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%M = 10.0
    This%D = 0.1
    This%L = 2
    This%Tau = 30.2
    This%M_Dependency = ''
    This%D_Dependency = ''
    This%L_Dependency = ''
    This%Tau_Dependency = ''

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(TestSpill_Type), intent(inout)                              ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
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
    integer                                                           ::    NbTimes
    logical                                                           ::    MandatoryLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    Found = .false.

    SectionName = 'Output'
    i = 0
    do
      SubSectionName = SectionName // '>output' // ConvertToString(Value=i)
      if ( .not. Input%HasSection( SubSectionName=SubSectionName ) ) exit
      i = i + 1
    end do
    This%NbOutputs = i

    if ( i <= 0 ) call Error%Raise( Line='Found no outputs in the input deck', ProcName=ProcName )

    ParameterName = 'time_range'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
    TimeRange = ConvertToRealrkps(String=VarC0D)
    if ( TimeRange(1) < 0 ) call Error%Raise( Line='Minimum time range below zero', ProcName=ProcName )
    if ( TimeRange(2) < TimeRange(1) ) call Error%Raise( Line='Minimum time larger than maximum', ProcName=ProcName )
        
    ParameterName = 'nb_times'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
    NbTimes = VarI0D

    This%Time = LinSpace(TimeRange(1), TimeRange(2), NbTimes)

    allocate(This%Label(This%NbOutputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    allocate(This%ResponseName(This%NbOutputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ResponseName', ProcName=ProcName, stat=StatLoc )

    allocate(This%AbscissaName(This%NbOutputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%AbscissaName', ProcName=ProcName, stat=StatLoc )

    allocate(This%Location(This%NbOutputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Location', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbOutputs
      SubSectionName = SectionName // '>output' // ConvertToString(Value=i)

      ParameterName = 'location'
      call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      This%Location(i) = VarR0D
      if ( This%Location(i) > 3.0 .or. This%Location(i) < 0.0 ) call Error%Raise( Line='Location must be in between 0 and 3',     &
                                                                                                               ProcName=ProcName ) 

      ParameterName = 'label'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      This%Label(i) = VarC0D

      ParameterName = 'response_name'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.false., Found=Found )
      if ( Found ) then
        This%ResponseName(i) = VarC0D
      else
        This%ResponseName(i) = This%Label(i)
      end if

      ParameterName = 'abscissa_name'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.false., Found=Found )
      if ( Found ) then
        This%AbscissaName(i) = VarC0D
      else
        This%AbscissaName(i) = 'time at s=' // ConvertToString(Value=This%Location(i)) // ', (s)'
      end if

    end do

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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(TestSpill_Type), intent(in)                                 ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
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

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    SectionName = 'output'
    call GetInput%AddSection( SectionName=SectionName )
    VarC0D = ConvertToString(This%Time(1)) // ' ' // ConvertToString(This%Time(2))
    call GetInput%AddParameter( Name='time_range', Value=VarC0D, SectionName=SectionName )
    call GetInput%AddParameter( Name='nb_times', Value=ConvertToString(Value=size(This%Time)), SectionName=SectionName )
    i = 1
    do i = 1, This%NbOutputs
      SubSectionName = 'output' // ConvertToString(Value=i)
      call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
      SubSectionName = SectionName // '>' // SubSectionName
      call GetInput%AddParameter( Name='location', Value=ConvertToString(Value=This%Location(i)), SectionName=SubSectionName )
      call GetInput%AddParameter( Name='label', Value=This%Label(i)%GetValue(), SectionName=SubSectionName )
      call GetInput%AddParameter( Name='response_name', Value=This%ResponseName(i)%GetValue(), SectionName=SubSectionName )
      call GetInput%AddParameter( Name='abscissa_name', Value=This%AbscissaName(i)%GetValue(), SectionName=SubSectionName )
    end do

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

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Run( This, Input, Output, Debug )

    class(TestSpill_Type), intent(inout)                              ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), dimension(:), allocatable, intent(inout)       ::    Output
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
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

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( .not. allocated(Output) ) then
      allocate( Output(This%NbOutputs), stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
    else
      if ( size(Output,1) /= This%NbOutputs ) then
        deallocate(Output, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )
        allocate( Output(This%NbOutputs), stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
      end if
    end if

    select type (Input)
      type is (InputDet_Type)
        allocate(Ordinate(size(This%Time),1), stat=StatLoc)
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
        do i = 1, This%NbOutputs
          call This%ComputeSpill( M=M, D=D, L=L, Tau=Tau, Location=This%Location(i), Time=This%Time, Concentration=Ordinate(:,1) )
          call Output(i)%Construct( Abscissa=This%Time, Ordinate=Ordinate, AbscissaName=This%AbscissaName(i)%GetValue(),          &
                                                     OrdinateName=This%ResponseName(i)%GetValue(), Label=This%Label(i)%GetValue())
        end do
      type is (InputStoch_Type)
        allocate(Ordinate(size(This%Time),Input%GetNbDegen()), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
        i = 1
        do i = 1, This%NbOutputs
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
                                                                                                    Concentration=Ordinate(:,ii) )
          end do
          call Output(i)%Construct( Abscissa=This%Time, Ordinate=Ordinate, AbscissaName=This%AbscissaName(i)%GetValue(),          &
                                                     OrdinateName=This%ResponseName(i)%GetValue(), Label=This%Label(i)%GetValue())
        end do

      class default
        call Error%Raise( Line='Update input type definitions', ProcName=ProcName )
    end select

    deallocate(Ordinate, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ComputeSpill( M, D, L, Tau, Location, Time, Concentration, Debug )

    real(rkp), intent(in)                                             ::    M
    real(rkp), intent(in)                                             ::    D
    real(rkp), intent(in)                                             ::    L
    real(rkp), intent(in)                                             ::    Tau
    real(rkp), intent(in)                                             ::    Location
    real(rkp), dimension(:), intent(in)                               ::    Time
    real(rkp), dimension(:), intent(inout)                            ::    Concentration
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeSpill'
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

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
    end do

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy( LHS, RHS )

    class(TestSpill_Type), intent(out)                                ::    LHS
    class(TestFunction_Type), intent(in)                              ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
      type is (TestSpill_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if( RHS%Constructed ) then
          LHS%AbscissaName = RHS%AbscissaName
          LHS%ResponseName = RHS%ResponseName
          LHS%NbOutputs = RHS%NbOutputs
          allocate(LHS%Label, source=RHS%Label, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Label', ProcName=ProcName, stat=StatLoc )
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

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Finalizer( This )

    type(TestSpill_Type), intent(inout)                               ::    This

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%Time) ) deallocate(This%Time, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Time', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Location) ) deallocate(This%Location, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Location', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Label) ) deallocate(This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%AbscissaName) ) deallocate(This%AbscissaName, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%AbscissaName', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ResponseName) ) deallocate(This%ResponseName, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ResponseName', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
