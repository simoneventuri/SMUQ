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

module SMD_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Model_Class                                                   ,only:    Model_Type
use ModelExtTemplate_Class                                        ,only:    ModelExtTemplate_Type
use Output_Class                                                  ,only:    Output_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type

implicit none

private

public                                                                ::    SMD_Type

type, extends(ModelExtTemplate_Type)                                  ::    SMD_Type
  character(:), allocatable                                           ::    Label
  real(rkp), dimension(2)                                             ::    IC=Zero
  real(rkp), allocatable, dimension(:)                                ::    Abscissa
  real(rkp)                                                           ::    RTOL=1.0d-6
  real(rkp)                                                           ::    ATOL=1.0d-10
  real(rkp)                                                           ::    M
  real(rkp)                                                           ::    K
  real(rkp)                                                           ::    C
  character(:), allocatable                                           ::    M_Dependency
  character(:), allocatable                                           ::    K_Dependency
  character(:), allocatable                                           ::    C_Dependency
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    RunCase1
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(SMD_Type), intent(inout)                                    ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'smd'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(SMD_Type), intent(inout)                                    ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    if ( allocated(This%Abscissa) ) deallocate(This%Abscissa, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Abscissa', ProcName=ProcName, stat=StatLoc )

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(SMD_Type), intent(inout)                                    ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%Label = 'smd'
    This%IC = Zero
    This%RTOL = 1.0d-6
    This%ATOL = 1.0d-10
    This%M = 1.0
    This%K = 1.1
    This%C = 1.2
    This%M_Dependency = ''
    This%K_Dependency = ''
    This%C_Dependency = ''

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    use String_Library

    class(SMD_Type), intent(inout)                                    ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    logical                                                           ::    Found
    character(:), allocatable                                         ::    VarC0D
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer                                                           ::    VarI0D
    logical                                                           ::    MandatoryLoc

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'label'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Label = VarC0D

    SectionName = 'initial_conditions'
    if ( .not. Input%HasSection( SubSectionName=SectionName ) ) then
      call Error%Raise( Line='Mandatory section missing', ProcName=ProcName )
    else
      ParameterName = 'position'
      call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%IC(1) = VarR0D
      ParameterName = 'velocity'
      call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      This%IC(2) = VarR0D
    end if

    SectionName = 'abscissa'
    if ( .not. Input%HasSection( SubSectionName=SectionName ) ) then
      call Error%Raise( Line='Mandatory section missing', ProcName=ProcName )
    else
      ParameterName = 'source'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
      SubSectionName = SectionName // '>source'
      select case (VarC0D)
        case('internal')
          ParameterName = 'value'
          call Input%GetValue( Values=VarR1D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
          allocate(This%Abscissa, source=VarR1D, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Abscissa', ProcName=ProcName, stat=StatLoc )
          deallocate(VarR1D, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
        case('external')
          call Error%Raise( Line='External abscissa source functionality not implemented', ProcName=ProcName )
        case default
          call Error%Raise( Line='abscissa source not recognized', ProcName=ProcName )
      end select
    end if

    SectionName = 'parameters'

    ParameterName = 'm_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) This%M_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'm'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%M = VarR0D

    ParameterName = 'k_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) This%K_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'k'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%K = VarR0D

    ParameterName = 'c_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) This%C_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'c'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%C = VarR0D

    SectionName = 'solver'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      ParameterName = 'relative_tolerance'
      call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
      if ( Found ) This%RTOL = VarR0D
      ParameterName = 'absolute_tolerance'
      call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
      if ( Found ) This%ATOL = VarR0D
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase( This, CaseDir, Prefix )

    use String_Library

    class(SMD_Type), intent(inout)                                    ::    This
    character(*), intent(in)                                          ::    CaseDir
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FileName
    type(InputReader_Type)                                            ::    Input

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    FileName = PrefixLoc // CaseDir // '/input/input.dat'
    call Input%Read( FileName=FileName )

    call This%Construct( Input=Input, Prefix=PrefixLoc // CaseDir )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput

    class(SMD_Type), intent(in)                                       ::    This
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

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )
    
    call GetInput%AddParameter( Name='label', Value=This%Label )

    SectionName='parameters'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%AddParameter( Name='m', Value=ConvertToString(Value=This%M), SectionName=SectionName )
    call GetInput%AddParameter( Name='k', Value=ConvertToString(Value=This%K), SectionName=SectionName )
    call GetInput%AddParameter( Name='c', Value=ConvertToString(Value=This%C), SectionName=SectionName )
    if ( len_trim(This%M_Dependency) /= 0 ) call GetInput%AddParameter( Name='m_dependency', Value=This%M_Dependency,             &
                                                                                                         SectionName=SectionName )
    if ( len_trim(This%K_Dependency) /= 0 ) call GetInput%AddParameter( Name='k_dependency', Value=This%K_Dependency,             &
                                                                                                         SectionName=SectionName )
    if ( len_trim(This%C_Dependency) /= 0 ) call GetInput%AddParameter( Name='c_dependency', Value=This%C_Dependency,             &
                                                                                                         SectionName=SectionName )

    SectionName = 'initial_conditions'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%AddParameter( Name='position', Value=Convert_To_String(This%IC(1)), SectionName=SectionName )
    call GetInput%AddParameter( Name='velocity', Value=Convert_To_String(This%IC(2)), SectionName=SectionName )

    SectionName = 'abscissa'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%AddParameter( Name='source', Value='internal', SectionName=SectionName )
    SubSectionName = 'source'
    call GetInput%AddSection( SectionName=SubSectionName, To_SubSection=SectionName )
    SubSectionName = SectionName // '>' // SubSectionName
    call GetInput%AddParameter( Name='value', Value=ConvertToString(Values=This%Abscissa), SectionName=SubSectionName )

    SectionName = 'solver'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%AddParameter( Name='relative_tolerance', Value=Convert_To_String(This%RTOL), SectionName=SectionName )
    call GetInput%AddParameter( Name='absolute_tolerance', Value=Convert_To_String(This%ATOL), SectionName=SectionName )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RunCase1( This, Input, Output, Stat )

    class(SMD_Type), intent(inout)                                    ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), dimension(:), allocatable, intent(inout)       ::    Output
    integer, optional, intent(out)                                    ::    Stat

    character(*), parameter                                           ::    ProcName='ProcessInput'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                            ::    Ordinate
    real(rkp)                                                         ::    M
    real(rkp)                                                         ::    K
    real(rkp)                                                         ::    C
    real(rkp)                                                         ::    T
    real(rkp), dimension(2)                                           ::    Y
    integer                                                           ::    NEQ
    integer                                                           ::    IOPT
    integer                                                           ::    ITASK
    integer                                                           ::    MF
    integer                                                           ::    LIW
    integer                                                           ::    LRW
    integer                                                           ::    ITOL
    integer, allocatable, dimension(:)                                ::    IWORK                                             
    real(rkp), allocatable, dimension(:)                              ::    RWORK
    integer                                                           ::    NbRuns
    integer                                                           ::    ii_max
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    IState
    logical                                                           ::    Found
    type(InputDet_Type)                                               ::    InputDetLoc
    integer                                                           ::    RunStatLoc
    real(8)                                                           ::    TOUT
    real(8)                                                           ::    RTOL
    real(8)                                                           ::    ATOL

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( .not. allocated(Output) ) then
      allocate( Output(1), stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
    else
      if ( size(Output,1) /= 1 ) then
        deallocate(Output, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )
        allocate( Output(1), stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
      end if
    end if

    MF = 22
    IOPT = 0
    ITASK = 1
    NEQ = 2
    ITOL = 1

    LRW = 22 +  9*NEQ + NEQ**2 
    LIW = 20 + NEQ
    
    ATOL = This%ATOL
    RTOL = THIS%RTOL

    allocate(IWORK(LIW), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='IWORK', ProcName=ProcName, stat=StatLoc )
    IWORK=0

    allocate(RWORK(LRW), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='RWORK', ProcName=ProcName, stat=StatLoc )
    RWORK=0.d0

    ii_max = size(This%Abscissa,1)

    RunStatLoc = 0

    select type (Input)
      type is (InputDet_Type)
        allocate( Ordinate(ii_max, 1), stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
        Ordinate = Zero
        if ( len_trim(This%M_Dependency) /= 0 ) then
          call Input%GetValue( Value=M, Label=This%M_Dependency )
        else
          M = This%M
        end if
        if ( len_trim(This%C_Dependency) /= 0 ) then
          call Input%GetValue( Value=C, Label=This%C_Dependency )
        else
          C = This%C
        end if
        if ( len_trim(This%K_Dependency) /= 0 ) then
          call Input%GetValue( Value=K, Label=This%K_Dependency )
        else
          K = This%K
        end if

        T = This%Abscissa(1)
        Y = This%IC
        ISTATE = 1

        ii_max = size(This%Abscissa)
        do ii = 1, ii_max
          TOUT = This%Abscissa(ii)
          call DLSODE(ODE, 2, Y, T, TOUT, ITOL, RTOL, ATOL, ITASK, ISTATE, IOPT, RWORK, LRW, IWORK, LIW,   &
                      Dummy, MF)
          T = TOUT
          if (ISTATE .gt. 0) then  
            Ordinate(ii,1)=Y(1)
          else 
            RunStatLoc = -1
          end if
          if ( RunStatLoc /= 0 ) exit
        end do

      type is (InputStoch_Type)
        allocate(Ordinate(ii_max,Input%GetNbDegen()), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
        i = 1
        do i = 1, Input%GetNbDegen()
          InputDetLoc = Input%GetDetInput(Num=i)
          if ( len_trim(This%M_Dependency) /= 0 ) then
            call InputDetLoc%GetValue( Value=M, Label=This%M_Dependency )
          else
            M = This%M
          end if
          if ( len_trim(This%C_Dependency) /= 0 ) then
            call InputDetLoc%GetValue( Value=C, Label=This%C_Dependency )
          else
            C = This%C
          end if
          if ( len_trim(This%K_Dependency) /= 0 ) then
            call InputDetLoc%GetValue( Value=K, Label=This%K_Dependency )
          else
            K = This%K
          end if
          T = This%Abscissa(1)
          Y = This%IC
          ISTATE = 1

          do ii = 1, ii_max
            call DLSODE(ODE, 2, Y, T, This%Abscissa(ii), ITOL, This%RTOL, This%ATOL, ITASK, ISTATE, IOPT, RWORK, LRW, IWORK, LIW, &
                        Dummy, MF)
            if (ISTATE .gt. 0) then  
              Ordinate(ii,i)=Y(1)
            else 
              RunStatLoc = -1
            end if
            if ( RunStatLoc /= 0 ) exit
          end do
          if ( RunStatLoc /= 0 ) exit
        end do

      class default
        call Error%Raise( Line='Update input type definitions', ProcName=ProcName )
    end select

    call Output(1)%Construct( Values=Ordinate, Label=This%Label )

    deallocate(IWORK, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='IWORK', ProcName=ProcName, stat=StatLoc )

    deallocate(RWORK, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='RWORK', ProcName=ProcName, stat=StatLoc )

    deallocate(Ordinate, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )

    if ( present(Stat) ) Stat = RunStatLoc

    contains

      subroutine ODE(NEQ, TT, YY, YDOT)
        implicit none
        integer                   NEQ
        double precision          TT, YY(2), YDOT(2)
        YDOT(1) = + ((0.d0)* YY(1)) + ((1.d0)* YY(2))
        YDOT(2) = - ((K/M) * YY(1)) - ((C/M) * YY(2))
      end subroutine

      subroutine Dummy()
      end subroutine

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(SMD_Type), intent(out)                                      ::    LHS
    class(Model_Type), intent(in)                                     ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (SMD_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if( RHS%Constructed ) then
          LHS%Label = RHS%Label
          LHS%M = RHS%M
          LHS%K = RHS%K
          LHS%C = RHS%C
          LHS%M_Dependency = RHS%M_Dependency
          LHS%K_Dependency = RHS%K_Dependency
          LHS%C_Dependency = RHS%C_Dependency
          LHS%IC = RHS%IC
          LHS%RTOL = RHS%RTOL
          LHS%ATOL = RHS%ATOL
          allocate(LHS%Abscissa, source=RHS%Abscissa, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Abscissa', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
