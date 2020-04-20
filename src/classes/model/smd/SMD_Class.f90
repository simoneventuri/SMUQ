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
use ModelInternal_Class                                           ,only:    ModelInternal_Type
use Output_Class                                                  ,only:    Output_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use InputProcessor_Class                                          ,only:    InputProcessor_Type

implicit none

private

public                                                                ::    SMD_Type
public                                                                ::    SMD

type, extends(ModelInternal_Type)                                     ::    SMD_Type
  character(:), allocatable                                           ::    OutputLabel
  real(rkp)                                                           ::    X0
  real(rkp)                                                           ::    Xdot0
  real(rkp), allocatable, dimension(:)                                ::    Abscissa
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
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    RunInternal
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(SMD_Type), intent(inout)                                    ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'smd'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(SMD_Type), intent(inout)                                    ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    if (allocated(This%Abscissa)) deallocate(This%Abscissa, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Abscissa', ProcName=ProcName, stat=StatLoc)

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(SMD_Type), intent(inout)                                    ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%Label = 'smd'
    This%OutputLabel = 'smd'
    This%X0 = Zero
    This%Xdot0 = Zero
    This%M = 1.0
    This%K = 1.1
    This%C = 1.2
    This%M_Dependency = ''
    This%K_Dependency = ''
    This%C_Dependency = ''
    This%NbOutputs = 1
    This%Silent = .false.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(SMD_Type), intent(inout)                                    ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
    logical                                                           ::    MandatoryLoc
    logical                                                           ::    Found

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'label'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
    This%Label = VarC0D

    ParameterName = 'output_label'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
    This%OutputLabel = VarC0D

    ParameterName = 'silent'
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%Silent = VarL0D

    SectionName = 'initial_conditions'
    if (.not. Input%HasSection(SubSectionName=SectionName)) then
      call Error%Raise(Line='Mandatory initial conditions section missing', ProcName=ProcName)
    else
      ParameterName = 'position'
      call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
      This%X0 = VarR0D
      ParameterName = 'velocity'
      call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
      This%Xdot0 = VarR0D
    end if

    SectionName = 'abscissa'
    if (.not. Input%HasSection(SubSectionName=SectionName)) then
      call Error%Raise(Line='Mandatory section missing', ProcName=ProcName)
    else
      ParameterName = 'source'
      call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true.)
      SubSectionName = SectionName // '>source'
      select case (VarC0D)
        case('internal')
          ParameterName = 'value'
          call Input%GetValue(Values=VarR1D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true.)
          allocate(This%Abscissa, source=VarR1D, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='This%Abscissa', ProcName=ProcName, stat=StatLoc)
          deallocate(VarR1D, stat=StatLoc)
          if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
        case('external')
          call Error%Raise(Line='External abscissa source functionality not implemented', ProcName=ProcName)
        case default
          call Error%Raise(Line='abscissa source not recognized', ProcName=ProcName)
      end select
    end if

    SectionName = 'parameters'

    ParameterName = 'm_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found)
    if (Found) This%M_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'm'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found)
    if (Found) This%M = VarR0D

    ParameterName = 'k_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found)
    if (Found) This%K_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'k'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found)
    if (Found) This%K = VarR0D

    ParameterName = 'c_dependency'
    call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found)
    if (Found) This%C_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'c'
    call Input%GetValue(VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found)
    if (Found) This%C = VarR0D

    SectionName = 'input_preprocessor'
    if (Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call This%InputProcessor%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(SMD_Type), intent(in)                                       ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%SetName(SectionName = trim(adjustl(Name)))
    
    call GetInput%AddParameter(Name='label', Value=This%Label)
    call GetInput%AddParameter(Name='output_label', Value=This%Label)

    SectionName='parameters'
    call GetInput%AddSection(SectionName=SectionName)
    call GetInput%AddParameter(Name='m', Value=ConvertToString(Value=This%M), SectionName=SectionName)
    call GetInput%AddParameter(Name='k', Value=ConvertToString(Value=This%K), SectionName=SectionName)
    call GetInput%AddParameter(Name='c', Value=ConvertToString(Value=This%C), SectionName=SectionName)
    if (len_trim(This%M_Dependency) /= 0) call GetInput%AddParameter(Name='m_dependency', Value=This%M_Dependency,             &
                                                                                                         SectionName=SectionName)
    if (len_trim(This%K_Dependency) /= 0) call GetInput%AddParameter(Name='k_dependency', Value=This%K_Dependency,             &
                                                                                                         SectionName=SectionName)
    if (len_trim(This%C_Dependency) /= 0) call GetInput%AddParameter(Name='c_dependency', Value=This%C_Dependency,             &
                                                                                                         SectionName=SectionName)

    SectionName = 'initial_conditions'
    call GetInput%AddSection(SectionName=SectionName)
    call GetInput%AddParameter(Name='position', Value=Convert_To_String(This%X0), SectionName=SectionName)
    call GetInput%AddParameter(Name='velocity', Value=Convert_To_String(This%Xdot0), SectionName=SectionName)

    SectionName = 'abscissa'
    call GetInput%AddSection(SectionName=SectionName)
    call GetInput%AddParameter(Name='source', Value='internal', SectionName=SectionName)
    SubSectionName = 'source'
    call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
    SubSectionName = SectionName // '>' // SubSectionName
    call GetInput%AddParameter(Name='value', Value=ConvertToString(Values=This%Abscissa), SectionName=SubSectionName)

    if (This%InputProcessor%IsConstructed()) call GetInput%AddSection(Section=This%InputProcessor%GetInput(                  &
                                                 Name='input_preprocessor', Prefix=PrefixLoc, Directory=DirectorySub))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RunInternal(This, Input, Output, Stat)

    class(SMD_Type), intent(inout)                                    ::    This
    type(Input_Type), intent(in)                                      ::    Input
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    integer, optional, intent(out)                                    ::    Stat

    character(*), parameter                                           ::    ProcName='RunInternal'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                            ::    Ordinate
    real(rkp)                                                         ::    M
    real(rkp)                                                         ::    K
    real(rkp)                                                         ::    C
    logical                                                           ::    Found

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    if (size(Output,1) /= This%NbOutputs) call Error%Raise('Passed down an output array of incorrect length',                  &
                                                                                                               ProcName=ProcName)

    allocate(Ordinate(size(This%Abscissa,1), 1), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='Ordinate', ProcName=ProcName, stat=StatLoc)
    Ordinate = Zero

    if (len_trim(This%M_Dependency) /= 0) then
      call Input%GetValue(Value=M, Label=This%M_Dependency)
    else
      M = This%M
    end if
    if (len_trim(This%C_Dependency) /= 0) then
      call Input%GetValue(Value=C, Label=This%C_Dependency)
    else
      C = This%C
    end if
    if (len_trim(This%K_Dependency) /= 0) then
      call Input%GetValue(Value=K, Label=This%K_Dependency)
    else
      K = This%K
    end if

    call SMD(M=M, C=C, K=K, X0=This%X0, Xdot0=This%Xdot0, Abscissa=This%Abscissa, Response=Ordinate(:,1))

    call Output(1)%Construct(Values=Ordinate, Label=This%OutputLabel)

    if (present(Stat)) Stat = 0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(SMD_Type), intent(out)                                      ::    LHS
    class(Model_Type), intent(in)                                     ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (SMD_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if(RHS%Constructed) then
          LHS%Label = RHS%Label
          LHS%OutputLabel = RHS%OutputLabel
          LHS%M = RHS%M
          LHS%K = RHS%K
          LHS%C = RHS%C
          LHS%M_Dependency = RHS%M_Dependency
          LHS%K_Dependency = RHS%K_Dependency
          LHS%C_Dependency = RHS%C_Dependency
          LHS%X0 = RHS%X0
          LHS%Xdot0 = RHS%Xdot0
          allocate(LHS%Abscissa, source=RHS%Abscissa, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%Abscissa', ProcName=ProcName, stat=StatLoc)
          LHS%NbOutputs = RHS%NbOutputs
          LHS%Label = RHS%Label
          LHS%InputProcessor = RHS%InputProcessor
        end if

      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(SMD_Type), intent(inout)                                     ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%Abscissa)) deallocate(This%Abscissa, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Abscissa', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SMD(M, C, K, X0, Xdot0, Abscissa, Response)

    real(rkp), intent(in)                                             ::    M
    real(rkp), intent(in)                                             ::    C
    real(rkp), intent(in)                                             ::    K
    real(rkp), intent(in)                                             ::    X0
    real(rkp), intent(in)                                             ::    Xdot0
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    real(rkp), dimension(:), intent(inout)                            ::    Response

    character(*), parameter                                           ::    ProcName='Run_0D'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    QuadraticEQ
    real(rkp)                                                         ::    C1
    real(rkp)                                                         ::    C2
    real(rkp)                                                         ::    R1
    real(rkp)                                                         ::    R2
    real(rkp)                                                         ::    Mu
    real(rkp)                                                         ::    Lambda

    if (size(Response,1) /= size(Abscissa,1)) call Error%Raise('Mismatch in length of abscissa and response arrays',           &
                                                                                                               ProcName=ProcName)    

    QuadraticEQ = C**2 - Four*M*K

    if (QuadraticEQ > Zero) then
      R1 = (- C + dsqrt(QuadraticEQ)) / (Two*M)
      R2 = (- C - dsqrt(QuadraticEQ)) / (Two*M)
      C2 = (Xdot0 - X0*R1) / (R2-R1)
      C1 = X0 - C2
      Response = C1*dexp(R1*Abscissa) + C2*dexp(R2*Abscissa)
    elseif (QuadraticEQ < Zero) then
      Lambda = -C/(Two*M)
      Mu = dsqrt(-QuadraticEQ) / (Two*M)
      C1 = X0
      C2 = (Xdot0-Lambda*C1) / Mu
      Response = dexp(Lambda*Abscissa) * (C1*dcos(Mu*Abscissa)+C2*dsin(Mu*Abscissa))
    else
      R1 = -C/(Two*M)
      R2 = R1
      C1 = X0
      C2 = Xdot0 - R1*C1
      Response = C1*dexp(R1*Abscissa) + C2*Abscissa*dexp(R1*Abscissa)
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
