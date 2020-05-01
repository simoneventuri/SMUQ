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

module ModelTransform_class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Model_Class                                                   ,only:    Model_Type
use Model_Factory_Class                                           ,only:    Model_Factory
use TransfSampleSpace_Class                                       ,only:    TransfSampleSpace_Type
use TransfSampleSpace_Factory_Class                               ,only:    TransfSampleSpace_Factory
use Output_Class                                                  ,only:    Output_Type
use Input_Class                                                   ,only:    Input_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    ModelTransform_Type

type, extends(Model_Type)                                             ::    ModelTransform_Type
  class(Model_Type), allocatable                                      ::    Model
  class(TransfSampleSpace_Type), allocatable                          ::    SpaceTransform
  type(SMUQString_Type), allocatable, dimension(:)                    ::    InputLabels
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, public                                                   ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run_0D
  procedure, public                                                   ::    Run_1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(ModelTransform_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    if (.not. This%Initialized) then
      This%Name = 'modeltransform'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(ModelTransform_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0
    if (allocated(This%SpaceTransform)) deallocate(This%SpaceTransform, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%SpaceTransform', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Model)) deallocate(This%Model, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Model', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%InputLabels)) deallocate(This%InputLabels, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%InputLabels', ProcName=ProcName, stat=StatLoc)

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(ModelTransform_Type), intent(inout)                         ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(ModelTransform_Type), intent(inout)                         ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    SectionName = 'model'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call Model_Factory%Construct(Object=This%Model, Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)

    This%Label = This%Model%GetLabel()
    This%NbOutputs = This%Model%GetNbOutputs()

    SectionName = 'transformed_space'
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call TransfSampleSpace_Factory%Construct(Object=This%SpaceTransform, Input=InputSection, Prefix=PrefixLoc)
    nullify(InputSection)

    allocate(This%InputLabels(This%SpaceTransform%GetNbDim()), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%InputLabels', ProcName=ProcName, stat=StatLoc)
    This%InputLabels = This%SpaceTransform%GetLabel()

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1(This, SpaceTransform, Model)

    class(ModelTransform_Type), intent(inout)                         ::    This
    class(TransfSampleSpace_Type), intent(in)                         ::    SpaceTransform
    class(Model_Type), intent(in)                                     ::    Model

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0
    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    allocate(This%SpaceTransform, source=SpaceTransform, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%SpaceTransform', ProcName=ProcName, stat=StatLoc)

    allocate(This%Model, source=Model, stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%Model', ProcName=ProcName, stat=StatLoc)

    allocate(This%InputLabels(This%SpaceTransform%GetNbDim()), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='This%InputLabels', ProcName=ProcName, stat=StatLoc)
    This%InputLabels = This%SpaceTransform%GetLabel()

    This%NbOutputs = This%Model%GetNbOutputs()
    This%Label = This%Model%GetLabel()

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(ModelTransform_Type), intent(in)                            ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    DirectoryLoc = ''
    PrefixLoc = ''
    if (present(Directory)) DirectoryLoc = Directory
    if (present(Prefix)) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    if (ExternalFlag) DirectorySub = DirectoryLoc // '/model'
    call GetInput%AddSection(Section=Model_Factory%GetObjectInput(Object=This%Model, Name='model',                   &
                                                                                      Prefix=PrefixLoc, Directory=DirectorySub))

    if (ExternalFlag) DirectorySub = DirectoryLoc // '/transformed_space'
    call GetInput%AddSection(Section=TransfSampleSpace_Factory%GetObjectInput(Object=This%SpaceTransform,                       &
                                                 Name='transformed_space', Prefix=PrefixLoc, Directory=DirectorySub))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_0D(This, Input, Output, Stat)

    class(ModelTransform_Type), intent(inout)                         ::    This
    type(Input_Type), intent(in)                                      ::    Input
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    integer, optional, intent(out)                                    ::    Stat

    character(*), parameter                                           ::    ProcName='Run_0D'
    integer                                                           ::    StatLoc=0
    logical                                                           ::    ExternalFlag=.false.
    type(Input_Type)                                                  ::    InputLoc
    real(rkp), allocatable, dimension(:)                              ::    VarR1D

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    call Input%GetValue(Values=VarR1D, Labels=This%InputLabels)
    call InputLoc%Construct(Input=This%SpaceTransform%InvTransform(Z=VarR1D), Labels=This%InputLabels)
    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
   
    call This%Model%Run(Input=InputLoc, Output=Output, Stat=Stat)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_1D(This, Input, Output, Stat)

    class(ModelTransform_Type), intent(inout)                         ::    This
    type(Input_Type), dimension(:), intent(in)                        ::    Input
    type(Output_Type), dimension(:,:), intent(inout)                  ::    Output
    integer, dimension(:), optional, intent(inout)                    ::    Stat

    character(*), parameter                                           ::    ProcName='Run_1D'
    integer                                                           ::    StatLoc=0
    logical                                                           ::    ExternalFlag=.false.
    type(Input_Type), allocatable, dimension(:)                       ::    InputLoc
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer                                                           ::    NbInputs
    integer                                                           ::    i

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    NbInputs = size(Input,1)

    allocate(InputLoc(NbInputs), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='InputLoc', ProcName=ProcName, stat=StatLoc)

    i = 1
    do i = 1, NbInputs
      call Input(i)%GetValue(Values=VarR1D, Labels=This%InputLabels)
      call InputLoc(i)%Construct(Input=This%SpaceTransform%InvTransform(Z=VarR1D), Labels=This%InputLabels)
    end do

    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

    call This%Model%Run(Input=InputLoc, Output=Output, Stat=Stat)

    deallocate(InputLoc, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='InputLoc', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(ModelTransform_Type), intent(out)                           ::    LHS
    class(Model_Type), intent(in)                            ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)
      type is (ModelTransform_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if (RHS%Constructed) then
          allocate(LHS%SpaceTransform, source=RHS%SpaceTransform, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%SpaceTransform', ProcName=ProcName, stat=StatLoc)
          allocate(LHS%Model, source=RHS%Model, stat=StatLoc)
          if (StatLoc /= 0) call Error%Allocate(Name='LHS%Model', ProcName=ProcName, stat=StatLoc)
        end if
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(ModelTransform_Type),intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%SpaceTransform)) deallocate(This%SpaceTransform, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%SpaceTransform', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%Model)) deallocate(This%Model, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Model', ProcName=ProcName, stat=StatLoc)

    if (allocated(This%InputLabels)) deallocate(This%InputLabels, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%InputLabels', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
