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

module RadialDesign_Class

use Parameters_Library
use Input_Library
use ArrayIORoutines_Module
use ArrayRoutines_Module
use ComputingRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use RandPseudo_Class                                              ,only:    RandPseudo_Type
use LinkedList0D_Class                                            ,only:    LinkedList0D_Type
use SampleMethod_Class                                            ,only:    SampleMethod_Type
use SampleMethod_Factory_Class                                    ,only:    SampleMethod_Factory
use SampleLHS_Class                                               ,only:    SampleLHS_Type
use SampleQuasiMC_Class                                           ,only:    SampleQuasiMC_Type
use RandPseudo_Class                                              ,only:    RandPseudo_Type

implicit none

private

public                                                                ::    RadialDesign_Type

type                                                                  ::    RadialDesign_Type
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  character(:), allocatable                                           ::    Name
  real(rkp)                                                           ::    Eps
  class(SampleMethod_Type), allocatable                               ::    Sampler
  integer                                                             ::    AuxilaryShift
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Draw
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(RadialDesign_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Initialize'
    integer(8)                                                        ::    SysTimeCount

    if (.not. This%Initialized) then
      This%Initialized = .true.
      This%Name = 'radialdesign'
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(RadialDesign_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized=.false.
    This%Constructed=.false.

    if (allocated(This%Sampler)) deallocate(This%Sampler, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Sampler', ProcName=ProcName, stat=StatLoc)

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(RadialDesign_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%AuxilaryShift = 4
    This%Eps = 1.0d-10

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput (This, Input, Prefix)

    use StringConversion_Module

    class(RadialDesign_Type), intent(inout)                           ::    This
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
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'epsilon'
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%Eps = VarR0D
    if (This%Eps < 0) call Error%Raise('Epsilon must be non-negative', ProcName=ProcName)

    ParameterName = 'auxilary_shift'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%AuxilaryShift = VarI0D
    if (This%AuxilaryShift < 0) call Error%Raise('Auxilary shift must be a non-negative integer', ProcName=ProcName)

    SectionName = 'sampler'
    if (Input%HasSection(SubSectionName=SectionName)) then
      call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
      call SampleMethod_Factory%Construct(Object=This%Sampler, Input=InputSection, Prefix=PrefixLoc)
    else
      allocate(SampleQuasiMC_Type :: This%Sampler)
      select type (Object => This%Sampler)
        type is (SampleQuasiMC_Type)
          call Object%Construct()
        class default
          call Error%Raise(Line='Something went wrong', ProcName=ProcName)
      end select
    end if

    This%Constructed = .true.

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1 (This, Sampler, AuxilaryShift, Eps)

    class(RadialDesign_Type), intent(inout)                           ::    This
    class(SampleMethod_Type), optional, intent(in)                    ::    Sampler
    integer, optional, intent(in)                                     ::    AuxilaryShift
    real(rkp), optional, intent(in)                                   ::    Eps

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    if (present(Eps)) This%Eps = Eps
    if (This%Eps < 0) call Error%Raise('Epsilon must be non-negative', ProcName=ProcName)

    if (present(AuxilaryShift)) This%AuxilaryShift = AuxilaryShift
    if (This%AuxilaryShift < 0) call Error%Raise('Auxilary shift must be a non-negative integer', ProcName=ProcName)

    if (present(Sampler)) then
      allocate(This%Sampler, source=Sampler, stat=StatLoc)
      if (StatLoc /= 0) call Error%Allocate(Name='This%Sampler', ProcName=ProcName, stat=StatLoc)
    else
      allocate(SampleQuasiMC_Type :: This%Sampler)
      select type (Object => This%Sampler)
        type is (SampleQuasiMC_Type)
          call Object%Construct()
        class default
          call Error%Raise(Line='Something went wrong', ProcName=ProcName)
      end select
    end if

    This%Constructed = .true.

  end subroutine 
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    use CommandRoutines_Module
    use StringConversion_Module

    type(InputSection_Type)                                           ::    GetInput
    class(RadialDesign_Type), intent(in)                              ::    This
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

    call GetInput%AddParameter(Name='epsilon', Value=ConvertToString(Value=This%Eps))
    call GetInput%AddParameter(Name='auxilary_shift', Value=ConvertToString(Value=This%AuxilaryShift))

    if (ExternalFlag) DirectorySub = DirectoryLoc // '/sampler'
    call GetInput%AddSection(Section=SampleMethod_Factory%GetObjectInput(Object=This%Sampler, Name='sampler',        &
                                                                                      Prefix=PrefixLoc, Directory=DirectorySub))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Draw(This, NbDim, NbBlocks, Blocks, StepSize)

    class(RadialDesign_Type), intent(inout)                           ::    This
    integer, intent(in)                                               ::    NbBlocks
    integer, intent(in)                                               ::    NbDim
    real(rkp), dimension(:,:), intent(inout)                          ::    Blocks
    real(rkp), dimension(:,:), optional, intent(inout)                ::    StepSize
    
    character(*), parameter                                           ::    ProcName='Draw'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    NbEntries
    integer                                                           ::    NbDimP1
    integer                                                           ::    NbDimTwo
    real(rkp), allocatable, dimension(:,:)                            ::    XStar
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    integer                                                           ::    iv
    integer                                                           ::    iOffset
    integer                                                           ::    iBlockMin
    real(rkp), allocatable, dimension(:)                              ::    VarR1D

    if (.not. This%Constructed) call Error%Raise(Line='The object was never constructed', ProcName=ProcName)

    NbDimP1 = NbDim+1
    NbDimTwo = NbDim*2
    NbEntries = NbDimP1*NbBlocks

    allocate(VarR1D(NbDim), stat=StatLoc)
    if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
    VarR1D = Zero

    if (present(StepSize)) then
      if (size(StepSize,1) /= NbDim .or. size(StepSize,2) /= NbBlocks) call Error%Raise('Passed step size array of ' //        &
                                                                                             'incorrect size', ProcName=ProcName)
    end if

    if (size(Blocks,2) /= NbEntries .or. size(Blocks,1) /= NbDim) call Error%Raise('Passed blocks ' //                         &
                                                                                    'array of incorrect size', ProcName=ProcName)

    Blocks = Zero
    call This%Sampler%Draw(NbSamples=NbBlocks+This%AuxilaryShift, NbDim=NbDimTwo, Samples=XStar)

    ! generating lager sample size if auxillary points do not differ from non-auxillary ones in any dimension
    do
      iOffset = 0
      iv = 0
      i = 1
      do i = 1, NbBlocks
        ii = 1
        iii = 0
        do ii = This%AuxilaryShift+iOffset+i, size(XStar,2)
          VarR1D = dabs(XStar(1:NbDim,i)-XStar(NbDimP1:NbDimTwo,ii))
          if (all(VarR1D>This%Eps)) then
            iv = iv + 1
            exit
          end if
          iii = iii + 1
        end do
        iOffset = iOffset + iii
        if (This%AuxilaryShift+iOffset+i+1 > size(XStar,2)) exit
      end do

      if (iv == NbBlocks) exit
      deallocate(XStar, stat=StatLoc)
      if (StatLoc /= 0) call Error%Deallocate(Name='XStar', ProcName=ProcName, stat=StatLoc)
      call This%Sampler%Draw(NbSamples=NbBlocks+This%AuxilaryShift+iOffset, NbDim=NbDimTwo, Samples=XStar)
    end do

    iOffset = 0

    i = 1
    do i = 1, NbBlocks
      iBlockMin = (i-1)*NbDimP1+1
      Blocks(:,iBlockMin) = XStar(1:NbDim,i)
      do
        VarR1D = dabs(XStar(1:NbDim,i)-XStar(NbDimP1:NbDimTwo,i+This%AuxilaryShift+iOffset))
        if (all(VarR1D>This%Eps)) exit
        iOffset = iOffset + 1
      end do
      ii = 1
      do ii = 1, NbDim
        Blocks(:,iBlockMin+ii) = Blocks(:,iBlockMin)
        Blocks(ii,iBlockMin+ii) = XStar(NbDim+ii,i+This%AuxilaryShift+iOffset)
        if (present(StepSize)) StepSize(ii,i) = Blocks(ii,iBlockMin+ii) - Blocks(ii,iBlockMin)
      end do
    end do

    deallocate(XStar, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='XStar', ProcName=ProcName, stat=StatLoc)

    deallocate(VarR1D, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(RadialDesign_Type), intent(out)                             ::    LHS
    class(RadialDesign_Type), intent(in)                              ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    call LHS%Reset()
    LHS%Initialized = RHS%Initialized
    LHS%Constructed = RHS%Constructed

    if (RHS%Constructed) then
      LHS%Sampler = RHS%Sampler
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(RadialDesign_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
