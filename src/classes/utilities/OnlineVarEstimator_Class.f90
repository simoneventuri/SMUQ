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

! Welfords online algorithm by updating the sum of mean residuals squared
module OnlineVarEstimator_Class

use Input_Library
use Parameters_Library
use StringConversion_Module
use CommandROutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    OnlineVarEstimator_Type

type                                                                  ::    OnlineVarEstimator_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbSamples
  real(rkp)                                                           ::    M2
  real(rkp)                                                           ::    Mean
  logical                                                             ::    SampleVariance
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  generic, public                                                     ::    Update                  =>    Update_0D,              &
                                                                                                          Update_1D
  procedure, public                                                   ::    Update_0D
  procedure, public                                                   ::    Update_1D
  procedure, public                                                   ::    GetMean
  procedure, public                                                   ::    GetVariance
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize(This)

    class(OnlineVarEstimator_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if (.not. This%Initialized) then
      This%Name = 'OnlineVarEstimator'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset(This)

    class(OnlineVarEstimator_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized=.false.
    This%Constructed=.false.

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults(This)

    class(OnlineVarEstimator_Type), intent(inout)                     ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%SampleVariance = .true.
    This%NbSamples = 0
    This%Mean = Zero
    This%M2 = Zero

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput(This, Input, Prefix)

    class(OnlineVarEstimator_Type), intent(inout)                     ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    PrefixLoc = ''
    if (present(Prefix)) PrefixLoc = Prefix

    ParameterName = 'nb_samples'
    call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
    This%NbSamples = VarI0D

    ParameterName = 'mean'
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.true.)
    This%Mean = VarR0D

    ParameterName = 'm2'
    call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.true.)
    This%M2 = VarR0D

    ParameterName = 'sample_variance'
    call Input%GetValue(Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found)
    if (Found) This%SampleVariance = VarL0D

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1(This, SampleVariance)

    class(OnlineVarEstimator_Type), intent(inout)                     ::    This
    logical, optional, intent(in)                                     ::    SampleVariance

    character(*), parameter                                           ::    ProcName='ConstructCase1'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0

    if (This%Constructed) call This%Reset()
    if (.not. This%Initialized) call This%Initialize()

    if (present(SampleVariance)) This%SampleVariance = SampleVariance

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput(This, Name, Prefix, Directory)

    type(InputSection_Type)                                           ::    GetInput

    class(OnlineVarEstimator_Type), intent(inout)                     ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
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

    if (ExternalFlag) call MakeDirectory(Path=PrefixLoc // DirectoryLoc, Options='-p')

    call GetInput%SetName(SectionName = trim(adjustl(Name)))

    call GetInput%AddParameter(Name='nb_samples', Value=ConvertToString(Value=This%NbSamples))
    call GetInput%AddParameter(Name='mean', Value=ConvertToString(Value=This%Mean))
    call GetInput%AddParameter(Name='m2', Value=ConvertToString(Value=This%M2))
    call GetInput%AddParameter(Name='sample_variance', Value=ConvertToString(Value=This%SampleVariance))

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Update_0D(This, Value)

    class(OnlineVarEstimator_Type), intent(inout)                     ::    This
    real(rkp), intent(in)                                             ::    Value

    character(*), parameter                                           ::    ProcName='Update_0D'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    del1
    real(rkp)                                                         ::    del2

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    This%NbSamples = This%NbSamples + 1
    del1 = Value - This%Mean
    This%Mean = This%Mean + del1/This%NbSamples
    del2 = Value - This%Mean
    This%M2 = This%M2 + del1 * del2

  end Subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Update_1D(This, Values)

    class(OnlineVarEstimator_Type), intent(inout)                     ::    This
    real(rkp), dimension(:), intent(in)                               ::    Values

    character(*), parameter                                           ::    ProcName='Update_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    i = 1
    do i = 1, size(Values,1)
      call This%Update(Value=Values(i))
    end do

  end Subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMean(This)

    real(rkp)                                                         ::    GetMean

    class(OnlineVarEstimator_Type), intent(in)                        ::    This

    character(*), parameter                                           ::    ProcName='GetMean'
    integer                                                           ::    StatLoc=0

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    GetMean = This%Mean

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetVariance(This, SampleVariance)

    real(rkp)                                                         ::    GetVariance

    class(OnlineVarEstimator_Type), intent(in)                        ::    This
    logical, optional, intent(in)                                     ::    SampleVariance

    character(*), parameter                                           ::    ProcName='GetVariance'
    integer                                                           ::    StatLoc=0
    logical                                                           ::    SampleVarianceLoc

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    SampleVarianceLoc = This%SampleVariance
    if (present(SampleVariance)) SampleVarianceLoc = SampleVariance

    if (This%NbSamples >= 2) then
      if (SampleVarianceLoc) then
        GetVariance = This%M2 / real(This%NbSamples-1,rkp)
      else
        GetVariance = This%M2 / real(This%NbSamples,rkp)
      end if
    else
      GetVariance = Zero
    end if

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy(LHS, RHS)

    class(OnlineVarEstimator_Type), intent(out)                       ::    LHS
    class(OnlineVarEstimator_Type), intent(in)                        ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (OnlineVarEstimator_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if (RHS%Constructed) then
          LHS%NbSamples = RHS%NbSamples
          LHS%M2 = RHS%M2
          LHS%Mean = RHS%Mean
          LHS%SampleVariance = RHS%SampleVariance
        end if
      
      class default
        call Error%Raise(Line='Incompatible types', ProcName=ProcName)

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer(This)

    type(OnlineVarEstimator_Type), intent(inout)                      ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if (allocated(This%Name)) deallocate(This%Name, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='This%Name', ProcName=ProcName, stat=StatLoc)

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
