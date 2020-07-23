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
module OnlineMeanEstimator_Class

use Input_Library
use Parameters_Library
use StringConversion_Module
use CommandROutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    OnlineMeanEstimator_Type

type                                                                  ::    OnlineMeanEstimator_Type
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbSamples
  real(rkp)                                                           ::    Mean
contains
  procedure, public                                                   ::    Reset
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
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(OnlineMeanEstimator_Type), intent(inout)                      ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc = 0

  This%Constructed=.false.

  This%NbSamples = 0
  This%Mean = Zero

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(OnlineMeanEstimator_Type), intent(inout)                      ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  real(rkp)                                                           ::    VarR0D
  integer                                                             ::    VarI0D
  logical                                                             ::    VarL0D
  character(:), allocatable                                           ::    ParameterName
  logical                                                             ::    Found
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  type(InputVerifier_Type)                                              ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  ParameterName = 'nb_samples'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarI0D, ParameterName=ParameterName, Mandatory=.true.)
  This%NbSamples = VarI0D

  ParameterName = 'mean'
  call InputVerifier%AddParameter(Parameter=ParameterName)
  call Input%GetValue(Value=VarR0D, ParameterName=ParameterName, Mandatory=.true.)
  This%Mean = VarR0D

  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructCase1(This)

  class(OnlineMeanEstimator_Type), intent(inout)                      ::    This

  character(*), parameter                                             ::    ProcName='ConstructCase1'
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  logical                                                             ::    Found
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0

  call This%Reset()

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(OnlineMeanEstimator_Type), intent(inout)                       ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.

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

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Update_0D(This, Value)

  class(OnlineMeanEstimator_Type), intent(inout)                      ::    This
  real(rkp), intent(in)                                               ::    Value

  character(*), parameter                                             ::    ProcName='Update_0D'
  integer                                                             ::    StatLoc=0
  real(rkp)                                                           ::    del1

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  This%NbSamples = This%NbSamples + 1
  del1 = Value - This%Mean
  This%Mean = This%Mean + del1/This%NbSamples

end Subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Update_1D(This, Values)

  class(OnlineMeanEstimator_Type), intent(inout)                      ::    This
  real(rkp), dimension(:), intent(in)                                 ::    Values

  character(*), parameter                                             ::    ProcName='Update_1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  i = 1
  do i = 1, size(Values,1)
    call This%Update(Value=Values(i))
  end do

end Subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetMean(This)

  real(rkp)                                                           ::    GetMean

  class(OnlineMeanEstimator_Type), intent(in)                          ::    This

  character(*), parameter                                             ::    ProcName='GetMean'
  integer                                                             ::    StatLoc=0

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  GetMean = This%Mean

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(OnlineMeanEstimator_Type), intent(out)                        ::    LHS
  class(OnlineMeanEstimator_Type), intent(in)                         ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (OnlineMeanEstimator_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%NbSamples = RHS%NbSamples
        LHS%Mean = RHS%Mean
      end if
    
    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(OnlineMeanEstimator_Type), intent(inout)                       ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
