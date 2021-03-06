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

module LikelihoodProduct_Class

use Input_Library
use Parameters_Library
use ArrayRoutines_Module
use StringConversion_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use Response_Class                                                ,only:    Response_Type
use Output_Class                                                  ,only:    Output_Type
use LikelihoodFunction_Class                                      ,only:    LikelihoodFunction_Type
use LikelihoodFunctionContainer_Class                             ,only:    LikelihoodFunctionContainer_Type
use LikelihoodFunction_Factory_Class                              ,only:    LikelihoodFunction_Factory
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    LikelihoodProduct_Type

type, extends(LikelihoodFunction_Type)                                ::    LikelihoodProduct_Type
  type(LikelihoodFunctionContainer_Type), allocatable, dimension(:)   ::    Likelihoods
  integer                                                             ::    NbLikelihoods
contains
  procedure, public                                                   ::    Reset
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    Evaluate_0D
  procedure, private                                                  ::    Evaluate_1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Reset(This)

  class(LikelihoodProduct_Type), intent(inout)                        ::    This

  character(*), parameter                                             ::    ProcName='Reset'
  integer                                                             ::    StatLoc = 0

  This%Constructed=.false.

  if (allocated(This%Likelihoods)) deallocate(This%Likelihoods, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Likelihoods', ProcName=ProcName, stat=StatLoc)
  This%NbLikelihoods = 0

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine ConstructInput(This, Input, Prefix)

  class(LikelihoodProduct_Type), intent(inout)                        ::    This
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='ConstructInput'
  integer                                                             ::    StatLoc=0
  logical                                                             ::    VarL0D
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    PrefixLoc
  logical                                                             ::    Found
  real(rkp)                                                           ::    VarR0D
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    i
  character(:), allocatable                                           ::    SectionName
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  class(LikelihoodFunction_Type), allocatable                         ::    LikelihoodFunction
  type(InputVerifier_Type)                                            ::    InputVerifier

  call This%Reset()

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()

  This%NbLikelihoods = Input%GetNumberofSubSections()

  allocate(This%Likelihoods(This%NbLikelihoods), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='This%Likelihoods', ProcName=ProcName, stat=StatLoc)

  i = 1
  do i = 1, This%NbLikelihoods
    SectionName = 'likelihood' // ConvertToString(Value=i)
    call InputVerifier%AddSection(Section=SectionName)
    call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
    call LikelihoodFunction_Factory%Construct(Object=LikelihoodFunction, Input=InputSection, Prefix=PrefixLoc)
    call This%Likelihoods(i)%Set(Object=LikelihoodFunction)
    deallocate(LikelihoodFunction, stat=StatLoc)
    if (StatLoc /= 0) call Error%Deallocate(Name='LikelihoodFunction', ProcName=ProcName, stat=StatLoc)
  end do

  This%Constructed = .true.

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetInput(This, Name, Prefix, Directory)

  type(InputSection_Type)                                             ::    GetInput

  class(LikelihoodProduct_Type), intent(inout)                        ::    This
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetInput'
  character(:), allocatable                                           ::    PrefixLoc
  integer                                                             ::    StatLoc=0
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  character(:), allocatable                                           ::    SubSectionName
  character(:), allocatable                                           ::    SectionName
  integer                                                             ::    i
  class(LikelihoodFunction_Type), pointer                             ::    LFunctionPtr=>null()

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  DirectoryLoc = ''
  PrefixLoc = ''
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix
  DirectorySub = DirectoryLoc

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetInput%SetName(SectionName = trim(adjustl(Name)))

  i = 1
  do i = 1, This%NbLikelihoods
    SectionName = 'likelihood' // ConvertToString(Value=i)
    if (ExternalFlag) DirectorySub = DirectoryLoc // 'likelihood' // ConvertToString(Value=i) // '/'
    LFunctionPtr => This%Likelihoods(i)%GetPointer()
    call GetInput%AddSection(Section=LikelihoodFunction_Factory%GetObjectInput(Object=LFunctionPtr,                           &
                                                        Name=SectionName, Prefix=PrefixLoc, Directory=DirectorySub))
  end do

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Evaluate_1D(This, Responses, Input, Output, LogValue)

  real(rkp)                                                           ::    Evaluate_1D

  class(LikelihoodProduct_Type), intent(inout)                        ::    This
  type(Response_Type), dimension(:), intent(in)                       ::    Responses
  type(Input_Type), intent(in)                                        ::    Input
  type(Output_Type), dimension(:), intent(in)                         ::    Output
  logical, optional, intent(in)                                       ::    LogValue

  character(*), parameter                                             ::    ProcName='Evaluate_1D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  class(LikelihoodFunction_Type), pointer                             ::    LFunctionPtr=>null()
  character(:), allocatable                                           ::    LabelLoc
  real(rkp)                                                           ::    VarR0D
  logical                                                             ::    LogValueLoc
  real(rkp)                                                           ::    HVarR0D
  real(rkp)                                                           ::    TVarR0D

  HVarR0D = dlog(huge(VarR0D))
  TVarR0D = dlog(tiny(VarR0D))

  LogValueLoc = .false.
  if (present(LogValue)) LogValueLoc = LogValue

  Evaluate_1D = Zero

  do i = 1, This%NbLikelihoods

    LFunctionPtr => This%Likelihoods(i)%GetPointer()

    VarR0D = LFunctionPtr%Evaluate(Responses=Responses, Input=Input, Output=Output, LogValue=.true.)
    nullify(LFunctionPtr)

    if (VarR0D <= -huge(One)) then
      Evaluate_1D = VarR0D
      exit
    end if

    Evaluate_1D = Evaluate_1D + VarR0D
    
  end do

  if (.not. LogValueLoc) then
    if (Evaluate_1D > TVarR0D .and. Evaluate_1D < HVarR0D) then
      Evaluate_1D = dexp(Evaluate_1D)
    elseif (Evaluate_1D < TVarR0D) then
      Evaluate_1D= Zero
    else
      call Error%Raise(Line='Likelihood Value above machine precision where ln(likelihood) is : ' //                           &
                          ConvertToString(Value=Evaluate_1D) // '. Consider changing value of the scalar modifier of responses')
    end if
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function Evaluate_0D(This, Response, Input, Output, LogValue)

  real(rkp)                                                           ::    Evaluate_0D

  class(LikelihoodProduct_Type), intent(inout)                        ::    This
  type(Response_Type), intent(in)                                     ::    Response
  type(Input_Type), intent(in)                                        ::    Input
  type(Output_Type), intent(in)                                       ::    Output
  logical, optional, intent(in)                                       ::    LogValue

  character(*), parameter                                             ::    ProcName='Evaluate_0D'
  integer                                                             ::    StatLoc=0
  integer                                                             ::    i
  class(LikelihoodFunction_Type), pointer                             ::    LFunctionPtr=>null()
  character(:), allocatable                                           ::    LabelLoc
  real(rkp)                                                           ::    VarR0D
  logical                                                             ::    LogValueLoc
  real(rkp)                                                           ::    HVarR0D
  real(rkp)                                                           ::    TVarR0D

  HVarR0D = dlog(huge(VarR0D))
  TVarR0D = dlog(tiny(VarR0D))

  LogValueLoc = .false.
  if (present(LogValue)) LogValueLoc = LogValue

  Evaluate_0D = Zero

  do i = 1, This%NbLikelihoods

    LFunctionPtr => This%Likelihoods(i)%GetPointer()

    VarR0D = LFunctionPtr%Evaluate(Response=Response, Input=Input, Output=Output, LogValue=.true.)
    nullify(LFunctionPtr)

    if (VarR0D <= -huge(One)) then
      Evaluate_0D = VarR0D
      exit
    end if

    Evaluate_0D = Evaluate_0D + VarR0D
    
  end do

  if (.not. LogValueLoc) then
    if (Evaluate_0D > TVarR0D .and. Evaluate_0D < HVarR0D) then
      Evaluate_0D = dexp(Evaluate_0D)
    elseif (Evaluate_0D < TVarR0D) then
      Evaluate_0D= Zero
    else
      call Error%Raise(Line='Likelihood Value above machine precision where ln(likelihood) is : ' //                           &
                          ConvertToString(Value=Evaluate_0D) // '. Consider changing value of the scalar modifier of responses')
    end if
  end if

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Copy(LHS, RHS)

  class(LikelihoodProduct_Type), intent(out)                          ::    LHS
  class(LikelihoodFunction_Type), intent(in)                          ::    RHS

  character(*), parameter                                             ::    ProcName='Copy'
  integer                                                             ::    i
  integer                                                             ::    StatLoc=0

  select type (RHS)

    type is (LikelihoodProduct_Type)
      call LHS%Reset()
      LHS%Constructed = RHS%Constructed

      if (RHS%Constructed) then
        LHS%Label = RHS%Label
        LHS%NbLikelihoods = RHS%NbLikelihoods
        allocate(LHS%Likelihoods, source=RHS%Likelihoods, stat=StatLoc)
        if (StatLoc /= 0) call Error%Allocate(Name='LHS%Likelihoods', ProcName=ProcName, stat=StatLoc)
      end if
    
    class default
      call Error%Raise(Line='Incompatible types', ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
impure elemental subroutine Finalizer(This)

  type(LikelihoodProduct_Type), intent(inout)                         ::    This

  character(*), parameter                                             ::    ProcName='Finalizer'
  integer                                                             ::    StatLoc=0

  if (allocated(This%Likelihoods)) deallocate(This%Likelihoods, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='This%Likelihoods', ProcName=ProcName, stat=StatLoc)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

end module
