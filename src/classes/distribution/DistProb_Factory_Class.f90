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

module DistProb_Factory_Class

use DistProb_Class                                                ,only:    DistProb_Type
use DistUnif_Class                                                ,only:    DistUnif_Type
use DistLogUnif_Class                                             ,only:    DistLogUnif_Type
use DistLog10Unif_Class                                           ,only:    DistLog10Unif_Type
use DistNorm_Class                                                ,only:    DistNorm_Type
use DistLogNorm_Class                                             ,only:    DistLogNorm_Type
use DistLog10Norm_Class                                           ,only:    DistLog10Norm_Type
use DistGamma_Class                                               ,only:    DistGamma_Type
use DistLogistic_Class                                            ,only:    DistLogistic_Type
use DistKernel_Class                                              ,only:    DistKernel_Type
use DistInfBoundTransf_Class                                      ,only:    DistInfBoundTransf_Type
use Input_Library
use String_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputVerifier_Class                                           ,only:    InputVerifier_Type

implicit none

private

public                                                                ::    DistProb_Factory
public                                                                ::    DistProb_Factory_Type

type                                                                  ::    DistProb_Factory_Type
contains
  generic, public                                                     ::    Construct               =>    Construct_C0D,          &
                                                                                                          Construct_Input
  procedure, nopass, public                                           ::    Construct_C0D
  procedure, public                                                   ::    Construct_Input
  procedure, nopass, public                                           ::    GetOption
  procedure, public                                                   ::    GetObjectInput
End Type

type(DistProb_Factory_Type)                                           ::    DistProb_Factory
logical, parameter                                                    ::    DebugGlobal = .false.

contains

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Construct_C0D(Object, DesiredType)

  class(DistProb_Type), allocatable, intent(inout)                    ::    Object                                             
  character(*), intent(in)                                            ::    DesiredType                                               

  character(*), parameter                                             ::    ProcName='Construct_C0D' 

  if (allocated(Object)) call Error%Raise(Line="Object already allocated", ProcName=ProcName)

  select case (LowerCase(DesiredType))

    case('uniform')
      allocate(DistUnif_Type   :: Object)

    case('loguniform')
      allocate(DistLogUnif_Type   :: Object)

    case('log10uniform')
      allocate(DistLog10Unif_Type   :: Object)

    case('normal')
      allocate(DistNorm_Type   :: Object)

    case('lognormal')
      allocate(DistLogNorm_Type   :: Object)

    case('log10normal')
      allocate(DistLog10Norm_Type   :: Object)

    case('gamma')
      allocate(DistGamma_Type   :: Object)

    case('logistic')
      allocate(DistLogistic_Type   :: Object)

    case('kernel')
      allocate(DistKernel_Type   :: Object)

    case('infinite_bound_transform')
      allocate(DistInfBoundTransf_Type   :: Object)

    case default
      call Error%Raise(Line="Type not supported: DesiredType = " // DesiredType, ProcName=ProcName)

  end select

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
subroutine Construct_Input(This, Object, Input, Prefix)
  
  use Input_Library

  class(DistProb_Factory_Type), intent(in)                            ::    This
  class(DistProb_Type), allocatable, intent(inout)                    ::    Object
  type(InputSection_Type), intent(in)                                 ::    Input
  character(*), optional, intent(in)                                  ::    Prefix

  character(*), parameter                                             ::    ProcName='Construct_Input'                                   
  type(InputSection_Type), pointer                                    ::    InputSection=>null()
  character(:), allocatable                                           ::    ParameterName
  character(:), allocatable                                           ::    SectionName
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    VarC0D
  integer                                                             ::    StatLoc=0 
  type(InputVerifier_Type)                                            ::    InputVerifier

  PrefixLoc = ''
  if (present(Prefix)) PrefixLoc = Prefix

  call InputVerifier%Construct()
  call InputVerifier%AddParameter(Parameter='type')
  call InputVerifier%AddSection(Section='type')
  call InputVerifier%Process(Input=Input)
  call InputVerifier%Reset()

  ParameterName = 'type'
  call Input%GetValue(Value=VarC0D, ParameterName=ParameterName, Mandatory=.true.)
  call This%Construct(Object=Object, DesiredType=VarC0D)

  SectionName = 'type'
  call Input%FindTargetSection(TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true.)
  call Object%Construct(Input=InputSection, Prefix=PrefixLoc)
  nullify(InputSection)

end subroutine
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetOption(Object)

  character(:), allocatable                                           ::    GetOption

  class(DistProb_Type), intent(in)                                    ::    Object                                                                                            

  character(*), parameter                                             ::    ProcName='GetOption' 

  select type (Object)

    type is (DistUnif_Type)
      GetOption = 'uniform'

    type is (DistLogUnif_Type)
      GetOption = 'loguniform'

    type is (DistLog10Unif_Type)
      GetOption = 'log10uniform'

    type is (DistNorm_Type)
      GetOption = 'normal'

    type is (DistLogNorm_Type)
      GetOption = 'lognormal'

    type is (DistLog10Norm_Type)
      GetOption = 'log10normal'

    type is (DistGamma_Type)
      GetOption = 'gamma'

    type is (DistLogistic_Type)
      GetOption = 'logistic'

    type is (DistKernel_Type)
      GetOption = 'kernel'

    type is (DistInfBoundTransf_Type)
      GetOption = 'infinite_bound_transform'

    class default
      call Error%Raise(Line="Object is either not allocated/associated or definitions are not up to date", ProcName=ProcName)

  end select

end function
!!------------------------------------------------------------------------------------------------------------------------------

!!------------------------------------------------------------------------------------------------------------------------------
function GetObjectInput(This, Object, Name, Prefix, Directory)

  use Input_Library

  type(InputSection_Type)                                             ::    GetObjectInput

  class(DistProb_Factory_Type), intent(in)                            ::    This
  class(DistProb_Type), intent(in)                                    ::    Object
  character(*), intent(in)                                            ::    Name
  character(*), optional, intent(in)                                  ::    Prefix
  character(*), optional, intent(in)                                  ::    Directory

  character(*), parameter                                             ::    ProcName='GetObjectInput'
  character(:), allocatable                                           ::    PrefixLoc
  character(:), allocatable                                           ::    DirectoryLoc
  character(:), allocatable                                           ::    DirectorySub
  logical                                                             ::    ExternalFlag=.false.
  integer                                                             ::    StatLoc=0

  DirectoryLoc = ''
  PrefixLoc = ''
  DirectorySub = DirectoryLoc
  if (present(Directory)) DirectoryLoc = Directory
  if (present(Prefix)) PrefixLoc = Prefix

  if (len_trim(DirectoryLoc) /= 0) ExternalFlag = .true.

  call GetObjectInput%SetName(SectionName=Name)

  call GetObjectInput%AddParameter(Name='type', Value=This%GetOption(Object=Object))

  if (ExternalFlag) DirectorySub = DirectoryLoc // 'type/'

  call GetObjectInput%AddSection(Section=Object%GetInput(Name='type', Prefix=PrefixLoc, Directory=DirectorySub))

end function
!!------------------------------------------------------------------------------------------------------------------------------

end module
