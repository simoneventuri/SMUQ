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

module SampleSpace_Class

use Parameters_Library
use Input_Library
use String_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use DistProb_Class                                                ,only:    DistProb_Type
use DistProb_Vec_Class                                            ,only:    DistProb_Vec_Type

implicit none

private

public                                                                ::    SampleSpace_Type

type, abstract                                                        ::    SampleSpace_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbDim=0
  logical                                                             ::    Correlated=.false.
  type(DistProb_Vec_Type), allocatable, dimension(:)                  ::    DistProb
  type(String_Type), allocatable, dimension(:)                        ::    ParamName
  type(String_Type), allocatable, dimension(:)                        ::    Label
  real(rkp), dimension(:,:), allocatable                              ::    CorrMat
contains
  procedure(Initialize_SampleSpace), deferred, public                 ::    Initialize
  procedure(Reset_SampleSpace), deferred, public                      ::    Reset
  procedure(SetDefaults_SampleSpace), deferred, public                ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure(ConstructInput_SampleSpace), deferred, private            ::    ConstructInput
  procedure(GetInput_SampleSpace), deferred, public                   ::    GetInput
  procedure, public                                                   ::    GetNbDim
  procedure, public                                                   ::    IsCorrelated
  generic, public                                                     ::    GetDistribution         =>    GetDist0D_Label,        &
                                                                                                          GetDist0D_Num,          &
                                                                                                          GetDist1D
  procedure, private                                                  ::    GetDist0D_Label
  procedure, private                                                  ::    GetDist0D_Num
  procedure, private                                                  ::    GetDist1D
  generic, public                                                     ::    GetLabel                =>    GetLabel0D,             &
                                                                                                          GetLabel1D
  procedure, private                                                  ::    GetLabel0D
  procedure, private                                                  ::    GetLabel1D
  generic, public                                                     ::    GetName                 =>    GetName0D_Label,        &
                                                                                                          GetName0D_Num,          &
                                                                                                          GetName1D
  procedure, private                                                  ::    GetName0D_Label
  procedure, private                                                  ::    GetName0D_Num
  procedure, private                                                  ::    GetName1D
  generic, public                                                     ::    GetDistributionPointer  =>    GetDistPointer_Label,   &
                                                                                                          GetDistPointer_Num
  procedure, public                                                   ::    GetDistPointer_Label
  procedure, public                                                   ::    GetDistPointer_Num
  procedure, public                                                   ::    GetCorrMat
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure(Copy_SampleSpace), deferred, public                       ::    Copy
end type

logical, parameter                                                    ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_SampleSpace( This )
    import                                                            ::    SampleSpace_Type
    class(SampleSpace_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_SampleSpace( This )
    import                                                            ::    SampleSpace_Type
    class(SampleSpace_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_SampleSpace( This )
    import                                                            ::    SampleSpace_Type
    class(SampleSpace_Type),intent(inout)                             ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------  

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_SampleSpace( This, Input, Prefix )
    import                                                            ::    SampleSpace_Type
    import                                                            ::    InputSection_Type
    class(SampleSpace_Type), intent(inout)                            ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_SampleSpace( This, MainSectionName, Prefix, Directory )
      import                                                          ::    InputSection_Type
      import                                                          ::    SampleSpace_Type
      type(InputSection_Type)                                         ::    GetInput_SampleSpace
      class(SampleSpace_Type), intent(in)                             ::    This
      character(*), intent(in)                                        ::    MainSectionName
      character(*), optional, intent(in)                              ::    Prefix
      character(*), optional, intent(in)                              ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_SampleSpace( LHS, RHS )
    import                                                            ::    SampleSpace_Type
    class(SampleSpace_Type), intent(out)                              ::    LHS
    class(SampleSpace_Type), intent(in)                               ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName0D_Num( This, Num )

    character(:), allocatable                                         ::    GetName0D_Num
    class(SampleSpace_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Num

    character(*), parameter                                           ::    ProcName='GetName0D_Num'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Num > This%NbDim ) call Error%Raise( Line='Num specifier above maximum number of distributions', ProcName=ProcName )
    if ( Num < 1 ) call Error%Raise( Line='Num specifier below minimum of 1', ProcName=ProcName )

    GetName0D_Num = This%ParamName(Num)%GetValue()      

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName0D_Label( This, Label )

    character(:), allocatable                                         ::    GetName0D_Label
    class(SampleSpace_Type), intent(in)                               ::    This
    character(*), intent(in)                                          ::    Label

    character(*), parameter                                           ::    ProcName='GetName0D_Label'
    integer                                                           ::    i
    integer                                                           ::    ii

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    i = 1
    ii = 0
    do i = 1, This%NbDim
      if ( This%Label(i)%GetValue() /= Label ) cycle
      ii = i
      exit
    end do

    if ( ii == 0 ) call Error%Raise( 'Did not find required parameter with label : ' // Label, ProcName=ProcName )

    GetName0D_Label = This%ParamName(ii)%GetValue()      

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName1D( This )

    type(String_Type), allocatable, dimension(:)                      ::    GetName1D
    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='GetName1D'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetName1D, source=This%ParamName, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetName1D', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel0D( This, Num )

    character(:), allocatable                                         ::    GetLabel0D
    class(SampleSpace_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Num

    character(*), parameter                                           ::    ProcName='GetLabel0D'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetLabel0D = This%Label(Num)%GetValue()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel1D( This )

    type(String_Type), allocatable, dimension(:)                      ::    GetLabel1D
    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='GetLabel1D'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetLabel1D, source=This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetLabel1D', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDist0D_Num( This, Num )

    class(DistProb_Type), allocatable                                 ::    GetDist0D_Num

    class(SampleSpace_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Num

    character(*), parameter                                           ::    ProcName='GetDist0D_Num'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Num > This%NbDim ) call Error%Raise( Line='Num specifier above maximum number of distributions', ProcName=ProcName )
    if ( Num < 1 ) call Error%Raise( Line='Num specifier below minimum of 1', ProcName=ProcName )

    allocate(GetDist0D_Num, source=This%DistProb(Num)%GetPointer(), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetDist0D_Num', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDist0D_Label( This, Label )

    class(DistProb_Type), allocatable                                 ::    GetDist0D_Label

    class(SampleSpace_Type), intent(in)                               ::    This
    character(*), intent(in)                                          ::    Label

    character(*), parameter                                           ::    ProcName='GetDist0D_Label'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    i = 1
    ii = 0
    do i = 1, This%NbDim
      if ( This%Label(i)%GetValue() /= Label ) cycle
      ii = i
      exit
    end do

    if ( ii == 0 ) call Error%Raise( 'Did not find required parameter with label : ' // Label, ProcName=ProcName )

    allocate(GetDist0D_Label, source=This%DistProb(ii)%GetPointer(), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetDist0D_Num', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDist1D( This )

    type(DistProb_Vec_Type), allocatable, dimension(:)                ::    GetDist1D

    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='GetDist1D'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetDist1D, source=This%DistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetDist1D', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistPointer_Label( This, Label )

    class(DistProb_Type), pointer                                     ::    GetDistPointer_Label

    class(SampleSpace_Type), intent(in)                               ::    This
    character(*), intent(in)                                          ::    Label

    character(*), parameter                                           ::    ProcName='GetDistPointer_Label'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    ii

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    i = 1
    ii = 0
    do i = 1, This%NbDim
      if ( This%Label(i)%GetValue() /= Label ) cycle
      ii = i
      exit
    end do

    if ( ii == 0 ) call Error%Raise( 'Did not find required parameter with label : ' // Label, ProcName=ProcName )

    GetDistPointer_Label => This%DistProb(ii)%GetPointer()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistPointer_Num( This, Num )

    class(DistProb_Type), pointer                                     ::    GetDistPointer_Num

    class(SampleSpace_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Num

    character(*), parameter                                           ::    ProcName='GetDistPointer_Num'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Num > This%NbDim ) call Error%Raise( Line='Num specifier above maximum number of distributions', ProcName=ProcName )
    if ( Num < 1 ) call Error%Raise( Line='Num specifier below minimum of 1', ProcName=ProcName )

    GetDistPointer_Num => This%DistProb(Num)%GetPointer()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCorrMat( This )

    real(rkp), allocatable, dimension(:,:)                            ::    GetCorrMat
    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='GetCorrMat'
    integer                                                           ::    StatLoc=0

    allocate(GetCorrMat, source=This%CorrMat, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetCorrMat', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This )

    character(:), allocatable                                         ::    GetName
    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='GetName'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetName = This%Name

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbDim( This )

    integer                                                           ::    GetNbDim
    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='GetNbDim'

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbDim = This%NbDim

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsCorrelated( This )

    logical                                                           ::    IsCorrelated
    class(SampleSpace_Type), intent(in)                               ::    This

    character(*), parameter                                           ::    ProcName='IsCorrelated'
    integer                                                           ::    StatLoc=0

    IsCorrelated = This%Correlated

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
