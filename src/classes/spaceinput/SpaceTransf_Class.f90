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

module SpaceTransf_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use DistProb_Class                                                ,only:    DistProb_Type
use DistProb_Vec_Class                                            ,only:    DistProb_Vec_Type
use SpaceInput_Class                                              ,only:    SpaceInput_Type
use SpaceParam_Class                                              ,only:    SpaceParam_Type

implicit none

private

public                                                                ::    SpaceTransf_Type

type, abstract, extends(SpaceInput_Type)                              ::    SpaceTransf_Type
  type(SpaceParam_Type)                                               ::    SpaceInputOrig
  type(SpaceParam_Type)                                               ::    SpaceInput
contains
  procedure, private                                                  ::    GetDistribution0D
  procedure, private                                                  ::    GetDistribution1D
  procedure, public                                                   ::    GetDistributionPointer
  procedure, private                                                  ::    GetDistribution0DOrig
  procedure, private                                                  ::    GetDistribution1DOrig
  procedure, private                                                  ::    GetParamName0D
  procedure, private                                                  ::    GetParamName1D
  procedure, private                                                  ::    GetLabel0D
  procedure, private                                                  ::    GetLabel1D
  procedure, public                                                   ::    GetCorrMat
  procedure, public                                                   ::    GetCorrMatOrig
  generic, public                                                     ::    Construct               =>    ConstructInput
  generic, public                                                     ::    Transform               =>    Transform1D,            &
                                                                                                          Transform2D
  procedure, public                                                   ::    Transform2D
  generic, public                                                     ::    InvTransform            =>    InvTransform1D,         &
                                                                                                          InvTransform2D
  procedure, public                                                   ::    InvTransform2D
  procedure(Initialize_SpaceTransf), deferred, public                 ::    Initialize
  procedure(Reset_SpaceTransf), deferred, public                      ::    Reset
  procedure(SetDefaults_SpaceTransf), deferred, public                ::    SetDefaults
  procedure(ConstructInput_SpaceTransf), deferred, private            ::    ConstructInput
  procedure(GetInput_SpaceTransf), deferred, public                   ::    GetInput
  procedure(Transform1D_SpaceTransf), deferred, public                ::    Transform1D
  procedure(InvTransform1D_SpaceTransf), deferred, public             ::    InvTransform1D
end type

logical, parameter                                                    ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_SpaceTransf( This, Debug )
    import                                                            ::    SpaceTransf_Type
    class(SpaceTransf_Type), intent(inout)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_SpaceTransf( This, Debug )
    import                                                            ::    SpaceTransf_Type
    class(SpaceTransf_Type), intent(inout)                            ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_SpaceTransf( This, Debug )
    import                                                            ::    SpaceTransf_Type
    class(SpaceTransf_Type),intent(inout)                             ::    This
    logical, optional, intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------  

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_SpaceTransf( This, Input, SpaceInput, Prefix, Debug )
    import                                                            ::    SpaceTransf_Type
    import                                                            ::    InputSection_Type
    import                                                            ::    SpaceParam_Type
    class(SpaceTransf_Type), intent(inout)                            ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    type(SpaceParam_Type), optional, intent(in)                       ::    SpaceInput
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_SpaceTransf( This, MainSectionName, Prefix, Directory, Debug )
      use StringRoutines_Module
      use String_Library
      import                                                          ::    InputSection_Type
      import                                                          ::    SpaceParam_Type
      import                                                          ::    SpaceTransf_Type
      type(InputSection_Type)                                         ::    GetInput_SpaceTransf
      class(SpaceTransf_Type), intent(in)                             ::    This
      character(*), intent(in)                                        ::    MainSectionName
      character(*), optional, intent(in)                              ::    Prefix
      character(*), optional, intent(in)                              ::    Directory
      logical, optional ,intent(in)                                   ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Transform1D_SpaceTransf( This, X, Debug )
    use Parameters_Library
    import                                                            ::    SpaceTransf_Type
    real(rkp), allocatable, dimension(:)                              ::    Transform1D_SpaceTransf   
    class(SpaceTransf_Type), intent(inout)                            ::    This
    real(rkp), dimension(:), intent(in)                               ::    X
    logical, optional ,intent(in)                                     ::    Debug                                             
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvTransform1D_SpaceTransf( This, Z, Debug )
    use Parameters_Library
    import                                                            ::    SpaceTransf_Type
    real(rkp), allocatable, dimension(:)                              ::    InvTransform1D_SpaceTransf   
    class(SpaceTransf_Type), intent(inout)                            ::    This
    real(rkp), dimension(:), intent(in)                               ::    Z
    logical, optional ,intent(in)                                     ::    Debug                                             
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains 

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistribution0D( This, Num, Debug )

    class(DistProb_Type), allocatable                                 ::    GetDistribution0D

    class(SpaceTransf_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Num
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetDistribution0D'
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Num > This%NbDim ) call Error%Raise( Line='Num specifier above maximum number of distributions', ProcName=ProcName )
    if ( Num < 1 ) call Error%Raise( Line='Num specifier below minimum of 1', ProcName=ProcName )

    allocate(GetDistribution0D, source=This%SpaceInput%GetDistributionPointer(Num=Num), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetDistribution0D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistribution1D( This, Debug )

    type(DistProb_Vec_Type), allocatable, dimension(:)                ::    GetDistribution1D

    class(SpaceTransf_Type), intent(in)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetDistribution1D'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate( GetDistribution1D(This%NbDim), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( name='GetDistribution1D', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbDim
      call GetDistribution1D(i)%Set( Object=This%SpaceInput%GetDistributionPointer(Num=i) )
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistributionPointer( This, Num, Debug )

    class(DistProb_Type), pointer                                     ::    GetDistributionPointer

    class(SpaceTransf_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Num
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetDistributionPointer'
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Num > This%NbDim ) call Error%Raise( Line='Num specifier above maximum number of distributions', ProcName=ProcName )
    if ( Num < 1 ) call Error%Raise( Line='Num specifier below minimum of 1', ProcName=ProcName )

    GetDistributionPointer => This%SpaceInput%GetDistributionPointer(Num=Num)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistribution0DOrig( This, Num, Debug )

    class(DistProb_Type), allocatable                                 ::    GetDistribution0DOrig

    class(SpaceTransf_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Num
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetDistribution0DOrig'
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( Num > This%NbDim ) call Error%Raise( Line='Num specifier above maximum number of distributions', ProcName=ProcName )
    if ( Num < 1 ) call Error%Raise( Line='Num specifier below minimum of 1', ProcName=ProcName )

    allocate(GetDistribution0DOrig, source=This%SpaceInputOrig%GetDistributionPointer(Num=Num), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetDistribution0DOrig', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetDistribution1DOrig( This, Debug )

    class(DistProb_Vec_Type), allocatable, dimension(:)               ::    GetDistribution1DOrig

    class(SpaceTransf_Type), intent(in)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetDistribution1DOrig'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate( GetDistribution1DOrig(This%NbDim), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( name='GetDistribution1DOrig', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbDim
      call GetDistribution1DOrig(i)%Set( Object=This%SpaceInputOrig%GetDistributionPointer(Num=i) )
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCorrMat( This, Debug )

    real(rkp), allocatable, dimension(:,:)                            ::    GetCorrMat
    class(SpaceTransf_Type), intent(in)                               ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetCorrMat'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetCorrMat, source=This%SpaceInput%GetCorrMat(), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetCorrMat', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetCorrMatOrig( This, Debug )

    real(rkp), allocatable, dimension(:,:)                            ::    GetCorrMatOrig
    class(SpaceTransf_Type), intent(in)                               ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetCorrMatOrig'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetCorrMatOrig, source=This%SpaceInputOrig%GetCorrMat(), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetCorrMatOrig', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetParamName0D( This, Num, Debug )

    use String_Library

    character(:), allocatable                                         ::    GetParamName0D
    class(SpaceTransf_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Num
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='GetParamName0D'
    logical                                                           ::    DebugLoc
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetParamName0D = This%SpaceInput%GetParamName( Num=Num )      

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetParamName1D( This, Debug )

    use String_Library

    type(String_Type), allocatable, dimension(:)                      ::    GetParamName1D
    class(SpaceTransf_Type), intent(in)                               ::    This
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='GetParamName1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetParamName1D, source=This%SpaceInput%GetParamName(), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetParamName1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel0D( This, Num, Debug )

    use String_Library

    character(:), allocatable                                         ::    GetLabel0D
    class(SpaceTransf_Type), intent(in)                               ::    This
    integer, intent(in)                                               ::    Num
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='GetLabel0D'
    logical                                                           ::    DebugLoc
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetLabel0D = This%SpaceInput%GetLabel( Num=Num )      

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel1D( This, Debug )

    use String_Library

    type(String_Type), allocatable, dimension(:)                      ::    GetLabel1D
    class(SpaceTransf_Type), intent(in)                               ::    This
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='GetLabel1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetLabel1D, source=This%SpaceInput%GetLabel(), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetLabel1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Transform2D( This, X, Debug )

    real(rkp), allocatable, dimension(:,:)                            ::    Transform2D  

    class(SpaceTransf_Type), intent(inout)                            ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    X
    logical, optional ,intent(in)                                     ::    Debug   

    character(*), parameter                                           ::    ProcName='Transform2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbDegen
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%SpaceInput%GetNbDim() /= size(X,1) ) call Error%Raise( Line='Incorrect dimensionality', ProcName=ProcName )

    NbDegen = size(X,2)

    allocate(Transform2D(This%SpaceInput%GetNbDim(),NbDegen), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Transform2D', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbDegen
      Transform2D(:,i) = This%Transform( X=X(:,i) )
    end do

    if (DebugLoc) call Logger%Exiting
                                          
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvTransform2D( This, Z, Debug )

    real(rkp), allocatable, dimension(:,:)                            ::    InvTransform2D 
 
    class(SpaceTransf_Type), intent(inout)                            ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Z
    logical, optional ,intent(in)                                     ::    Debug   

    character(*), parameter                                           ::    ProcName='InvTransform2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbDegen
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%SpaceInput%GetNbDim() /= size(Z,1) ) call Error%Raise( Line='Incorrect dimensionality', ProcName=ProcName )

    NbDegen = size(Z,2)

    allocate(InvTransform2D(This%SpaceInput%GetNbDim(),NbDegen), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Transform2D', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbDegen
      InvTransform2D(:,i) = This%InvTransform( Z=Z(:,i) )
    end do

    if (DebugLoc) call Logger%Exiting
                                            
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
