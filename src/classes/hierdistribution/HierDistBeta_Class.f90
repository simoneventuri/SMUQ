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
! defined using shape and rate form
module HierDistBeta_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StatisticsRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use HierDistProb_Class                                            ,only:    HierDistProb_Type
use Input_Class                                                   ,only:    Input_Type
use DistBeta_Class                                                ,only:    DistBeta_Type
use DistProb_Class                                                ,only:    DistProb_Type

implicit none

private

public                                                                ::    HierDistBeta_Type

type, extends(HierDistProb_Type)                                      ::    HierDistBeta_Type
  real(rkp)                                                           ::    Alpha=One
  real(rkp)                                                           ::    Beta=One
  character(:), allocatable                                           ::    AlphaDependency
  character(:), allocatable                                           ::    BetaDependency
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Generate
  procedure, private                                                  ::    GenerateDistribution
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(HierDistBeta_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'hiererchical_gamma'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(HierDistBeta_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(HierDistBeta_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%A = Zero
    This%B = One
    This%Alpha = One
    This%Beta = One
    This%TruncatedRight = .true.
    This%TruncatedLeft = .true.
    This%AlphaDependency=''
    This%BetaDependency=''
    This%ADependency=''
    This%BDependency=''

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(HierDistBeta_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ProcessInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    PrefixLoc
    logical                                                           ::    MandatoryLoc

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()
    
    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    MandatoryLoc = .true.
    ParameterName = 'alpha_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%AlphaDependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'alpha'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%Alpha = VarR0D

    MandatoryLoc = .true.
    ParameterName = 'beta_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%BetaDependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'beta'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%Beta = VarR0D

    ParameterName = 'a_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%ADependency = VarC0D
    ParameterName = 'a'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%A = VarR0D

    ParameterName = 'b_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%BDependency = VarC0D
    ParameterName = 'b'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%B = VarR0D

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(HierDistBeta_Type), intent(in)                              ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )
    call GetInput%AddParameter( Name='alpha', Value=ConvertToString( Value=This%Alpha ) )
    call GetInput%AddParameter( Name='beta', Value=ConvertToString( Value=This%Beta ) )
    call GetInput%AddParameter( Name='a', Value=ConvertToString( Value=This%A ) )
    call GetInput%AddParameter( Name='b', Value=ConvertToString( Value=This%B ) )
    if ( len_trim(This%AlphaDependency) /= 0 ) call GetInput%AddParameter( Name='alpha_dependency', Value=This%AlphaDependency )
    if ( len_trim(This%BetaDependency) /= 0 ) call GetInput%AddParameter( Name='beta_dependency', Value=This%BetaDependency )
    if ( len_trim(This%ADependency) /= 0 ) call GetInput%AddParameter( Name='a_dependency', Value=This%ADependency )
    if ( len_trim(This%BDependency) /= 0 ) call GetInput%AddParameter( Name='b_dependency', Value=This%BDependency )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Generate( This, Input, Distribution )

    class(HierDistBeta_Type), intent(in)                              ::    This
    type(Input_Type), intent(in)                                      ::    Input
    class(DistProb_Type), allocatable, intent(out)                    ::    Distribution

    character(*), parameter                                           ::    ProcName='Generate'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    Alpha
    real(rkp)                                                         ::    Beta
    real(rkp)                                                         ::    A
    real(rkp)                                                         ::    B

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    B = Zero

    Alpha = This%Alpha
    if ( len_trim(This%AlphaDependency) /= 0 ) call Input%GetValue( Value=Alpha, Label=This%AlphaDependency )

    Beta = This%Beta
    if ( len_trim(This%BetaDependency) /= 0 ) call Input%GetValue( Value=Beta, Label=This%BetaDependency )

    A = This%A
    if ( len_trim(This%ADependency) /= 0 ) call Input%GetValue( Value=A, Label=This%ADependency )
    
    B = This%B
    if ( len_trim(This%BDependency) /= 0 ) call Input%GetValue( Value=B, Label=This%BDependency )

    call This%GenerateDistribution( Alpha=Alpha, Beta=Beta, A=A, B=B, Distribution=Distribution )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GenerateDistribution( This, Alpha, Beta, A, B, Distribution )

    class(HierDistBeta_Type), intent(in)                              ::    This
    real(rkp), intent(in)                                             ::    Alpha
    real(rkp), intent(in)                                             ::    Beta
    real(rkp), intent(in)                                             ::    A
    real(rkp), intent(in)                                             ::    B
    class(DistProb_Type), allocatable, intent(out)                    ::    Distribution

    character(*), parameter                                           ::    ProcName='GenerateDistribution'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    allocate( DistBeta_Type :: Distribution )

    select type ( Distribution )
      type is ( DistBeta_Type ) 
        call Distribution%Construct( Alpha=Alpha, Beta=Beta, A=A, B=B )
      class default
        call Error%Raise( "Something went wrong", ProcName=ProcName )
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(HierDistBeta_Type), intent(out)                            ::    LHS
    class(HierDistProb_Type), intent(in)                              ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (HierDistBeta_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%A = RHS%A
          LHS%B = RHS%B
          LHS%Alpha = RHS%Alpha
          LHS%Beta = RHS%Beta
          LHS%TruncatedLeft = RHS%TruncatedLeft
          LHS%TruncatedRight = RHS%TruncatedRight
          LHS%AlphaDependency = RHS%AlphaDependency
          LHS%BetaDependency = RHS%BetaDependency
          LHS%ADependency = RHS%ADependency
          LHS%BDependency = RHS%BDependency
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(HierDistBeta_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
