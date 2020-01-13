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

module HierDistLogistic_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StatisticsRoutines_Module
use HierDistProb_Class                                            ,only:    HierDistProb_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use DistLogistic_Class                                            ,only:    DistLogistic_Type
use DistProb_Class                                                ,only:    DistProb_Type

implicit none

private

public                                                                ::    HierDistLogistic_Type

type, extends(HierDistProb_Type)                                      ::    HierDistLogistic_Type
  real(rkp)                                                           ::    Mu=Zero
  real(rkp)                                                           ::    S=One
  character(:), allocatable                                           ::    MuDependency
  character(:), allocatable                                           ::    SDependency
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

    class(HierDistLogistic_Type), intent(inout)                       ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'hierarchical_logistic'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(HierDistLogistic_Type), intent(inout)                       ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(HierDistLogistic_Type), intent(inout)                       ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%A = tiny(One)
    This%B = One
    This%Mu = Zero
    This%S = One
    This%TruncatedRight = .false.
    This%TruncatedLeft = .false.
    This%MuDependency=''
    This%SDependency=''
    This%ADependency=''
    This%BDependency=''

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(HierDistLogistic_Type), intent(inout)                       ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
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
    ParameterName = 'mu_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%MuDependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'mu'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%Mu = VarR0D

    MandatoryLoc = .true.
    ParameterName = 's_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%SDependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 's'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%S = VarR0D

    ParameterName = 'a_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%ADependency = VarC0D
      This%TruncatedLeft = .true.
    end if
    ParameterName = 'a'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%A = VarR0D
      This%TruncatedLeft = .true.
    end if

    ParameterName = 'b_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%BDependency = VarC0D
      This%TruncatedRight = .true.
    end if
    ParameterName = 'b'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) then
      This%B = VarR0D
      This%TruncatedRight = .true.
    end if

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Generate( This, Input, Distribution )

    class(HierDistLogistic_Type), intent(in)                          ::    This
    type(Input_Type), intent(in)                                      ::    Input
    class(DistProb_Type), allocatable, intent(out)                    ::    Distribution

    character(*), parameter                                           ::    ProcName='Generate'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    VarR0D     
    real(rkp)                                                         ::    Mu
    real(rkp)                                                         ::    S  
    real(rkp)                                                         ::    A  
    real(rkp)                                                         ::    B  

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    A = Zero
    B = Zero

    Mu = This%Mu
    if ( len_trim(This%MuDependency) /= 0 ) call Input%GetValue( Value=Mu, Label=This%MuDependency )

    S = This%S
    if ( len_trim(This%SDependency) /= 0 ) call Input%GetValue( Value=S, Label=This%SDependency )

    if ( This%TruncatedLeft ) then
      if ( len_trim(This%ADependency) /= 0 ) then
        call Input%GetValue( Value=A, Label=This%ADependency )
      else
        A = This%A
      end if
    end if

    if ( This%TruncatedRight ) then
      if ( len_trim(This%BDependency) /= 0 ) then
        call Input%GetValue( Value=B, Label=This%BDependency )
      else
        B = This%B
      end if
    end if

    call This%GenerateDistribution( Mu=Mu, S=S, A=A, B=B, Distribution=Distribution )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GenerateDistribution( This, Mu, S, A, B, Distribution )

    class(HierDistLogistic_Type), intent(in)                          ::    This
    real(rkp), intent(in)                                             ::    Mu
    real(rkp), intent(in)                                             ::    S
    real(rkp), intent(in)                                             ::    A
    real(rkp), intent(in)                                             ::    B
    class(DistProb_Type), allocatable, intent(out)                    ::    Distribution

    character(*), parameter                                           ::    ProcName='GenerateDistribution'
    integer                                                           ::    StatLoc=0

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    allocate( DistLogistic_Type :: Distribution )

    select type ( Distribution )
      type is ( DistLogistic_Type ) 
        if ( This%TruncatedLeft .and. This%TruncatedRight ) then
          call Distribution%Construct( Mu=Mu, S=S, A=A, B=B )
        elseif ( This%TruncatedLeft ) then
          call Distribution%Construct( Mu=Mu, S=S, A=A )
        elseif ( This%TruncatedRight ) then
          call Distribution%Construct( Mu=Mu, S=S, B=B )
        else
          call Distribution%Construct( Mu=Mu, S=S )
        end if
      class default
        call Error%Raise( "Something went wrong", ProcName=ProcName )
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(HierDistLogistic_Type), intent(in)                          ::    This
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
    call GetInput%AddParameter( Name='mu', Value=ConvertToString( Value=This%Mu ) )
    call GetInput%AddParameter( Name='s', Value=ConvertToString( Value=This%S ) )
    if ( This%TruncatedLeft ) call GetInput%AddParameter( Name='a', Value=ConvertToString( Value=This%A ) )
    if ( This%TruncatedRight ) call GetInput%AddParameter( Name='b', Value=ConvertToString( Value=This%B ) )
    if ( len_trim(This%MuDependency) /= 0 ) call GetInput%AddParameter( Name='mu_dependency', Value=This%MuDependency )
    if ( len_trim(This%SDependency) /= 0 ) call GetInput%AddParameter( Name='s_dependency', Value=This%SDependency )
    if ( len_trim(This%ADependency) /= 0 ) call GetInput%AddParameter( Name='a_dependency', Value=This%ADependency )
    if ( len_trim(This%BDependency) /= 0 ) call GetInput%AddParameter( Name='b_dependency', Value=This%BDependency )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(HierDistLogistic_Type), intent(out)                         ::    LHS
    class(HierDistProb_Type), intent(in)                              ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (HierDistLogistic_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%A = RHS%A
          LHS%B = RHS%B
          LHS%Mu = RHS%Mu
          LHS%S = RHS%S
          LHS%TruncatedLeft = RHS%TruncatedLeft
          LHS%TruncatedRight = RHS%TruncatedRight
          LHS%MuDependency = RHS%MuDependency
          LHS%SDependency = RHS%SDependency
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

    type(HierDistLogistic_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
