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

module DistInfBoundTransf_Class

use Prob_Library
use Input_Library
use Parameters_Library
use DistProb_Class                                                ,only:    DistProb_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use BaseDistProb_Factory_Class                                    ,only:    BaseDistProb_Factory

implicit none

private

public                                                                ::    DistInfBoundTransf_Type

type, extends(DistProb_Type)                                          ::    DistInfBoundTransf_Type
  class(DistProb_Type), allocatable                                   ::    DistProb
  logical                                                             ::    DistTLeft
  logical                                                             ::    DistTRight
  real(rkp)                                                           ::    DistA
  real(rkp)                                                           ::    DistB
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    PDF_R0D
  procedure, public                                                   ::    CDF_R0D
  procedure, public                                                   ::    InvCDF_R0D
  generic, private                                                    ::    Transform               =>    Transform_0D,           &
                                                                                                          Transform_1D
  procedure, private                                                  ::    Transform_0D
  procedure, private                                                  ::    Transform_1D
  generic, private                                                    ::    InvTransform            =>    InvTransform_0D,        &
                                                                                                          InvTransform_1D
  procedure, private                                                  ::    InvTransform_0D
  procedure, private                                                  ::    InvTransform_1D
  generic, private                                                    ::    fInvTransform           =>    fInvTransform_0D
  procedure, private                                                  ::    fInvTransform_0D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(DistInfBoundTransf_Type), intent(inout)                     ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'infinite_bound_transform'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(DistInfBoundTransf_Type), intent(inout)                     ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%DistProb) ) deallocate(This%DistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%DistProb', ProcName=ProcName, stat=StatLoc )

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(DistInfBoundTransf_Type), intent(inout)                     ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%DistTLeft = .false.
    This%DistTRight = .false.
    This%DistA = Zero
    This%DistB = Zero

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(DistInfBoundTransf_Type), intent(inout)                     ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    SectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()
    
    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'distribution'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call BaseDistProb_Factory( Object=This%DistProb, Input=InputSection, Prefix=PrefixLoc )

    This%DistTLeft = This%DistProb%IsTruncatedLeft()
    This%DistTRIght = This%DistProb%IsTruncatedRight()

    if ( This%DistTLeft ) This%DistA = This%DistProb%GetA()
    if ( This%DistTRight ) This%DistB = This%DistProb%GetB()

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, Distribution, Debug )

    class(DistInfBoundTransf_Type), intent(inout)                     ::    This
    class(DistProb_Type), intent(in)                                  ::    Distribution
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    i
    type(DistNorm_Type)                                               ::    DistNorm
    real(rkp)                                                         ::    SampleMin
    real(rkp)                                                         ::    SampleMax
    real(rkp), allocatable, dimension(:)                              ::    SamplesLoc
    integer                                                           ::    NbSamples

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    allocate(This%DistProb, source=Distribution, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%DistProb', ProcName=ProcName, stat=StatLoc )

    This%DistTLeft = This%DistProb%IsTruncatedLeft()
    This%DistTRIght = This%DistProb%IsTruncatedRight()

    if ( This%DistTLeft ) This%DistA = This%DistProb%GetA()
    if ( This%DistTRight ) This%DistB = This%DistProb%GetB()

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )
    
    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/distribution'

    SectionName = 'distribution'
    call GetInput%AddSection( Section=BaseDistProb_Factory%GetObjectInput( Object=This%DistProb, MainSectionName=SectionName,     &
                                                                                      Prefix=PrefixLoc, Directory=DirectorySub ) )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D( This, X, Debug )

    real(rkp)                                                         ::    PDF_R0D

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='PDF_R0D'
    real(rkp)                                                         ::    XLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    XLoc = X
    call This%Transform( Value=XLoc )

    PDF_R0D = This%DistProb%PDF( X=XLoc )

    call This%fInvTransform( Value=PDF_R0D, X=XLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF_R0D( This, X, Debug )

    real(rkp)                                                         ::    CDF_R0D

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='CDF_R0D'
    real(rkp)                                                         ::    XLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    XLoc = X
    call This%Transform( Value=XLoc )

    call This%DistProb%CDF( X=XLoc )
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF_R0D( This, P, Debug )

    real(rkp)                                                         ::    InvCDF_R0D

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), intent(in)                                             ::    P
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='InvCDF_R0D'
    real(rkp)                                                         ::    XLoc
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    InvCDF_R0D = This%DistProb%InvCDF( P=P )
    call This%InvTransform( Value=InvCDF_R0D )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_0D( This, Value, Debug )

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), intent(inout)                                          ::    Value
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Transform_0D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%DistTLeft .and. This%DistTRight ) then
      Value = ( This%DistB*dexp(Value)+This%DistA ) / ( One+dexp(Value) )
    elseif ( This%DistTLeft ) then
      Value = dexp(Value) + This%DistA
    elseif ( This%DistTRight ) then
      Value = This%DistB - One / dexp(Value)
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform_1D( This, Values, Debug )

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), dimension(:), intent(inout)                            ::    Values
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Transform_1D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%DistTLeft .and. This%DistTRight ) then
      Value = ( This%DistB*dexp(Values)+This%DistA ) / ( One+dexp(Values) )
    elseif ( This%DistTLeft ) then
      Value = dexp(Values) + This%DistA
    elseif ( This%DistTRight ) then
      Value = This%DistB - One / dexp(Values)
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine InvTransform_0D( This, Value, Debug )

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), intent(inout)                                          ::    Value
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='InvTransform_0D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%DistTLeft .and. This%DistTRight ) then
      Value = dlog((Value-This%DistA)/(This%DistB-Value))
    elseif ( This%DistTLeft ) then
      Value = dlog((Value-This%DistA))
    elseif ( This%DistTRight ) then
      Value = dlog(One/(This%DistB-Value))
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine InvTransform_1D( This, Values, Debug )

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), dimension(:), intent(inout)                            ::    Values
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='InvTransform_1D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%DistTLeft .and. This%DistTRight ) then
      Value = dlog((Values-This%DistA)/(This%DistB-Values))
    elseif ( This%DistTLeft ) then
      Value = dlog((Values-This%DistA))
    elseif ( This%DistTRight ) then
      Value = dlog(One/(This%DistB-Values))
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine fInvTransform_0D( This, Value, X, Debug )

    class(DistInfBoundTransf_Type), intent(in)                        ::    This
    real(rkp), intent(inout)                                          ::    Value
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='fInvTransform_0D'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%DistTLeft .and. This%DistTRight ) then
      Value = Value * dabs(((X-This%DistA)*(This%DistB-X))/(This%DistB-This%DistA))
    elseif ( This%DistTLeft ) then
      Value = Value * dabs(X-This%DistA)
    elseif ( This%DistTRight ) then
      Value = Value * dabs(-(This%DistB-X))
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(DistInfBoundTransf_Type), intent(out)                       ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (DistInfBoundTransf_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          allocate(LHS%DistProb, source=RHS%DistProb, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%DistProb', ProcName=ProcName, stat=StatLoc )
          LHS%DistTLeft = RHS%DistTLeft
          LHS%DistTRight = RHS%DistTRight
          LHS%DistA = RHS%DistA
          LHS%DistB = RHS%DistB
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(DistInfBoundTransf_Type), intent(inout)                      ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%DistProb) ) deallocate(This%DistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%DistProb', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
