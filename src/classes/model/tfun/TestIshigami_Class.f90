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

module TestIshigami_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use TestFunction_Class                                            ,only:    TestFunction_Type
use Output_Class                                                  ,only:    Output_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type

implicit none

private

public                                                                ::    TestIshigami_Type

type, extends(TestFunction_Type)                                      ::    TestIshigami_Type
  real(rkp)                                                           ::    A
  real(rkp)                                                           ::    B
  real(rkp)                                                           ::    C
  real(rkp)                                                           ::    X1
  real(rkp)                                                           ::    X2
  real(rkp)                                                           ::    X3
  character(:), allocatable                                           ::    X1_Dependency
  character(:), allocatable                                           ::    X2_Dependency
  character(:), allocatable                                           ::    X3_Dependency

contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, nopass, public                                           ::    ComputeIshigami
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(TestIshigami_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'ishigami'
      This%Initialized = .true.
    end if

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(TestIshigami_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(TestIshigami_Type), intent(inout)                           ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'
    integer                                                           ::    StatLoc=0

    This%A = One
    This%B = Seven
    This%C = 0.1_rkp
    This%X1 = One
    This%X2 = One
    This%X3 = One
    This%X1_Dependency = ''
    This%X2_Dependency = ''
    This%X3_Dependency = ''
    This%Label = 'ishigami'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    class(TestIshigami_Type), intent(inout)                           ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    i
    logical                                                           ::    MandatoryLoc

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'label'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.true. )
    This%Label = VarC0D

    ParameterName = 'a'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%A = VarR0D

    ParameterName = 'b'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%B = VarR0D

    ParameterName = 'c'
    call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%C = VarR0D

    SectionName = 'parameters'

    ParameterName = 'x1_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) This%X1_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'x1'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%X1 = VarR0D

    ParameterName = 'x2_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) This%X2_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'x2'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%X2 = VarR0D

    ParameterName = 'x3_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
    if ( Found ) This%X3_Dependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'x3'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%X3 = VarR0D

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(TestIshigami_Type), intent(in)                              ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='a', Value=ConvertToString(Value=This%A) )
    call GetInput%AddParameter( Name='b', Value=ConvertToString(Value=This%B) )
    call GetInput%AddParameter( Name='c', Value=ConvertToString(Value=This%C) )
    call GetInput%AddParameter( Name='label', Value=This%Label )

    SectionName='parameters'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%AddParameter( Name='x1', Value=ConvertToString(Value=This%X1), SectionName=SectionName )
    call GetInput%AddParameter( Name='x2', Value=ConvertToString(This%X2), SectionName=SectionName )
    call GetInput%AddParameter( Name='x3', Value=ConvertToString(This%X3), SectionName=SectionName )
    if ( len_trim(This%X1_Dependency) /= 0 ) call GetInput%AddParameter( Name='x1_dependency', Value=This%X1_Dependency,          &
                                                                                                         SectionName=SectionName )
    if ( len_trim(This%X2_Dependency) /= 0 ) call GetInput%AddParameter( Name='x2_dependency', Value=This%X2_Dependency,          &
                                                                                                         SectionName=SectionName )
    if ( len_trim(This%X3_Dependency) /= 0 ) call GetInput%AddParameter( Name='x3_dependency', Value=This%X3_Dependency,          &
                                                                                                         SectionName=SectionName )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run( This, Input, Output )

    class(TestIshigami_Type), intent(inout)                           ::    This
    type(Input_Type), intent(in)                                      ::    Input
    type(Output_Type), intent(inout)                                  ::    Output

    character(*), parameter                                           ::    ProcName='ProcessInput'
    real(rkp), allocatable, dimension(:,:)                            ::    Ordinate
    real(rkp)                                                         ::    X1
    real(rkp)                                                         ::    X2
    real(rkp)                                                         ::    X3
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    VarC0D

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    allocate(Ordinate(1,1), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
    if ( len_trim(This%X1_Dependency) /= 0 ) then
      call Input%GetValue( Value=X1, Label=This%X1_Dependency )
    else
      X1 = This%X1
    end if
    if ( len_trim(This%X2_Dependency) /= 0 ) then
      call Input%GetValue( Value=X2, Label=This%X2_Dependency )
    else
      X2 = This%X2
    end if
    if ( len_trim(This%X3_Dependency) /= 0 ) then
      call Input%GetValue( Value=X3, Label=This%X3_Dependency )
    else
      X3 = This%X3
    end if
    Ordinate(1,1) = This%ComputeIshigami( This%A, This%B, This%C, X1, X2, X3 )

    call Output%Construct( Values=Ordinate, Label=This%Label)

    deallocate(Ordinate, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeIshigami( A, B, C, X1, X2, X3 )

    real(rkp)                                                         ::    ComputeIshigami

    real(rkp), intent(in)                                             ::    A
    real(rkp), intent(in)                                             ::    B
    real(rkp), intent(in)                                             ::    C
    real(rkp), intent(in)                                             ::    X1
    real(rkp), intent(in)                                             ::    X2
    real(rkp), intent(in)                                             ::    X3

    character(*), parameter                                           ::    ProcName='ComputeIshigami'

    ComputeIshigami = A*dsin(X1) + B*(dsin(X2))**2 + C*(X3**4)*dsin(X1)

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(TestIshigami_Type), intent(out)                             ::    LHS
    class(TestFunction_Type), intent(in)                              ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
      type is (TestIshigami_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if ( RHS%Constructed ) then
          LHS%Label = RHS%Label
          LHS%A = RHS%A
          LHS%B = RHS%B
          LHS%C = RHS%C
          LHS%X1 = RHS%X1
          LHS%X2 = RHS%X2
          LHS%X3 = RHS%X3
          LHS%X1_Dependency = RHS%X1_Dependency
          LHS%X2_Dependency = RHS%X2_Dependency
          LHS%X3_Dependency = RHS%X3_Dependency
        end if
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )
    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(TestIshigami_Type), intent(inout)                            ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
