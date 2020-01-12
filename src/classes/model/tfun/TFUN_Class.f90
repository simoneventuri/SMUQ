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

module TFUN_Class

use Input_Library
use Parameters_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use TestFunction_Class                                            ,only:    TestFunction_Type
use TestFunctionContainer_Class                                   ,only:    TestFunctionContainer_Type
use TestFunction_Factory_Class                                    ,only:    TestFunction_Factory
use ModelInternal_Class                                           ,only:    ModelInternal_Type
use Model_Class                                                   ,only:    Model_Type
use Output_Class                                                  ,only:    Output_Type
use Input_Class                                                   ,only:    Input_Type

implicit none

private

public                                                                ::    TFUN_Type

type, extends(ModelInternal_Type)                                     ::    TFUN_Type
  class(TestFunctionContainer_Type), allocatable, dimension(:)        ::    TestFunctions
  integer                                                             ::    NbFunctions
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run_0D
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(TFUN_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'tfunmodel'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(TFUN_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    This%Initialized = .false.
    This%Constructed = .false.

    if ( allocated(This%TestFunctions) ) deallocate(This%TestFunctions, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TestFunctions', ProcName=ProcName, stat=StatLoc )
    This%NbFunctions = 0

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(TFUN_Type), intent(inout)                                   ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%Label = 'tfun'
    This%NbOutputs = 0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    use String_Library

    class(TFUN_Type), intent(inout)                                   ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    i
    integer                                                           ::    ii
    class(TestFunction_Type), allocatable                             ::    TestFunction
    type(String_Type), allocatable, dimension(:)                      ::    Labels

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'label'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Label = VarC0D

    This%NbFunctions = Input%GetNumberofSubSections()

    allocate(This%TestFunctions(This%NbFunctions), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TestFunctions', ProcName=ProcName, stat=StatLoc )

    allocate(Labels(This%NbFunctions), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Labels', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbFunctions
      SectionName = 'function' // ConvertToString(Value=i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call TestFunction_Factory%Construct( Object=TestFunction, Input=InputSection, Prefix=PrefixLoc )
      nullify(InputSection)
      call This%TestFunctions(i)%Set( Object=TestFunction )
      Labels(i) = TestFunction%GetLabel()
      deallocate(TestFunction, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='TestFunction', ProcName=ProcName, stat=StatLoc )
    end do

    i = 1
    do i = 1, This%NbFunctions
      ii = i
      do ii = i, This%NbFunctions
        if ( i == ii ) cycle
        if ( Labels(i)%GetValue() == Labels(ii)%GetValue() ) call Error%Raise( 'Detected duplicate output label : ' //            &
                                                                                         Labels(i)%GetValue(), ProcName=ProcName )
      end do
    end do

    This%NbOutputs = This%NbFunctions

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use String_Library

    type(InputSection_Type)                                           ::    GetInput

    class(TFUN_Type), intent(in)                                      ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    character(:), allocatable                                         ::    SectionName
    logical                                                           ::    ExternalFlag=.false.
    class(TestFunction_Type), pointer                                 ::    TestFunctionPtr=>null()
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='label', Value=This%Label )

    i = 1
    do i = 1, This%NbFunctions
      SectionName = 'function' // ConvertToString(Value=i)
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/function' // ConvertToString(Value=i)
      TestFunctionPtr => This%TestFunctions(i)%GetPointer()
      call GetInput%AddSection( Section=TestFunction_Factory%GetObjectInput(Object=TestFunctionPtr, MainSectionName=SectionName,  &
                                                                                       Prefix=PrefixLoc, Directory=DirectorySub) )
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run_0D( This, Input, Output, Stat )

    class(TFUN_Type), intent(inout)                                   ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    integer, optional, intent(out)                                    ::    Stat

    character(*), parameter                                           ::    ProcName='Run_0D'
    integer                                                           ::    StatLoc=0
    class(TestFunction_Type), pointer                                 ::    TestFunctionPtr=>null()
    integer                                                           ::    i

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( size(Output,1) /= This%NbOutputs ) call Error%Raise( 'Passed down an output array of incorrect length',                  &
                                                                                                               ProcName=ProcName )

    i = 1
    do i = 1, This%NbFunctions
      TestFunctionPtr => This%TestFunctions(i)%GetPointer()
      call TestFunctionPtr%Run( Input=Input, Output=Output(i) )
    end do

    if ( present(Stat) ) Stat = 0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(TFUN_Type), intent(out)                                     ::    LHS
    class(Model_Type), intent(in)                                     ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (TFUN_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%NbFunctions = RHS%NbFunctions
          allocate(LHS%TestFunctions, source=RHS%TestFunctions, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%TestFunctions', ProcName=ProcName, stat=StatLoc )
          LHS%NbOutputs = RHS%NbOutputs
          LHS%Label = RHS%Label
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
