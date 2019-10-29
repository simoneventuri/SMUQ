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

module OFileReader_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Output_Class                                                  ,only:    Output_Type
use OFileFormated_Class                                           ,only:    OFileFormated_Type
use OFileFormated_Vec_Class                                       ,only:    OFileFormated_Vec_Type
use OFileFormated_Factory_Class                                   ,only:    OFileFormated_Factory

implicit none

private

public                                                                ::    OFileReader_Type

type                                                                  ::    OFileReader_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbOFiles
  type(OFileFormated_Vec_Type), allocatable, dimension(:)             ::    OFile
  integer                                                             ::    NbOutputs
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    ReadOutput
  procedure, public                                                   ::    GetNbOutputs
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(OFileReader_Type), intent(inout)                            ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'ofilereader'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(OFileReader_Type), intent(inout)                            ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%OFile) ) deallocate(This%OFile, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OFile', ProcName=ProcName, stat=StatLoc )

    This%NbOFiles = 0
    This%NbOutputs = 0

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(OFileReader_Type), intent(inout)                            ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    use StringRoutines_Module
    use String_Library

    class(OFileReader_Type), intent(inout)                            ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(OFileFormated_Type), allocatable                            ::    OFile
    class(OFileFormated_Type), pointer                                ::    OFilePointer=>null()
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    integer                                                           ::    NbOutputs
    integer                                                           ::    i, ii
    type(String_Type), allocatable, dimension(:)                      ::    OutputLabels
    character(:), allocatable                                         ::    LabelLoc


    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'files'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbOFiles = InputSection%GetNumberofSubSections()

    allocate(This%OFile(This%NbOFiles), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%OFile', ProcName=ProcName, stat=StatLoc )
    
    This%NbOutputs = 0
    i = 1
    do i = 1, This%NbOFiles
      SubSectionName = SectionName // '>file' // ConvertToString(i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call OFileFOrmated_Factory%Construct( Object=OFile, Input=InputSection, Prefix=PrefixLoc )
      call This%OFile(i)%Set( Object=OFile ) 
      This%NbOutputs = This%NbOutputs + OFile%GetNbOutputs()
      deallocate(OFile, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='OFile', ProcName=ProcName, stat=StatLoc )
      nullify(InputSection)
    end do

    allocate(OutputLabels(This%NbOutputs), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='OutputLabels', ProcName=ProcName, stat=StatLoc )
    ii = 0
    i = 1
    do i = 1, This%NbOFiles
      OFilePointer => This%OFile(i)%GetPointer()
      if ( OFilePointer%GetNbOutputs() <= 0 ) cycle
      OutputLabels(ii+1:ii+OFilePointer%GetNbOutputs()) = OFilePointer%GetOutputLabels()
      ii = ii+OFilePointer%GetNbOutputs()
    end do

    i = 1
    ii = 1
    do i = 1, This%NbOutputs-1
      LabelLoc = OutputLabels(i)%GetValue()
      ii = i
      do ii = i+1,This%NbOutputs
        if ( LabelLoc == OutputLabels(ii)%GetValue() ) call Error%Raise( Line='Duplicate labels detected', ProcName=ProcName )
      end do
    end do

    deallocate(OutputLabels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='OutputLabels', ProcName=ProcName, stat=StatLoc )

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(OFileReader_Type), intent(in)                               ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    class(OFileFormated_Type), pointer                                ::    OFile=>null()
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )


    SectionName = 'files'
    call GetInput%AddSection( SectionName=SectionName )
    
    i = 1
    do i = 1, This%NbOFiles
      SubSectionName = 'file' // ConvertToString(Value=i)
      OFile => This%OFile(i)%GetPointer()
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/file' // ConvertToString(Value=i)
      call GetInput%AddSection( Section=OFileFormated_Factory%GetObjectInput( MainSectionName=SubSectionName, Object=OFile,       &
                                                           Prefix=PrefixLoc, Directory=DirectorySub ), To_SubSection=SectionName )
      nullify(OFile)
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ReadOutput( This, Output, Debug )

    class(OFileReader_Type), intent(inout)                            ::    This
    type(Output_Type), dimension(:), intent(inout)                    ::    Output
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ReadOutput'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i, ii
    class(OFileFormated_Type), pointer                                ::    OFile=>null()
    integer                                                           ::    NbOutputsLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( size(Output,1) /= This%NbOutputs ) call Error%Raise( Line='Passed down incorrect size Output array', ProcName=ProcName )

    ii = 0
    i = 1
    do i = 1, This%NbOFiles
      OFile => This%OFile(i)%GetPointer()
      NbOutputsLoc = OFile%GetNbOutputs()
      if ( NbOutputsLoc <= 0 ) cycle
      call OFile%GetOutput( Output=Output(ii+1:ii+NbOutputsLoc) )
      ii = ii + NbOutputsLoc
      nullify(OFile)
    end do

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbOutputs( This, Debug )

    integer                                                           ::    GetNbOutputs

    class(OFileReader_Type), intent(in)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetNbOutputs'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbOutputs = This%NbOutputs

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(OFileReader_Type), intent(out)                              ::    LHS
    class(OFileReader_Type), intent(in)                               ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (OFileReader_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%NbOutputs = RHS%NbOutputs
          LHS%NbOFiles = RHS%NbOFiles
          allocate(LHS%OFile, source=RHS%OFile, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%OFile', ProcName=ProcName, stat=StatLoc )

          i = 1
          do i = 1, LHS%NbOFiles
            call LHS%OFile(i)%Set( RHS%OFile(i)%GetPointer() )
          end do
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(OFileReader_Type),intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%OFile) ) deallocate(This%OFile, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%OFile', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
