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

module ParameterWriter_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MFileInput_Class                                              ,only:    MFileInput_Type
use MFileInputContainer_Class                                     ,only:    MFileInputContainer_Type
use MFileInput_Factory_Class                                      ,only:    MFileInput_Factory
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use InputDet_Class                                                ,only:    InputDet_Type
use String_Library

implicit none

private

public                                                                ::    ParameterWriter_Type

type                                                                  ::    ParameterWriter_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  integer                                                             ::    NbFiles
  type(SMUQFile_Type), allocatable, dimension(:)                      ::    ModelFile
  type(MFileInputContainer_Type), allocatable, dimension(:)           ::    FileProcessor
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    WriteInput
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(ParameterWriter_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'ParameterWriter'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(ParameterWriter_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%FileProcessor) ) deallocate(This%FileProcessor, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FileProcessor', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ModelFile) ) deallocate(This%ModelFile, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ModelFile', ProcName=ProcName, stat=StatLoc )

    This%NbFiles = 0

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(ParameterWriter_Type), intent(inout)                        ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    use String_Library
    use StringRoutines_Module
    class(ParameterWriter_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(MFileInput_Type), allocatable                               ::    FileProcessor
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    integer                                                           ::    i
    

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix
    
    SectionName = 'files'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbFiles = InputSection%GetNumberofSubSections()

    allocate(This%ModelFile(This%NbFiles), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ModelFile', ProcName=ProcName, stat=StatLoc )

    allocate(This%FileProcessor(This%NbFiles), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%FileProcessor', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbFiles
      SectionName = 'files>file' // ConvertToString(Value=i)
      SubSectionName = SectionName // '>file'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call This%ModelFile(i)%Construct(Input=InputSection, Prefix=PrefixLoc)
      nullify(InputSection)

      SubSectionName = SectionName // '>format'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call MFileInput_Factory%Construct( Object=FileProcessor, Input=InputSection, Prefix=PrefixLoc )
      call This%FileProcessor(i)%Set( Object=FileProcessor )
      deallocate(FileProcessor, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='FileProcessor', ProcName=ProcName, stat=StatLoc )
    end do

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(ParameterWriter_Type), intent(in)                           ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    class(MFileInput_Type), pointer                                   ::    FileProcessor=>null()
    integer                                                           ::    i                        


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
    do i = 1, This%NbFiles
      call GetInput%AddSection( SectionName='file' // ConvertToString(Value=i), To_SubSection='files' )

      SectionName = 'files>file' // ConvertToString(Value=i)

      SubSectionName = 'file'
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/file' // ConvertToString(Value=i) // '_file'
      call GetInput%AddSection( Section=This%ModelFile(i)%GetInput(MainSectionName=SubSectionName, Prefix=PrefixLoc,              &
                                                                              Directory=DirectorySub), To_SubSection=SectionName )

      FileProcessor => This%FileProcessor(i)%GetPointer()
      SubSectionName = 'format'
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/file' // ConvertToString(Value=i) // '_format'
      call GetInput%AddSection( Section=MFileInput_Factory%GetObjectInput( Object=FileProcessor,MainSectionName=SubSectionName,   &
                                                           Prefix=PrefixLoc, Directory=DirectorySub ), To_SubSection=SectionName )

      nullify(FileProcessor)
    end do

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteInput( This, Input )

    class(ParameterWriter_Type), intent(inout)                        ::    This
    type(InputDet_Type), intent(in)                                   ::    Input

    character(*), parameter                                           ::    ProcName='WriteInput'
    integer                                                           ::    StatLoc=0
    class(MFileInput_Type), pointer                                   ::    FileProcessor=>null()
    integer                                                           ::    i
    type(String_Type), allocatable, dimension(:)                      ::    Strings

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    i = 1
    do i = 1, This%NbFiles
      FileProcessor => This%FileProcessor(i)%GetPointer()
      call FileProcessor%WriteInput( Input=Input, Strings=Strings )
      call This%ModelFile(i)%Export( Strings=Strings )
      deallocate(Strings, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='Strings', ProcName=ProcName, stat=StatLoc )
    end do

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(ParameterWriter_Type), intent(out)                          ::    LHS
    class(ParameterWriter_Type), intent(in)                           ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    select type (RHS)
  
      type is (ParameterWriter_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%NbFiles = RHS%NbFiles
          allocate(LHS%ModelFile, source=RHS%ModelFile, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%ModelFile', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%FileProcessor, source=RHS%FileProcessor, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%FileProcesso', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(ParameterWriter_Type),intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%FileProcessor) ) deallocate(This%FileProcessor, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%FileProcessor', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ModelFile) ) deallocate(This%ModelFile, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ModelFile', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
