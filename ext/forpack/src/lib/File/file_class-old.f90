!<==============================================================================================================
!       ____                   _          |                        |
!      / ___| _ __   __ _ _ __| | __      |  Simulation (for)      |  IST - IPFN
!      \___ \| '_ \ / _' | '__| |/ /      |  Plateform             |  Instituto Superior Técnico
!       ___) | |_) | (_| | |  |   <       |  Aerothermodynamics    |  Instituto de Plasmas e Fusão Nuclear
!      |____/| .__/ \__,_|_|  |_|\_\      |  Radiation (and)       |  Av. Rovisco Pais, 1049-001, Lisboa
!            |_|                          |  Kinetics              |  GPS: N 38 44 12.6, W 9 8 21.5
!                                         |                        |
!<==============================================================================================================
!> @brief       Class definition for the file derived-type structure
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        01/01/12 - Bruno LOPEZ - Module creation
!> @date        08/03/12 - Bruno LOPEZ - Removing abstract attribute for type direct usage (see level_Class.f90)
!<==============================================================================================================
!> @details
!! This module stores the class definition for the file derived-type structure. \n
!> @todo        The arguments to Err_GetValue procedure are not always correct or as detailed as they could be
!> @todo        Finish doxygen comments
!<==============================================================================================================
Module File_Class

! Dependency:
! universal_Class.f90   =>      Universal_Class:Universal_Type
! kind_parameters.f90   =>      Kind_Parameters:rkp

  use Universal_Class           ,only:  Universal_Type
  use Logger_Class              ,only:  Logger
  use Error_Class               ,only:  Error
  use Parameters_Module         ,only:  rkp
  use, intrinsic :: iso_fortran_env ,only:  IOStat_End

  implicit none

! @COMPILER_BUG: End-Of-File on a coarray image greater than one return iostat=781 instead of iostat=-1
! When using coarray, if an EOF is encountered by an image greater than the 1st one during a read statement with the iostat argument procvided,
! then the value stored in the iostat variable is 781 instead of 1.
! A simple workaround is to treats the 781 value as an EOF status using the following instauction:
! this is a workaround for a compiler bug when using coarrays
!   if ( ios == IOStat_781 ) exit
  integer       ,parameter      ::  IOStat_781=781

  private

  public  ::  File_Type
  public  ::  Comment_Character

  Type  ,extends(Universal_Type)                ::  File_Type
    logical                                     ::  i_Mandatory     =       .false.                         !< Mandatory indicator of the input type
    logical                                     ::  i_RmComment     =       .false.                         !< Comment-line removing indicator
    logical                                     ::  i_RmEmptyLine   =       .false.                         !< Empty-line removing indicator
    logical                                     ::  i_RmBlanks      =       .false.                         !< Blank-character removing indicator
    logical                                     ::  i_UpperCase     =       .false.                         !< Lower-to-Upper cast conversion indicator
    logical                                     ::  i_LowerCase     =       .false.                         !< Upper-to-Lower cast conversion indicator
    logical                                     ::  i_Tab2Space     =       .false.                         !< Tab-to-Space conversion indicator
    logical                                     ::  i_WriteStart    =       .false.                         !< Writing starting-point indicator
    logical                                     ::  i_WriteStop     =       .false.                         !< Writing stopping-point indicator
    logical                                     ::  i_ContinuationCharacter    =       .false.                         !<
    character(:)        ,allocatable            ::  OpenStatus                                              !< File open status
    character(:)        ,allocatable            ::  WriteStart                                              !< Writing starting-point character string
    character(:)        ,allocatable            ::  WriteStop                                               !< Writing stopping-point character string
    character(:)        ,allocatable            ::  Section                                                 !< Section name
    character(:)        ,allocatable            ::  Name                                                    !< Name of file
    character(:)        ,allocatable            ::  NewFileName
    character(:)        ,allocatable            ::  ContinuationCharacter
    integer                                     ::  Unit                                                    !< Unit of file
    integer             ,private                ::  IdxSec = 0                                              ! Section line index (Need to be initilise to zero when reading files with no section, for level files for example)
  contains
    procedure                   ,private        ::  GetValue_NoVar
    procedure                   ,private        ::  GetValue_l0D
    procedure                   ,private        ::  GetValue_i0D
    procedure                   ,private        ::  GetValue_r0D
    procedure                   ,private        ::  GetValue_c0D

    procedure                   ,private        ::  GetValue_i1D
    procedure                   ,private        ::  GetValue_c1D

!     procedure                   ,private        ::  GetValue_cr1D
    procedure                   ,private        ::  GetValue_C1_D1
    procedure                   ,private        ::  GetValue_c1D_cr2D
    procedure                   ,private        ::  GetValue_c1D_c1D
    procedure                   ,private        ::  GetValue_ccr2D
    procedure                   ,private        ::  GetValue_i0D_ri1D
    procedure                   ,private        ::  GetValue_c1D_r2D
    procedure                   ,private        ::  GetValue_3r1D
    procedure                   ,private        ::  GetValue_r2D
    procedure                   ,private        ::  GetValue_r1D
!   Public procedures
    procedure ,non_overridable  ,public         ::  Create  =>      Create_File
    procedure                   ,public         ::  Open    =>      Open_File
    procedure ,non_overridable  ,public         ::  Find_Section                                             !< Findng a section
    procedure ,non_overridable  ,public         ::  Section_Found
    procedure ,non_overridable  ,public         ::  Closing
!   Private procedures
    procedure                   ,private        ::  Set_Data
    procedure                   ,private        ::  Opening

    procedure                   ,private        ::  Rewind_Section

    procedure                   ,public         ::  Get_r1D

    generic                     ,public         ::  GetValue       =>      GetValue_NoVar,        &
                                                                                GetValue_l0D,          &
                                                                                GetValue_i0D,          &
                                                                                GetValue_r0D,          &
                                                                                GetValue_c0D,          &
                                                                                GetValue_i1D,          &
                                                                                GetValue_c1D,          &
!                                                                                 GetValue_cr1D,         &
                                                                                GetValue_C1_D1,        &
                                                                                GetValue_c1D_cr2D,     &
                                                                                GetValue_c1D_c1D,      &
                                                                                GetValue_ccr2D,        &
                                                                                GetValue_i0D_ri1D,     &
                                                                                GetValue_3r1D,         &
                                                                                GetValue_c1D_r2D,      &
                                                                                GetValue_r2D,          &
                                                                                GetValue_r1D
  End Type

  character(*)                                              ,parameter  ::  Key_SCRATCH             =       'SCRATCH'
  character(*)                                              ,parameter  ::  OpenStatus_Default      =       Key_SCRATCH
  character(*)                                              ,parameter  ::  Comment_Character       =       "#"
  character(*)                                              ,parameter  ::  key_StartSection        =       '$'                             !< Starting section keyword to be found in the input character string

  contains

Subroutine Create_File( This, FileName, WriteStart, WriteStop,  &
                        i_RmComment, i_RmEmptyLine, i_RmBlanks, &
                        i_UpperCase, i_LowerCase, i_Tab2Space,  &
                        i_Debug, OpenStatus, NewFileName, ContinuationCharacter        )
  class(File_Type)                              ,intent(out)    ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  FileName                                !< File name including relative path and extension
  character(*)                        ,optional ,intent(in)     ::  WriteStart                              !< Writing starting-point character string
  character(*)                        ,optional ,intent(in)     ::  WriteStop                               !< Writing stopping-point character string
  logical                             ,optional ,intent(in)     ::  i_RmComment                             !< Comment-line removing indicator
  logical                             ,optional ,intent(in)     ::  i_RmEmptyLine                           !< Empty-line removing indicator
  logical                             ,optional ,intent(in)     ::  i_RmBlanks                              !< Blank-character removing indicator
  logical                             ,optional ,intent(in)     ::  i_UpperCase                             !< Lower-to-Upper cast conversion indicator
  logical                             ,optional ,intent(in)     ::  i_LowerCase                             !< Upper-to-Lower cast conversion indicator
  logical                             ,optional ,intent(in)     ::  i_Tab2Space                             !< Tab-to-Space conversion indicator
  logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator
  character(*)                        ,optional ,intent(in)     ::  OpenStatus                              !< File open status
  character(*)                        ,optional ,intent(in)     ::  NewFileName
  character(*)                        ,optional ,intent(in)     ::  ContinuationCharacter

  logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator
  character(*)          ,parameter                              ::  ProcName        =       'Create_File'   ! Procedure name

  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )

  if (i_Debug_Loc) call Logger%Write( "Calling This%Set_Data" )
  call This%Set_Data( FileName, WriteStart, WriteStop,          &
                      i_RmComment, i_RmEmptyLine, i_RmBlanks,   &
                      i_UpperCase, i_LowerCase, i_Tab2Space,    &
                      OpenStatus, NewFileName, ContinuationCharacter, i_Debug          )

  if (i_Debug_Loc) call Logger%Write( "Calling This%Opening" )
  call This%Opening

  if (i_Debug_Loc) call Logger%Exiting

End Subroutine

Subroutine Set_Data( This, FileName, WriteStart, WriteStop,     &
                     i_RmComment, i_RmEmptyLine, i_RmBlanks,    &
                     i_UpperCase, i_LowerCase, i_Tab2Space,     &
                     OpenStatus, NewFileName, ContinuationCharacter, i_Debug           )

  class(File_Type)                              ,intent(out)    ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  FileName                                !< File name including relative path and extension
  character(*)                        ,optional ,intent(in)     ::  WriteStart                              !< Writing starting-point character string
  character(*)                        ,optional ,intent(in)     ::  WriteStop                               !< Writing stopping-point character string
  logical                             ,optional ,intent(in)     ::  i_RmComment                             !< Comment-line removing indicator
  logical                             ,optional ,intent(in)     ::  i_RmEmptyLine                           !< Empty-line removing indicator
  logical                             ,optional ,intent(in)     ::  i_RmBlanks                              !< Blank-character removing indicator
  logical                             ,optional ,intent(in)     ::  i_UpperCase                             !< Lower-to-Upper cast conversion indicator
  logical                             ,optional ,intent(in)     ::  i_LowerCase                             !< Upper-to-Lower cast conversion indicator
  logical                             ,optional ,intent(in)     ::  i_Tab2Space                             !< Tab-to-Space conversion indicator
  character(*)                        ,optional ,intent(in)     ::  OpenStatus                              !< File open status
  character(*)                        ,optional ,intent(in)     ::  NewFileName
  character(*)                        ,optional ,intent(in)     ::  ContinuationCharacter
  logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator

  logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator
  character(*)          ,parameter                              ::  ProcName        =       'Set_Data'      ! Procedure name

  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )

  This%Name     =       trim(adjustl(FileName))                                                                 ! Setting the file name

  if ( present(i_Debug) )       This%i_Debug            =       i_Debug                                         ! Setting indicator for debugging
  if ( present(i_RmComment) )   This%i_RmComment        =       i_RmComment                                     ! Setting indicator for comment lines removal
  if ( present(i_RmEmptyLine) ) This%i_RmEmptyLine      =       i_RmEmptyLine                                   ! Setting indicator for empty lines removal
  if ( present(i_RmBlanks) )    This%i_RmBlanks         =       i_RmBlanks                                      ! Setting indicator for blank lines removal
  if ( present(i_UpperCase) )   This%i_UpperCase        =       i_UpperCase                                     ! Setting indicator for upper case conversion
  if ( present(i_LowerCase) )   This%i_LowerCase        =       i_LowerCase                                     ! Setting indicator for lower case conversion
  if ( present(i_Tab2Space) )   This%i_Tab2Space        =       i_Tab2Space                                     ! Setting indicator for tabulation to space conversion

  if ( present(WriteStart) ) then                                                                               ! If present optional argument for optional value
    This%WriteStart     =       WriteStart                                                                      ! Setting variable to optional value
    This%i_WriteStart   =       .true.                                                                          ! Setting local indicator of variable setting
  else                                                                                                          ! If absent optional argument for optional value
    This%WriteStart     =       ''                                                                              ! Setting variable to empty string
  end if                                                                                                        ! End if case on presence of optional argument

  if ( present(WriteStop) ) then                                                                                ! If present optional argument for optional value
    This%WriteStop      =       WriteStop                                                                       ! Setting variable to optional value
    This%i_WriteStop    =       .true.                                                                          ! Setting local indicator of variable setting
  else                                                                                                          ! If absent optional argument for optional value
    This%WriteStop      =       ''                                                                              ! Setting variable to empty string
  end if                                                                                                        ! End if case on presence of optional argument

  if ( present(OpenStatus) ) then                                                                               ! If present optional argument for optional value
    This%OpenStatus     =       OpenStatus                                                                      ! Setting variable to optional value
  else                                                                                                          ! If absent optional argument for optional value
    This%OpenStatus     =       OpenStatus_Default                                                              ! Setting variable to empty string
  end if                                                                                                        ! End if case on presence of optional argument

  if ( present(NewFileName) ) then                                                                               ! If present optional argument for optional value
    This%NewFileName    =       NewFileName
    This%OpenStatus     =       'REPLACE'
  end if

  if ( present(ContinuationCharacter) ) then                                                                               ! If present optional argument for optional value
    This%ContinuationCharacter     =       ContinuationCharacter
    This%i_ContinuationCharacter   =       .true.                                                                          ! Setting local indicator of variable setting
  end if                                                                                                        ! End if case on presence of optional argument

  if (This%i_Debug) then
    call Logger%Write( "This%i_RmComment   = ", This%i_RmComment   )
    call Logger%Write( "This%i_RmEmptyLine = ", This%i_RmEmptyLine )
    call Logger%Write( "This%i_RmBlanks    = ", This%i_RmBlanks    )
    call Logger%Write( "This%i_UpperCase   = ", This%i_UpperCase   )
    call Logger%Write( "This%i_LowerCase   = ", This%i_LowerCase   )
    call Logger%Write( "This%i_Tab2Space   = ", This%i_Tab2Space   )
    call Logger%Write( "This%i_WriteStart  = ", This%i_WriteStart  )
    call Logger%Write( "This%i_WriteStop   = ", This%i_WriteStop   )
    call Logger%Write( "This%Name          = ", This%Name          )
    call Logger%Write( "This%WriteStart    = ", This%WriteStart    )
    call Logger%Write( "This%WriteStop     = ", This%WriteStop     )
    call Logger%Write( "This%OpenStatus    = ", This%OpenStatus    )
    call Logger%Write( "This%NewFileName   = ", This%NewFileName   )
    call Logger%Write( "This%NewFileName   = ", This%NewFileName   )
    call Logger%Write( "This%i_ContinuationCharacter = ", This%i_ContinuationCharacter )
    if (This%i_ContinuationCharacter) call Logger%Write( "This%ContinuationCharacter = ", This%ContinuationCharacter )
    call Logger%Exiting
  end if

End Subroutine


!<==============================================================================================================
!> @brief       Closes the file associated with a File derived-type structure
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        08/02/12 - Bruno LOPEZ - Initial creation of procedure
!<==============================================================================================================
!> @details
!! This procedure closes the file associated with a File derived-type structure. \n
!<==============================================================================================================
Subroutine Closing( This )
  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  close(This%Unit)                                                                                              ! Closing the file associated with a passed-object fiel structure
End Subroutine

!<==============================================================================================================
!> @brief       Opens a file from File derived-type structure
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        08/02/12 - Bruno LOPEZ - Initial creation of procedure
!<==============================================================================================================
!> @details
!! This procedure opens a file from File derived-type structure.
!<==============================================================================================================
Subroutine Opening( This, i_Debug )

  use String_Library            ,only:  RemoveLeftChar, UpperCase, LowerCase, Tab2Space, RemoveSpace, Parse

  class(File_Type)                              ,intent(inout)  ::  This                                    !< Passed-object dummy argument
  logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator


  logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator
  character(*)          ,parameter                              ::  ProcName        =       'Opening'       ! Procedure name
  integer                                                       ::  Unit_Old                                ! Old File Unit
  integer                                                       ::  ios                                     ! Input/Output status
  character(len=1000)                                            ::  Line, Line2                                    ! Character string corresponding a file line
  logical                                                       ::  i_Start                                 ! Writing start indicator
  logical                                                       ::  i_Exist                                 ! File existence indicator
  character(*)          ,parameter                              ::  OpenAction      =       'READWRITE'

  integer                                                       ::  iLine                                   ! Line index
  integer                                                       ::  i
!   integer                                                       ::  i                                ! Number of elements in the vector character string
!   character(500)        ,dimension(:)   ,allocatable            ::  LineVec                                 ! Vector of character string corresponding a data specification

  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )

! ==============================================================================================================
!    CHECKING FILE EXISTENCE
! ==============================================================================================================
  inquire( File=This%Name, Exist=i_Exist )                                                                      ! Setting the file existence indicator
  if ( .not.i_Exist ) call Error%Exist_File( File=This%Name, ProcName=ProcName )                                ! If the current FILE DOES NOT EXIST, then error

! ==============================================================================================================
!    OPENING FILES
! ==============================================================================================================
  if (This%i_Debug) call Logger%Write( "Opening old file" )
  open( NewUnit=Unit_Old, File=This%Name, status='OLD', action='READ', iostat=ios )                             ! Opening file for "read only mode"
  if ( ios /= 0 ) call Error%Open( Unit=Unit_Old, File=This%Name, ProcName=ProcName )                      ! Checking for errors during file opening

  if (This%i_Debug) call Logger%Write( "This%OpenStatus = ", This%OpenStatus )
  if ( This%OpenStatus == Key_SCRATCH ) then                                                                    ! If opening in scratch mode
    if (This%i_Debug) call Logger%Write( "Opening new file in SCRATCH mode" )
! ************************************************************************************************************
! @COMPILER_BUG: ifort version 15.0.0:file_Class.f90(298): error #8414: If NEWUNIT appears in OPEN statement either FILE or STATUS (with value SCRATCH) specifiers must appear.
! WORKAROUND:   Use an integer variable directly instead of the object component
    open( NewUnit=This%Unit, Status=Key_SCRATCH, Action=OpenAction, iostat=ios )                            ! Opening file in "read & write" mode
!     open( NewUnit=This%Unit, status=This%OpenStatus, Action=OpenAction, iostat=ios )                            ! Opening file in "read & write" mode
! ************************************************************************************************************
  else                                                                                                          ! if opening not in scratch mode
    if (This%i_Debug) call Logger%Write( "Opening new file in REPLACE mode" )
    open( NewUnit=This%Unit, File=This%NewFileName, Status='REPLACE', Action='READWRITE', iostat=ios )          ! Opening file in "read & write" mode
  end if                                                                                                        ! End i case on open status
  if ( ios /= 0 ) call Error%Open( Unit=This%Unit, ProcName=ProcName )                                     ! If something went wrong when opening file, then raise an error

! ==============================================================================================================
!    READING FILE AND WRITING NEW FILE
! ==============================================================================================================
  iLine         =       0                                                                                       ! Initialisation of line's index
  i_Start       =       .not. This%i_WriteStart                                                                 ! Setting the writing start-point indicator ( false if writing start at a specific character given by WriteStart )
  do                                                                                                            ! Infinit loop for copying data from old to new file

    read( Unit_Old, '(a)', iostat=ios ) Line                                                                    ! Reading a single line from old file
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios == IOStat_781 ) exit
    if ( ios > 0 ) call Error%Read( Unit=Unit_Old, File=This%Name, ProcName=ProcName )                     ! If error during file reading, then raise an error
    iLine       =       iLine + 1                                                                               ! Incrementation of line's index

    if (This%i_Debug) call Logger%Write( "iLine = ", iLine, "Line = ", trim(Line) )

    if (This%i_ContinuationCharacter) then
!       if (This%i_Debug) call Logger%Write( "This%ContinuationCharacter = ", This%ContinuationCharacter )
!       if (This%i_Debug) call Logger%Write( "index(Line,This%ContinuationCharacter) = ", index(Line,This%ContinuationCharacter) )
    if ( index(Line,This%ContinuationCharacter) /= 0 ) then                                                     ! If current line contains a continuation character
      do                                                                                                        ! Loop on the next lines to add them t current line
        i       =       index(Line,This%ContinuationCharacter)                                                  ! Getting the position of the continuation character, if any
        if ( i == 0 ) exit                                                                                      ! If no continuation character in current line,thenexit the loop
        if (This%i_Debug) call Logger%Write( "  i = ", i, NewLine=.true. )
        Line    =       Line(1:i-1)                                                                             ! Removing the continuation character from the line
        if (This%i_Debug) call Logger%Write( " Line = ", trim(Line) )
        read(Unit_Old,'(a)',iostat=ios) Line2                                                                   ! Reading a single line from old file
        if ( ios == IOStat_End .or. ios == IOStat_781) exit                                                     ! If end-of file while reading, then exiting the loop
        if ( ios > 0 ) call Error%Read( Unit=Unit_Old, File=This%Name, ProcName=ProcName )                 ! If error during file reading, then error
        iLine           =       iLine + 1                                                                           ! Incrementation of line's index
        Line2           =       adjustl(Line2)

        if (This%i_Debug) call Logger%Write( "  iLine = ", iLine, "Line2 = ", trim(Line2) )
        Line    =       trim(Line) // trim(adjustl(Line2))                                                                          ! Adding the line to the previous one
        if (This%i_Debug) call Logger%Write( "  iLine = ", iLine, "Line = ", trim(Line) )
      end do
    end if                                                                                                      ! End if case on the presence of the continuation character in current line
    end if


    if (This%i_RmComment)      Line     =       RemoveLeftChar( Line, Comment_Character ) !, i_Debug=This%i_Debug )           ! Removing comments if required (actually, all characters after the comment character are removed)
    if (This%i_UpperCase)      Line     =       UpperCase(Line)                                                 ! Converting to upper case if required
    if (This%i_LowerCase)      Line     =       LowerCase(Line)                                                 ! Converting to lower case if required
    if (This%i_Tab2Space)      Line     =       Tab2Space(Line)                                                 ! Converting tabulation to space if required
    if (This%i_RmBlanks)       Line     =       RemoveSpace(Line)                                               ! Removing space if required

    if (This%i_RmEmptyLine .and. (len_trim(Line)==0))   cycle                                                   ! If empty-lines need to be removed and if the current line is empty, then going to the next line
    if (This%i_WriteStop   .and. (trim(adjustl(Line))==trim(This%WriteStop)) )    exit                          ! Exiting the reading loop if the ending line has been meet
    if (i_Start) then                                                                                           ! If the writing process has started
      if (This%i_Debug) call Logger%Write( "Line  = ", trim(Line) )
      write(This%Unit,"(a)")  trim(Line)
!       call Parse( Line, ',', LineVec )                                                                   ! Parses the line into arguments LineVec based on the delimiters contained in the string ','
!       do i = 1,size(LineVec,1)                                                                                            ! Loop on all element of the string vector
!         write(This%Unit,"(a)")  trim(LineVec(i))                                                                ! Writing the current line into the new file
!       end do                                                                                                    ! End of loop
    end if                                                                                                      ! End if on writing process
    if ( This%i_WriteStart .and. (trim(adjustl(Line))==trim(This%WriteStart)) ) i_Start = .true.                ! Setting the starting indicator
  end do                                                                                                        ! End of infinit loop

  close(Unit_Old)                                                                                               ! Closing old file
  rewind(This%Unit)                                                                                             ! Rewinding new file


  if ( This%OpenStatus /= Key_SCRATCH ) then                                                                    ! If opening in scratch mode
  ! <COARRAYS>: Here, we close the new file and we re-open it again with "action=READ" so that it is read only
  ! and there is no problem for several images to read it. If the file is open with "action=WRITE/READWRITE"
  ! it is not safe.
    close(This%Unit)
    open( NewUnit=This%Unit, File=This%NewFileName, Status='OLD', Action='READ', iostat=ios )                     ! Opening file
    if ( ios /= 0 ) call Error%Open( Unit=This%Unit, ProcName=ProcName )                                     ! If something went wrong when opening file, then raise an error
  end if

  if (i_Debug_Loc) call Logger%Exiting

End Subroutine

Subroutine Open_File( This, FileName, OpenStatus, i_Debug )

  class(File_Type)                              ,intent(inout)  ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  FileName                                !< File name including relative path and extension
  character(*)                        ,optional ,intent(in)     ::  OpenStatus                              !< File open status
  logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator

  logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator
  character(*)          ,parameter                              ::  ProcName        =       'Open_File'     ! Procedure name
  logical                                                       ::  i_Exist                                 ! File existence indicator
  integer                                                       ::  ios                                     ! Input/Output status

  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )

! ==============================================================================================================
!    SETTING OBJECT COMPONENTS
! ==============================================================================================================
  This%Name     =       trim(adjustl(FileName))                                                                 ! Setting the file name


! ==============================================================================================================
!    CHECKING FILE EXISTENCE
! ==============================================================================================================
  inquire( File=This%Name, Exist=i_Exist )                                                                      ! Setting the file existence indicator
  if ( .not.i_Exist ) call Error%Exist_File( File=This%Name, ProcName=ProcName )                                ! If the current FILE DOES NOT EXIST, then error

  if (This%i_Debug) call Logger%Write( "Opening file: This%Name = ", This%Name )
  open( NewUnit=This%Unit, File=This%Name, status='OLD', action='READ', iostat=ios )                             ! Opening file for "read only mode"
  if ( ios /= 0 ) call Error%Open( Unit=This%Unit, File=This%Name, ProcName=ProcName )                      ! Checking for errors during file opening

  if (i_Debug_Loc) call Logger%Exiting

End Subroutine

!<==============================================================================================================
!> @brief       Finds a section name in a File derived-type structure
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        08/02/12 - Bruno LOPEZ - Initial creation of procedure
!<==============================================================================================================
!> @details
!! This procedure finds a section name in a File derived-type structure.
!<==============================================================================================================
Subroutine Find_Section( This, Section, i_Mandatory, i_Debug )

  use String_Library            ,only:  UpperCase

  class(File_Type)                              ,intent(inout)  ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  Section                                 !< Section name to be find
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the section is mandatory
  logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator

  logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='Find_Section'                 ! Procedure name
  character(100)                                                ::  Line                                    ! Character string corresponding to a line of the considered file
  integer                                                       ::  ios                                     ! Input/output status indicator
  logical                                                       ::  Section_Found                           ! Indicator that the current section has been found

  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )

  This%Section  =       trim( UpperCase(Section) )                                                              ! Setting section name by converting all lower to upper case
  if (This%i_Debug) call Logger%Write( "This%Section = ", This%Section )

  This%IdxSec   =       0                                                                                       ! Initialisation of current section's line index (Required even if it is initialized in the type definition because of consecutive section reading)
  Section_Found =       .false.                                                                                 ! Initialisation of the section founding indicator
  rewind(This%Unit)                                                                                             ! Rewinding the file to be read
  do                                                                                                            ! Loop on file in order to found the considered section
    read( This%Unit, "(a)", iostat=ios ) Line                                                                   ! Reading the current line of the file
    if ( ios == IOStat_End ) then
      if (This%i_Debug) call Logger%Write( "IOStat_End" )
      if (This%i_Debug) call Logger%Write( "This%IdxSec = ", This%IdxSec, "Line = ", Line )
      exit                                                                               ! If end-of file while reading, then exiting the loop
    end if
    if ( ios == IOStat_781 ) then
      if (This%i_Debug) call Logger%Write( "IOStat_End = IOStat_781" )
      if (This%i_Debug) call Logger%Write( "This%IdxSec = ", This%IdxSec, "Line = ", Line )
      exit                                                                               ! If end-of file while reading, then exiting the loop
    end if
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error

    This%IdxSec =       This%IdxSec + 1                                                                         ! Incrementation of the section line index
    if (This%i_Debug) call Logger%Write( "This%IdxSec = ", This%IdxSec, "Line = ", Line )
    if ( Detect_Keyword(Line,'$'//This%Section) ) then                                                          ! If the considered section has been detected
      Section_Found     =       .true.                                                                          ! Setting the section founding indicator
      exit                                                                                                      ! Exiting the loop
    end if                                                                                                      ! End if case on detected section
  end do                                                                                                        ! End of loop on file lines

  if (This%i_Debug) call Logger%Write( "End of loop" )

  if ( .not. Section_Found ) This%IdxSec = 0                                                                    ! If the section has not been found, then, setting the line index to zero

  if (This%i_Debug) call Logger%Write( "This%IdxSec = ", This%IdxSec )
  if ( present(i_Mandatory) ) then                                                                              ! If the mandatory optional argument is present
    if ( (i_Mandatory) .and. (This%IdxSec==0) ) call Error%Raise( "This section '"//trim(This%Section)//"' is mandatory for the specified simulation parameters" )
  end if                                                                                                        ! End if case on optional argument presence

  if (i_Debug_Loc) call Logger%Exiting

End Subroutine

!<==============================================================================================================
!> @brief       Detects a substring in a input string
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        08/02/12 - Bruno LOPEZ - Initial creation of procedure
!<==============================================================================================================
!> @details
!! This procedure detects a substring in a input string.
!<==============================================================================================================
Pure Function Detect_Keyword( String, Keyword ) result (Indicator)
  character(*)                  ,intent(in)     ::  String                                                  !< Input string in which the keyword has to be detected
  character(*)                  ,intent(in)     ::  Keyword                                                 !< Input string corresponding the keyword to detect in the input string
  logical                                       ::  Indicator                                               !< Output indicator whether or not the keyword has been detected
  Indicator     =       .false.                                                                                 ! Initialisation of the starting-section output indicator
!   if ( trim(String(1:len_trim(Keyword))) == trim(Keyword) ) Indicator = .true.                                  ! If starting-section character detected, the setting the starting-section output indicator
  if ( trim(String) == trim(Keyword) ) Indicator = .true.                                  ! If starting-section character detected, the setting the starting-section output indicator
End Function

!<==============================================================================================================
!> @brief       Detects the starting point of a section
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        08/02/12 - Bruno LOPEZ - Initial creation of procedure
!<==============================================================================================================
!> @details
!! This procedure detects the starting point of a section
!<==============================================================================================================
Pure Function Detect_Start_Section( String ) result (Indicator)
  character(*)                                          ,intent(in)     ::  String                          !< Input string in which the starting section characters has to be detected
  logical                                                               ::  Indicator                       !< Output indicator whether or not the starting-section character has been detected
  Indicator     =       Detect_Keyword( String, key_StartSection )                                              ! Detecting the presence of the starting-section string in the input string
End Function

!<==============================================================================================================
!> @brief       Rewinds a section
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        08/02/12 - Bruno LOPEZ - Initial creation of procedure
!<==============================================================================================================
!> @details
!! This procedure rewinds a section.
!<==============================================================================================================
Subroutine Rewind_Section(This)
  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  integer                                                       ::  iLine                                   ! Line index
  rewind(This%Unit)                                                                                             ! Rewinding the file
  do iLine = 1,This%IdxSec                                                                                      ! Loop on all lines until the considered section
    read(This%Unit,*)                                                                                           ! Positioning the file just after the loaded section
  end do
End Subroutine

Function Section_Found(This)
  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  logical                                                       ::  Section_Found                           !< Indicator whether or not the section has been found
  Section_Found =       This%IdxSec /= 0                                                                        ! Setting the section as found if its line i
End Function


!<==============================================================================================================
!> @brief       Reads the value associated with a real variable
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        08/02/12 - Bruno LOPEZ - Initial creation of procedure
!<==============================================================================================================
!> @details
!! This procedure reads the value associated with a real variable. \n
!! The data proceeded by this procedure has the following format:
!! VARIBALE_NAME = real_number
!<==============================================================================================================
Subroutine GetValue_i0D( This, Par, Var, i_Found, i_Mandatory )

  use String_Library            ,only:  Parse, Convert_To_Integer

  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  Par                                     !< Parameter corresponding to the variable to be assigned
  integer                                       ,intent(inout)  ::  Var                                     !< Variable to be assigned
  logical                             ,optional ,intent(out)    ::  i_Found                                 !< Indicator that the parameter has been found
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the parameter to be found is mandatory
!   logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName='GetValue_i0D'                ! Procedure name
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer       ,parameter                                      ::  i_LHS=1, i_RHS=2                        ! Left/Right-hand side index
  logical                                                       ::  i_Found_Loc                             ! Local indicator that the parameter has been found
  character(1000)                                               ::  Str                                     ! Character string corresponding to a line of the considered section
  character(:)  ,allocatable    ,dimension(:)                   ::  Str_LR                                  ! Character string vector corresponding the LHS and RHS string of the '=' character
!   logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator
!
!   i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator

  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  i_Found_Loc   =       .false.                                                                                 ! Initialisation of the local founding indicator
  do                                                                                                            ! Loop on input data
    read(This%Unit, "(a)", iostat=ios ) Str                                                                     ! Reading the entire line of the considered section
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios == IOStat_781 ) exit
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    if ( Detect_Start_Section(Str) ) exit                                                                       ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
    call Parse( Str, '=', Str_LR )                                                                              ! Cutting the current line ('Str') into two elements separated by the '=' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
    if  (trim(Str_LR(i_LHS)) /= trim(Par))      cycle                                                           ! If the LHS-string and the searched parameter are different, then going to the next line
    Var         =       Convert_To_Integer( Str_LR(i_RHS) )                                                     ! Converting the RHS-string into a number and storing its value in the output variable
    i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value
    exit                                                                                                        ! Exiting the loop
  end do                                                                                                        ! End loop on input data
  if    (present(i_Found))      i_Found =       i_Found_Loc                                                     ! Setting the output founding indicator to local value if present in argument list
  if    (present(i_Mandatory))  then                                                                            ! If mandatory attribute of the parameter to be affected is present in the argument list, then
    if  (i_Mandatory.and.(.not.i_Found_Loc))    then                                                            ! If the parameter to be affected is mandatory and has not been found, then
      call Error%GetValue(This%Section, Par)                                                                   ! Printing an error message and stopping the code
    end if                                                                                                      ! End if case on mandatory and found indicators
  end if                                                                                                        ! Enf if case on presence of optional argument
End Subroutine

!<==============================================================================================================
!> @brief       Reads the value associated with a real variable
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        08/02/12 - Bruno LOPEZ - Initial creation of procedure
!<==============================================================================================================
!> @details
!! This procedure reads the value associated with a real variable. \n
!! The data proceeded by this procedure has the following format:
!! VARIBALE_NAME = real_number
!<==============================================================================================================
Subroutine GetValue_r0D( This, Par, Var, i_Found, i_Mandatory )
  use String_Library            ,only:  Parse, Convert_To_Real
  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  Par                                     !< Parameter corresponding to the variable to be assigned
  real(rkp)                                     ,intent(inout)  ::  Var                                     !< Variable to be assigned
  logical                             ,optional ,intent(out)    ::  i_Found                                 !< Indicator that the parameter has been found
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the parameter to be found is mandatory
  character(*)                                              ,parameter  ::  ProcName='GetValue_r0D'                ! Procedure name
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer       ,parameter                                      ::  i_LHS=1, i_RHS=2                        ! Left/Right-hand side index
  logical                                                       ::  i_Found_Loc                             ! Local indicator that the parameter has been found
  character(1000)                                               ::  Str                                     ! Character string corresponding to a line of the considered section
  character(:)  ,allocatable    ,dimension(:)                   ::  Str_LR                                  ! Character string vector corresponding the LHS and RHS string of the '=' character
  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  i_Found_Loc   =       .false.                                                                                 ! Initialisation of the local founding indicator
  do                                                                                                            ! Loop on input data
    read(This%Unit, "(a)", iostat=ios ) Str                                                                     ! Reading the entire line of the considered section
    if ( ios == IOStat_781 ) exit
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    if ( Detect_Start_Section(Str) ) exit                                                                       ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
    call Parse( Str, '=', Str_LR )                                                                              ! Cutting the current line ('Str') into two elements separated by the '=' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
    if  (trim(Str_LR(i_LHS)) /= trim(Par))      cycle                                                           ! If the LHS-string and the searched parameter are different, then going to the next line
    Var =       Convert_To_Real( Str_LR(i_RHS) )                                                                ! Converting the RHS-string into a number and storing its value in the output variable
    i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value
    exit                                                                                                        ! Exiting the loop
  end do                                                                                                        ! End loop on input data
  if    (present(i_Found))      i_Found =       i_Found_Loc                                                     ! Setting the output founding indicator to local value if present in argument list
  if    (present(i_Mandatory))  then                                                                            ! If mandatory attribute of the parameter to be affected is present in the argument list, then
    if  (i_Mandatory.and.(.not.i_Found_Loc))    then                                                            ! If the parameter to be affected is mandatory and has not been found, then
      call Error%GetValue(This%Section, Par)                                                                   ! Printing an error message and stopping the code
    end if                                                                                                      ! End if case on mandatory and found indicators
  end if                                                                                                        ! Enf if case on presence of optional argument
End Subroutine

!<==============================================================================================================
!> @brief       Reads the value associated with a logical variable
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        08/02/12 - Bruno LOPEZ - Initial creation of procedure
!<==============================================================================================================
!> @details
!! This procedure reads the value associated with a logical variable. \n
!! The data proceeded by this procedure has the following format:
!! VARIBALE_NAME = string \n
!! The \c Par and \c Var arguments are inversed compared to other procedure. \n
!! This is to prevent the following error: \n
!! The type/rank/keyword signature for this specific procedure matches another specific procedure that
! shares the same generic binding name.
!<==============================================================================================================
Subroutine GetValue_l0D( This, Var, Par, i_Found, i_Mandatory )
  use String_Library            ,only:  Parse, Convert_To_Logical
  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  logical                                       ,intent(inout)  ::  Var                                     !< Variable to be assigned
  character(*)                                  ,intent(in)     ::  Par                                     !< Parameter corresponding to the variable to be assigned
  logical                             ,optional ,intent(out)    ::  i_Found                                 !< Indicator that the parameter has been found
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the parameter to be found is mandatory
  character(*)                                              ,parameter  ::  ProcName='GetValue_l0D'                ! Procedure name
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer       ,parameter                                      ::  i_LHS=1, i_RHS=2                        ! Left/Right-hand side index
  logical                                                       ::  i_Found_Loc                             ! Local indicator that the parameter has been found
  character(1000)                                               ::  Str                                     ! Character string corresponding to a line of the considered section
  character(:)  ,allocatable    ,dimension(:)                   ::  Str_LR                                  ! Character string vector corresponding the LHS and RHS string of the '=' character
  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  i_Found_Loc   =       .false.                                                                                 ! Initialisation of the local founding indicator
  do                                                                                                            ! Loop on input data
    read(This%Unit, "(a)", iostat=ios ) Str                                                                     ! Reading the entire line of the considered section
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios == IOStat_781 ) exit
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    if ( Detect_Start_Section(Str) ) exit                                                                       ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
    call Parse( Str, '=', Str_LR )                                                                              ! Cutting the current line ('Str') into two elements separated by the '=' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
    if ( trim(Str_LR(i_LHS)) /= trim(Par) ) cycle                                                               ! If the LHS-string and the searched parameter are different, then going to the next line
    if ( size(Str_LR) == 1 ) then                                                                               ! If no '=' character has been found in the 'Str' string, then
      Var       =       .true.                                                                                  ! Setting to true the logical output variable
    else                                                                                                        ! If at least one '=' character has been found in the 'Str' string, then
      Var       =       Convert_To_Logical( Str_LR(i_RHS) )                                                     ! Converting the RHS-string into a number and storing its value in the output variable
    end if                                                                                                      ! End if case on the number if found '=' character
    i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value
    exit                                                                                                        ! Exiting the loop
  end do                                                                                                        ! End loop on input data
  if    (present(i_Found))      i_Found =       i_Found_Loc                                                     ! Setting the output founding indicator to local value if present in argument list
  if    (present(i_Mandatory))  then                                                                            ! If mandatory attribute of the parameter to be affected is present in the argument list, then
    if  (i_Mandatory.and.(.not.i_Found_Loc))    then                                                            ! If the parameter to be affected is mandatory and has not been found, then
      call Error%GetValue(This%Section, Par)                                                                   ! Printing an error message and stopping the code
    end if                                                                                                      ! End if case on mandatory and found indicators
  end if                                                                                                        ! Enf if case on presence of optional argument
End Subroutine

!<==============================================================================================================
!> @brief       Reads the value associated with a string variable
!> @author      Bruno LOPEZ, blopez@ipfn.ist.utl.pt
!> @date        08/02/12 - Bruno LOPEZ - Initial creation of procedure
!<==============================================================================================================
!> @details
!! This procedure reads the value associated with a given string parameter.
!! The section containing the parameter definition need to be set before calling the current procedure.
!! The file is positioned just after the current section using the Rewind_Section type-bound procedure.
!! Then, the file is read until a end-of-file is encountered or until a nex section if found.
!! The data to be read has the following format "PARAMETER = VALUE", where PARAMETER is the Par input argument
!! and VALUE is the Val output value to be length-allocated and read. Both argument are character variables.
!! The Parse procedure is used to cut the "PARAMETER = VALUE" string in the input file into two sub-string
!! corresponding to the left-hand-side and the right-hand-side strings with respect to the "=" character.
!! If the PARAMETER string has no value or an empty one (ie "PARAMETER = " or "PARAMETER"), then VALUE is
!! set to an empty string.
!! Once the value associted to the input parameter has been found, the reading of the file is stop and
!! the found indicator is set if required.
!<==============================================================================================================
Subroutine GetValue_c0D( This, Par, Var, i_Found, i_Mandatory, i_Debug )

  use String_Library            ,only:  Parse, VecTrim


  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  Par                                     !< Parameter corresponding to the variable to be assigned
  character(:)  ,allocatable                    ,intent(out)    ::  Var                                     !< Variable to be assigned
  logical                             ,optional ,intent(out)    ::  i_Found                                 !< Indicator that the parameter has been found
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the parameter to be found is mandatory
  logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName='GetValue_c0D'                ! Procedure name
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer       ,parameter                                      ::  i_LHS=1, i_RHS=2                        ! Left/Right-hand side index
  logical                                                       ::  i_Found_Loc                             ! Local indicator that the parameter has been found
  character(1000)                                               ::  Str                                     ! Character string corresponding to a line of the considered section
  character(:)  ,allocatable    ,dimension(:)                   ::  Str_LR                                  ! Character string vector corresponding the LHS and RHS string of the '=' character
  integer                                                       ::  iLine                                   ! Index of line
  logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator

  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )

  if (i_Debug_Loc) call Logger%Write( "Par = ", Par )
  if (i_Debug_Loc) call Logger%Write( "Calling This%Rewind_Section" )
  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  i_Found_Loc   =       .false.                                                                                 ! Initialisation of the local founding indicator
  iLine         =       0                                                                                       ! Initialisation of line's index

  if (i_Debug_Loc) call Logger%Write( "Start of reading loop" )
  do                                                                                                            ! Loop on input data

    read(This%Unit, "(a)", iostat=ios ) Str                                                                     ! Reading the entire line of the considered section
    if ( ios == IOStat_781 ) exit
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error

    iLine       =       iLine + 1                                                                               ! Incrementation of line's index
    if (i_Debug_Loc) call Logger%Write( "iLine = ", iLine, "Str = ", Str )
    if ( Detect_Start_Section(Str) ) exit                                                                       ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
    if (i_Debug_Loc) call Logger%Write( "Calling Parse" )
    call Parse( Str, '=', Str_LR )                                                                              ! Cutting the current line ('Str') into two elements separated by the '=' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
    if (i_Debug_Loc) call Logger%Write( "size(Str_LR) = ", size(Str_LR) )
    if (i_Debug_Loc) call Logger%Write( "Str_LR(1) = ", Str_LR(1) )
    if (i_Debug_Loc) call Logger%Write( "Str_LR(2) = ", Str_LR(2) )
    if ( trim(Str_LR(i_LHS)) /= trim(Par) ) cycle                                                               ! If the LHS-string and the searched parameter are different, then going to the next line
    Var       =       ''                                                                                        ! Initialisation of the value to an empty string (required for parameters with no associated values, ie no '=' character in the 'Str')
    if ( size(Str_LR) /= 1 ) Var = trim(Str_LR(i_RHS))                                                                  ! If at least one '=' character has been found in the 'Str' string, then Storing the RHS-string in the output variable
    if (i_Debug_Loc) call Logger%Write( "Var = ", Var )
    i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value
    exit                                                                                                        ! Exiting the loop
  end do                                                                                                        ! End loop on input data

  if ( present(i_Found)     ) i_Found = i_Found_Loc                                                             ! Setting the output founding indicator to local value if present in argument list
  if ( present(i_Mandatory) ) then                                                                              ! If mandatory attribute of the parameter to be affected is present in the argument list, then
    if ( i_Mandatory .and. (.not.i_Found_Loc) ) call Error%GetValue(This%Section, Par)                         ! If the parameter to be affected is mandatory and has not been found, then Printing an error message and stopping the code
  end if                                                                                                        ! Enf if case on presence of optional argument

  if (i_Debug_Loc) call Logger%Exiting

End Subroutine

Subroutine GetValue_NoVar( This, Par, i_Found, i_Mandatory )
  use String_Library            ,only:  Parse
  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  Par                                     !< Parameter to be found
  logical                             ,optional ,intent(out)    ::  i_Found                                 !< Indicator that the parameter has been found
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the parameter to be found is mandatory
  character(*)                                              ,parameter  ::  ProcName='GetValue_NoVar'              ! Procedure name
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer       ,parameter                                      ::  i_LHS=1                                 ! Left/Right-hand side index
  logical                                                       ::  i_Found_Loc                             ! Local indicator that the parameter has been found
  character(1000)                                               ::  Str                                     ! Character string corresponding to a line of the considered section
  character(:)  ,allocatable    ,dimension(:)                   ::  Str_LR                                  ! Character string vector corresponding the LHS and RHS string of the '=' character
  call This%Rewind_Section                                                                                       ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  if    (.not.present(i_Found)) call Error%OptArg_Absent( Argument='i_Found', ProcName=ProcName )               !< If the input optional argument is not present, the error message since it is a mandatory optional argument
  i_Found_Loc   =       .false.                                                                                 ! Initialisation of the local founding indicator
  do                                                                                                            ! Loop on input data
    read(This%Unit, "(a)", iostat=ios ) Str                                                                     ! Reading the entire line of the considered section
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios == IOStat_781 ) exit
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    if ( Detect_Start_Section(Str) ) exit                                                                       ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
    call Parse( Str, '=', Str_LR )                                                                              ! Cutting the current line ('Str') into two elements separated by the '=' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
    if  (trim(Str_LR(i_LHS)) /= trim(Par))      cycle                                                           ! If the LHS-string and the searched parameter are different, then going to the next line
    i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value
    exit                                                                                                        ! Exiting the loop
  end do                                                                                                        ! End loop on input data
  if    (present(i_Found))      i_Found =       i_Found_Loc                                                     ! Setting the output founding indicator to local value if present in argument list
  if    (present(i_Mandatory))  then                                                                            ! If mandatory attribute of the parameter to be affected is present in the argument list, then
    if  (i_Mandatory.and.(.not.i_Found_Loc))    then                                                            ! If the parameter to be affected is mandatory and has not been found, then
      call Error%GetValue(This%Section, Par)                                                                   ! Printing an error message and stopping the code
    end if                                                                                                      ! End if case on mandatory and found indicators
  end if                                                                                                        ! Enf if case on presence of optional argument
End Subroutine

Subroutine GetValue_c1D( This, Par, Var, i_Found, i_Mandatory, i_Debug )

  use String_Library            ,only:  Parse, VecTrim

  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  Par                                     !< Parameter corresponding to the variable to be assigned
  character(:)  ,dimension(:)   ,allocatable    ,intent(inout)  ::  Var                                     !< Variable to be assigned
  logical                             ,optional ,intent(out)    ::  i_Found                                 !< Indicator that the parameter has been found
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the parameter to be found is mandatory
  logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName='GetValue_c1D'                ! Procedure name
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer       ,parameter                                      ::  i_LHS=1, i_RHS=2                        ! Left/Right-hand side index
  logical                                                       ::  i_Found_Loc                             ! Local indicator that the parameter has been found
  character(1000)                                               ::  Str                                     ! Character string corresponding to a line of the considered section
  character(:)  ,allocatable    ,dimension(:)                   ::  Str_LR                                  ! Character string vector corresponding the LHS and RHS string of the '=' character
  character(:)  ,allocatable    ,dimension(:)                   ::  String_List                             ! Character string vector corresponding to the output variable (This intermediate variable is required because the Parse procedure expecet an deffered-length character sring whereas current procedure is an assumed-length character)
  logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator
  integer       ::  i

  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )

  if (i_Debug_Loc) call Logger%Write( "Par = ", Par )
  if (i_Debug_Loc) call Logger%Write( "Calling This%Rewind_Section" )
  call This%Rewind_Section                                                                                       ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  i_Found_Loc   =       .false.                                                                                 ! Initialisation of the local founding indicator
  do                                                                                                            ! Loop on input data

    read(This%Unit, "(a)", iostat=ios ) Str                                                                     ! Reading the entire line in the secton file

    if ( ios == IOStat_781 ) exit
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    if (i_Debug_Loc) call Logger%Write( "Str = ", Str )

    if ( Detect_Start_Section(Str) ) exit                                                                       ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop

    call Parse( Str, '=', Str_LR )                                                                              ! Cutting the current line ('Str') into two elements separated by the '=' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
    if (i_Debug_Loc) call Logger%Write( "size(Str_LR) = ", size(Str_LR) )
    if (i_Debug_Loc) call Logger%Write( "Str_LR(1) = ", Str_LR(1) )
    if (i_Debug_Loc) call Logger%Write( "Str_LR(2) = ", Str_LR(2) )

    if ( trim(adjustl(Str_LR(i_LHS))) /= trim(Par) ) cycle                                                      ! If the LHS-string and the searched parameter are different, then going to the next line

    if (i_Debug_Loc) then
      call Logger%Write( "size(Str_LR) = ", size(Str_LR) )
      call Logger%Write( "i = ", "Str_LR(i) = ", Str_LR )
    end if

! REMARK:
! Workaround for the case where a "=" is present in the kinetic name (cf remark)
    if ( size(Str_LR) >= 3 ) then
      do i = 3,size(Str_LR)
        Str_LR(2) =  trim(Str_LR(2)) // "=" // trim(Str_LR(i))
      end do
    end if

    if (i_Debug_Loc) then
      call Logger%Write( "size(Str_LR) = ", size(Str_LR) )
      call Logger%Write( "i = ", "Str_LR(i) = ", Str_LR )
    end if

    call Parse( Str_LR(i_RHS), ';', String_List )                                                                 ! Cutting the RHS-string into multiple elements separated by the ';' character
    if ( allocated(Var) ) deallocate(Var)

    allocate( Var, source = String_List )     ! Do not work anymore because the Parse procedure output an deferred-length character array
!     allocate( Var( size(String_List) ) )
!     Var =       String_List

    i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value
    exit                                                                                                        ! Exiting the loop
  end do                                                                                                        ! End loop on input data
!   if ( .not. allocated(Var) )   allocate( Var(0) ) ! If the parameter has not been found, then allocating the variable to zero
  if ( .not. allocated(Var) )   allocate( character(0) :: Var(0) ) ! If the parameter has not been found, then allocating the variable to zero

  Var = VecTrim(Var)

  if ( present(i_Found) )       i_Found =       i_Found_Loc                                                     ! Setting the output founding indicator to local value if present in argument list
  if ( present(i_Mandatory) )   then                                                                            ! If mandatory attribute of the parameter to be affected is present in the argument list, then
    if  (i_Mandatory.and.(.not.i_Found_Loc))    then                                                            ! If the parameter to be affected is mandatory and has not been found, then
      call Error%GetValue(This%Section, Par)                                                                   ! Printing an error message and stopping the code
    end if                                                                                                      ! End if case on mandatory and found indicators
  end if                                                                                                        ! Enf if case on presence of optional argument

  if (i_Debug_Loc) call Logger%Exiting

End Subroutine


! REMARK: Data proceeded by this procedure has the following format:
! ITERATION              =       10 ; 58 ; 6854 ; 54125 ; 54
Subroutine GetValue_i1D( This, Par, Var, i_Found, i_Mandatory, i_Debug )
  use String_Library            ,only:  Parse, Convert_To_Integer
  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  Par                                     !< Parameter corresponding to the variable to be assigned
  integer       ,dimension(:)   ,allocatable    ,intent(inout)  ::  Var                                     !< Variable to be assigned
  logical                             ,optional ,intent(out)    ::  i_Found                                 !< Indicator that the parameter has been found
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the parameter to be found is mandatory
  logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator
  character(*)                                              ,parameter  ::  ProcName='GetValue_i1D'                ! Procedure name
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer       ,parameter                                      ::  i_LHS=1, i_RHS=2                        ! Left/Right-hand side index
  logical                                                       ::  i_Found_Loc                             ! Local indicator that the parameter has been found
  character(1000)                                               ::  Str                                     ! Character string corresponding to a line of the considered section
  character(:)  ,allocatable    ,dimension(:)                   ::  Str_LR                                  ! Character string vector corresponding the LHS and RHS string of the '=' character
  character(:)  ,allocatable    ,dimension(:)                   ::  CharVar                                 !< Character variable corresponding to the integer to be assigned
  logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator
  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  i_Found_Loc   =       .false.                                                                                 ! Initialisation of the local founding indicator
  do                                                                                                            ! Loop on input data
    read(This%Unit, "(a)", iostat=ios ) Str                                                                     ! Reading the entire line in the secton file
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios == IOStat_781 ) exit
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    if ( Detect_Start_Section(Str) ) exit                                                                       ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
    call Parse( Str, '=', Str_LR )                                                                              ! Cutting the current line ('Str') into two elements separated by the '=' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
    if ( trim(adjustl(Str_LR(i_LHS))) /= trim(Par) ) cycle                                                      ! If the LHS-string and the searched parameter are different, then going to the next line
    call Parse( Str_LR(i_RHS), ';', CharVar )                                                                   ! Cutting the RHS-string into multiple elements separated by the ';' character
    i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value
    exit                                                                                                        ! Exiting the loop
  end do                                                                                                        ! End loop on input data
  if ( present(i_Found) ) i_Found = i_Found_Loc                                                                 ! Setting the output founding indicator to local value if present in argument list
  if ( present(i_Mandatory) ) then                                                                              ! If mandatory attribute of the parameter to be affected is present in the argument list, then
    if ( i_Mandatory .and. (.not.i_Found_Loc) ) call Error%GetValue(This%Section, Par)                         ! If the parameter to be affected is mandatory and has not been found, then printing an error message and stopping the code
  end if                                                                                                        ! Enf if case on presence of optional argument
  allocate( Var( size(CharVar) ) )                                                                              ! Allocating the variable to be assigned to its associated character variable
  Var   =       Convert_To_Integer( CharVar )
End Subroutine


! REMARK: Data proceeded by this procedure has the following format:
!     MIXTURE:  N2      =       0.80
!     MIXTURE:  O2      =       0.20
Subroutine GetValue_cr1D( This, Par, Var, Val, i_Found, i_Mandatory )

  use String_Library            ,only:  Parse, Convert_To_Real, VecTrim

  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  Par                                     !< Parameter corresponding to the variable to be assigned
  character(:)  ,dimension(:)   ,allocatable    ,intent(inout)  ::  Var                                     !< Character Variable to be assigned    (Variable)
  real(rkp)     ,dimension(:)   ,allocatable    ,intent(inout)  ::  Val                                     !< Real Variable to be assigned         (Value)
  logical                             ,optional ,intent(out)    ::  i_Found                                 !< Indicator that the parameter has been found
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the parameter to be found is mandatory
  character(*)                                              ,parameter  ::  ProcName='GetValue_cr1D'               ! Procedure name
  integer                                                       ::  iLine                                   ! Index of line
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer       ,parameter                                      ::  i_LHS=1, i_RHS=2                        ! Left/Right-hand side index
  logical                                                       ::  i_Found_Loc                             ! Local indicator that the parameter has been found
  character(:)          ,dimension(:)   ,allocatable            ::  local_Var                               ! Local character Variable to be assigned
  real(rkp)             ,dimension(:)   ,allocatable            ::  local_Val                               ! Local real Variable to be assigned
  character(1000)                                               ::  Str                                     ! Character string corresponding to a line of the considered section
  character(:)  ,allocatable    ,dimension(:)                   ::  Str_LR                                  ! Character string vector corresponding the LHS and RHS string of the '=' characters
  call This%Rewind_Section                                                                                       ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  i_Found_Loc   =       .false.                                                                                 ! Initialisation of the local founding indicator
  iLine         =       0                                                                                       ! Initialisation of the line index
  do                                                                                                            ! Loop on input data
    read(This%Unit, "(a)", iostat=ios ) Str                                                                     ! Reading the entire line in the secton file
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios == IOStat_781 ) exit
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    if ( Detect_Start_Section(Str) ) exit                                                                       ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
    call Parse( Str, ':', Str_LR )                                                                              ! Cutting the current line ('Str') into two elements separated by the ':' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
    if ( trim(Str_LR(i_LHS)) /= trim(Par) ) cycle                                                               ! If the LHS-string and the searched parameter are different, then going to the next line
    iLine       =       iLine + 1                                                                               ! Incrementation of the line index
    Str         =       Str_LR(i_RHS)                                                                           ! Storing the RHS-string in the 'Str' variable
    call Parse( Str, '=', Str_LR )                                                                              ! Cutting the current line ('Str') into two elements separated by the '=' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
    allocate( character(len(Str_LR(i_LHS))) :: local_Var(iLine) )                                                                                ! Allocating the local LHS variable name to the current line number
!     allocate( local_Var(iLine) )                                                                                ! Allocating the local LHS variable name to the current line number
    allocate( local_Val(iLine) )                                                                                ! Allocating the local LHS variable name to the current line number
    local_Var(iLine)    =       Str_LR(i_LHS)                                                                   ! Affecting current  value in the local LHS character variable
    local_Val(iLine)    =       Convert_To_Real( Str_LR(i_RHS) )                                                ! Converting the LHS-string into a number and storing its value in the local variable
    if ( iLine > 1 ) local_Var(1:iLine-1)       =       Var(:)                                                  ! Affecting previous value for the LHS character variable
    if ( iLine > 1 ) local_Val(1:iLine-1)       =       Val(:)                                                  ! Affecting previous value for the LHS character variable
    call move_alloc( local_Var, Var )                                                                           ! Transfering allocation from local to output variable
    call move_alloc( local_Val, Val )                                                                           ! Transfering allocation from local to output variable
    i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value
  end do                                                                                                        ! End loop on input data

  if ( allocated(Var) ) Var = VecTrim(Var)

  if ( present(i_Found) ) i_Found = i_Found_Loc                                                                 ! Setting the output founding indicator to local value if present in argument list
  if ( present(i_Mandatory) ) then                                                                              ! If mandatory attribute of the parameter to be affected is present in the argument list, then
    if ( i_Mandatory .and. (.not.i_Found_Loc) ) call Error%GetValue(This%Section, Par)                         ! If the parameter to be affected is mandatory and has not been found, printing an error message and stopping the code
  end if                                                                                                        ! Enf if case on presence of optional argument
End Subroutine




! Data proceeded by this procedure has the following format:
! PARAMETER     =    N2:0.6; O2:0.6
Subroutine GetValue_C1_D1( This, Par, Var, Val, i_Found, i_Mandatory, i_Debug )

  use String_Library            ,only:  Parse, Convert_To_Real, VecTrim

  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  Par                                     !< Parameter corresponding to the variable to be assigned
  character(:)  ,dimension(:)   ,allocatable    ,intent(inout)  ::  Var                                     !< Character Variable to be assigned    (Variable)
  real(rkp)     ,dimension(:)   ,allocatable    ,intent(inout)  ::  Val                                     !< Real Variable to be assigned         (Value)
  logical                             ,optional ,intent(out)    ::  i_Found                                 !< Indicator that the parameter has been found
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the parameter to be found is mandatory
  logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName='GetValue_C1_D1'              ! Procedure name
  logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator
  integer       ,parameter                                      ::  i_LHS=1, i_RHS=2                        ! Left/Right-hand side index
  integer                                                       ::  i                                       ! Index of elements
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer                                                       ::  Length                                  ! Maximum length of character variables
  integer                                                       ::  NElements                               ! Number of elements
  logical                                                       ::  i_Found_Loc                             ! Local indicator that the parameter has been found
  character(1000)                                               ::  LongString                                     ! Character string corresponding to a line of the considered section
  character(:)  ,allocatable                                    ::  ErrMsg                                  ! Error message
  character(:)  ,allocatable                                    ::  Separator                               ! Separation character used to cut a string into multiple parts
  character(:)  ,allocatable    ,dimension(:)                   ::  Str_LR                                  ! Character string vector corresponding the LHS and RHS string of the '=' characters
  character(:)  ,allocatable    ,dimension(:)                   ::  VectorString

  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )

  if (i_Debug_Loc) call Logger%Write( "Calling This%Rewind_Section" )
  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  i_Found_Loc   =       .false.                                                                                 ! Initialisation of the local founding indicator

  do                                                                                                            ! Loop on input data

!   Reading the current line and exiting the loop if required
!   *********************************************************
    read(This%Unit, "(a)", iostat=ios ) LongString                                                              ! Reading the entire line in the secton file at storing it in the 'LongString' variable
    if ( (ios == IOStat_End) .or. (ios == IOStat_781) ) exit                                                    ! If end-of file while reading, then exiting the loop
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    if ( Detect_Start_Section(LongString) ) exit                                                                ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
    if (i_Debug_Loc) call Logger%Write( "LongString = ", trim(LongString) )                                     ! Debugging


!   Splitting the line at the "=" character and cycling if current parameter is not considered
!   ******************************************************************************************
    Separator   =       '='                                                                                     ! Setting the separation character to the "=" character
    call Parse( LongString, Separator, Str_LR )                                                                 ! Cutting the current line into two elements separated by the separation character (the 'Str_LR' variable should have only 2 elements)
    if ( trim(Str_LR(i_LHS)) /= trim(Par) ) cycle                                                               ! If the LHS-string and the searched parameter are different, then going to the next line
    if ( size(Str_LR) /= 2 ) then                                                                               ! Checking that the 'Str_LR' variable has only 2 elements, error if not
      ErrMsg = "The line '" // trim(LongString) //"' of the parameter '" // trim(Par) // "' of the section '" // trim(This%Section) // "' should contains only one character '='"
      call Error%Raise( ErrMsg, ProcName=ProcName )
    end if
    if (i_Found_Loc) then                                                                                       ! If the current parameter has already been read, then error since it should not be specified several time
      ErrMsg = "The parameter '" // trim(Par) // "' of the section '" // trim(This%Section) // "' should be specified only once."
      call Error%Raise( ErrMsg, ProcName=ProcName )
    end if

    if (i_Debug_Loc) call Logger%Write( "Str_LR(i_LHS) = ", Str_LR(i_LHS) )                                     ! Debugging
    if (i_Debug_Loc) call Logger%Write( "Str_LR(i_RHS) = ", Str_LR(i_RHS) )                                     ! Debugging


!   Splitting the RHS string at the "," character and getting the total number of elements
!   **************************************************************************************
    Separator   =       ','                                                                                     ! Setting the separation character to the ',' character
    call Parse( Str_LR(i_RHS), Separator, VectorString )                                                        ! Cutting the RHS string into multiple elements separated by the separation character
    NElements   =       size(VectorString)                                                                      ! Setting the number of elements
    if (i_Debug_Loc) call Logger%Write( "NElements = ", NElements )                                             ! Debugging
    if (i_Debug_Loc) call Logger%Write( "VectorString = ", VectorString )                                       ! Debugging

!   Checking elements, getting the maximum string length and allocating the output variables
!   ****************************************************************************************
    Length      =       0                                                                                       ! Initializing the length of the strings
    Separator   =       ':'                                                                                     ! Setting the separation character to the ':' character
    do i = 1,NElements                                                                                          ! Loop on all elements to compute the maximum length among all character strings in order to allocate the deferred-length output string
      call Parse( VectorString(i), Separator, Str_LR )                                                          ! Cutting the current element into two elements separated by the separation character (the 'Str_LR' variable should have only 2 elements)
      if ( size(Str_LR) /= 2 ) then                                                                             ! Checking that the 'Str_LR' vector string has only 2 elements, error if not
        ErrMsg = "A single ':' character must be specified for each species in the line '" // trim(LongString) //"' of the parameter '" // trim(Par) // "' of the section '" // trim(This%Section) // "'"
        call Error%Raise( ErrMsg, ProcName=ProcName )
      end if
      Length    =       max( Length, len_trim(Str_LR(i_LHS)) )                                                  ! Getting the maximum length of the strings for all elements which have been processed
    end do
    if ( allocated(Var) ) deallocate(Var)
    if ( allocated(Val) ) deallocate(Val)
    allocate( character(Length) :: Var(NElements) )                                                             ! Allocating the output character variable
    allocate( Val(NElements) )                                                                                  ! Allocating the output real variable
    if (i_Debug_Loc) call Logger%Write( "Length = ", Length )                                                   ! Debugging

!   Setting the output variables values
!   ***********************************
    do i = 1,NElements                                                                                          ! Loop on all elements to set the output variable values
      call Parse( VectorString(i), Separator, Str_LR )                                                          ! Cutting the current element into two elements separated by the separation character (the 'Str_LR' variable should have only 2 elements but this has already been checed in the previous loop)
      Var(i)    =       trim(Str_LR(i_LHS))                                                                     ! Setting output character variable
      Val(i)    =       Convert_To_Real( Str_LR(i_RHS) )                                                        ! Setting output real variable
      if (i_Debug_Loc) call Logger%Write( "i = ", i, "Var(i) = ", Var(i), "Val(i) = ", Val(i) )                 ! Debugging
    end do

    i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value

  end do                                                                                                        ! End loop on input data

  if ( present(i_Found) ) i_Found = i_Found_Loc                                                                 ! Setting the output founding indicator to local value if present in argument list
  if ( present(i_Mandatory) ) then                                                                              ! If mandatory attribute of the parameter to be affected is present in the argument list, then
    if ( i_Mandatory .and. (.not.i_Found_Loc) ) call Error%GetValue(This%Section, Par)                         ! If the parameter to be affected is mandatory and has not been found, printing an error message and stopping the code
  end if                                                                                                        ! Enf if case on presence of optional argument

  if (i_Debug_Loc) call Logger%Exiting

End Subroutine


! REMARK: Data proceeded by this procedure has the following format:
! PARTNER: M1   =       1.2 A ; 6.56 B ; A+ ;  5B-
Subroutine GetValue_c1D_cr2D ( This, Par, Var_L, Var_R, Coe_R, i_Mandatory, i_Debug )

  use String_Library            ,only:  Parse, Get_Numbers_AtLeftOf_Characters, VecTrim!, GetStrRHS
  use String_Library            ,only:  Get_Characters_AtRightOf_Numbers
  use Parameters_Module         ,only:  Zero

  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  Par                                     !< Parameter corresponding to the variable to be assigned
  character(:)  ,dimension(:)   ,allocatable    ,intent(out)    ::  Var_L                                   !< Character (variable name) on the left-hand-side of the '=' character
  character(:)  ,dimension(:,:) ,allocatable    ,intent(out)    ::  Var_R                                   !< Character (variable name) on the right-hand-side of the '=' character
  real(rkp)     ,dimension(:,:) ,allocatable    ,intent(out)    ::  Coe_R                                   !< Real (variable coefficient) on the right-hand-side of the '=' character
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the parameter to be found is mandatory
  logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName='GetValue_c1D_cr2D'           ! Procedure name
  integer                                                       ::  iLine                                   ! Index of line (equal the dimension of Var_L and the first dimension of Var_R/Coe_R)
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer       ,parameter                                      ::  i_LHS=1, i_RHS=2                        ! Left/Right-hand side index
  logical                                                       ::  i_Found_Loc                             ! Local indicator that the parameter has been found
!   character(:)          ,dimension(:)   ,allocatable            ::  VarLoc_L                                ! Local LHS character variable
!   character(:)          ,dimension(:,:) ,allocatable            ::  local_Var_R                             ! Local RHS character variable
!   real(rkp)             ,dimension(:,:) ,allocatable            ::  local_Coe_R                             ! Local RHS real variable

!   integer                                                       ::  NElt_Loc                                ! Number of elements in the RHS-string vector of a given line
!   integer                                                       ::  NElt_Max                                ! Maximum number of elements in the RHS-string vector for all lines
!   integer                                                       ::  Length


  character(1000)                                               ::  Line                                    ! Character string corresponding to a line of the considered section
  character(:)  ,allocatable    ,dimension(:)                   ::  Str_LR                                  ! Character string vector corresponding the LHS and RHS string of the '=' character
  character(:)  ,allocatable    ,dimension(:)                   ::  Str_Vec                                 ! Character string vector corresponding the element in the RHS string separated by the ';' character
  real(rkp)             ,dimension(:)   ,allocatable            ::  Vec_Coe_R                               ! Real string vector corresponding the element's coefficients in the RHS-string
  character(:)          ,dimension(:)   ,allocatable            ::  Vec_Var_R                               ! Character string vector corresponding the element's names   in the RHS-string
  logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator
!   integer       ::  i


  integer       ::  Length_Var_Left
  integer       ::  Length_Var_Right
  integer       ::  Size_Var_Right

  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )

  if ( .not. allocated(Var_L) ) allocate( character(0) :: Var_L(0)   )
  if ( .not. allocated(Var_R) ) allocate( character(0) :: Var_R(0,0) )

  if (i_Debug_Loc) call Logger%Write( "Calling This%Rewind_Section" )
  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  i_Found_Loc   =       .false.                                                                                 ! Initialisation of the local founding indicator



! ==============================================================================================================
!    FIRST LOOP ON THE DATA TO GET THE DIMENSIONS AND LENGTHS IN ORDER TO ALLOCATE THE OUTPUT VARIABLES
! ==============================================================================================================
  if (i_Debug_Loc) call Logger%Write( "FIRST LOOP ON THE DATA TO GET THE DIMENSIONS AND LENGTHS IN ORDER TO ALLOCATE THE OUTPUT VARIABLES" )

  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  iLine                 =       0                                                                               ! Initialisation of the line index
  Length_Var_Left       =       0
  Length_Var_Right      =       0
  Size_Var_Right        =       0

  do                                                                                                            ! Loop on input data

!   Reading the current line and cycling/exiting the loop if required
!   *****************************************************************
    read(This%Unit, "(a)", iostat=ios ) Line                                                                    ! Reading the entire line in the secton file
    if ( (ios == IOStat_End) .or. (ios == IOStat_781) ) exit                                                    ! If end-of file while reading, then exiting the loop
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    if ( Detect_Start_Section(Line) ) exit                                                                      ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
    call Parse( Line, ':', Str_LR )                                                                             ! Cutting the current line ('Line') into two elements separated by the ':' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
    if (trim(Str_LR(i_LHS)) /= trim(Par) ) cycle                                                                ! If the LHS-string and the searched parameter are different, then going to the next line

!   Splitting the parameter value into the name of the catalytic species and the list of species
!   ********************************************************************************************
!   At this point the variable "Str_LR(i_LHS)" and "Str_LR(i_RHS)" contains the parameter name and value.
!   The parameter is not required anymore. The Str_LR variable is overwritten by the next call to the
!   "Parser" procedure which split the parameter value into two string separated by the "=" character.
    iLine       =       iLine + 1                                                                               ! Incrementation of the line index
    Line        =       Str_LR(i_RHS)                                                                           ! Storing the RHS-string in the 'Line' variable
    if (i_Debug_Loc) call Logger%Write( "iLine = ", iLine, "trim(Line) = ", trim(Line) )                        ! Debugging
    call Parse( Line, '=', Str_LR )                                                                             ! Cutting the current line ('Line') into two elements separated by the '=' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
    Length_Var_Left     =       max( Length_Var_Left, len_trim(Str_LR(i_LHS)) )


!   Splitting the list of species into several string corresponding to each collisional species
!   *******************************************************************************************************
    call Parse( Str_LR(i_RHS), ';', Str_Vec )                                                                   ! Cutting the RHS string into multiple elements separated by the ';' character
    if (i_Debug_Loc) call Logger%Write( "i = ", "Str_Vec = ", Str_Vec )                                         ! Debugging
    call Get_Characters_AtRightOf_Numbers( Str_Vec, Vec_Var_R )                                                 ! Getting RHS character string
    Length_Var_Right    =       max( Length_Var_Right, len(Vec_Var_R) )                                    !
    Size_Var_Right      =       max( Size_Var_Right, size(Str_Vec) )                                            !

  end do                                                                                                        ! End loop on input data

! ==============================================================================================================
!    ALLOCATING AND INITIALIZING THE OUTPUT VARIABLES
! ==============================================================================================================
  if (i_Debug_Loc) call Logger%Write( "ALLOCATING AND INITIALIZING THE OUTPUT VARIABLES" )
  if (i_Debug_Loc) call Logger%Write( "Length_Var_Left  = ", Length_Var_Left  )
  if (i_Debug_Loc) call Logger%Write( "Length_Var_Right = ", Length_Var_Right )
  if (i_Debug_Loc) call Logger%Write( "Size_Var_Right   = ", Size_Var_Right   )


  if ( allocated(Var_L) ) deallocate( Var_L )
  if ( allocated(Var_R) ) deallocate( Var_R )
  if ( allocated(Coe_R) ) deallocate( Coe_R )

  allocate( character(Length_Var_Left ) :: Var_L(iLine)                )
  allocate( character(Length_Var_Right) :: Var_R(iLine,Size_Var_Right) )
  allocate(                                Coe_R(iLine,Size_Var_Right) )

  Var_L         =       ""
  Var_R         =       ""
  Coe_R         =       Zero

! ==============================================================================================================
!    SECOND LOOP ON THE DATA TO GET THE VALUES
! ==============================================================================================================
  if (i_Debug_Loc) call Logger%Write( "SECOND LOOP ON THE DATA TO GET THE VALUES" )
  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  iLine         =       0                                                                                       ! Initialisation of the line index
  do                                                                                                            ! Loop on input data

!   Reading the current line and cycling/exiting the loop if required
!   *****************************************************************
    read(This%Unit, "(a)", iostat=ios ) Line                                                                    ! Reading the entire line in the secton file
    if ( (ios == IOStat_End) .or. (ios == IOStat_781) ) exit                                                    ! If end-of file while reading, then exiting the loop
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    if ( Detect_Start_Section(Line) ) exit                                                                      ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
    call Parse( Line, ':', Str_LR )                                                                             ! Cutting the current line ('Line') into two elements separated by the ':' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
    if (trim(Str_LR(i_LHS)) /= trim(Par) ) cycle                                                                ! If the LHS-string and the searched parameter are different, then going to the next line

!   Splitting the parameter value into the name of the catalytic species and the list of species
!   ********************************************************************************************
!   At this point the variable "Str_LR(i_LHS)" and "Str_LR(i_RHS)" contains the parameter name and value.
!   The parameter is not required anymore. The Str_LR variable is overwritten by the next call to the
!   "Parser" procedure which split the parameter value into two string separated by the "=" character.
    iLine       =       iLine + 1                                                                               ! Incrementation of the line index
    Line        =       Str_LR(i_RHS)                                                                           ! Storing the RHS-string in the 'Line' variable
    if (i_Debug_Loc) call Logger%Write()
    if (i_Debug_Loc) call Logger%Write( "iLine = ", iLine, "trim(Line) = ", trim(Line) )                        ! Debugging
    call Parse( Line, '=', Str_LR )                                                                             ! Cutting the current line ('Line') into two elements separated by the '=' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)

!   Setting the name of the catalytic species in the output variable
!   ****************************************************************
    Var_L(iLine)        =       Str_LR(i_LHS)                                                                   ! Affecting current  value in the LHS output variable
    if (i_Debug_Loc) call Logger%Write( "trim(Var_L(iLine)) = ", trim(Var_L(iLine)) )

!   Separating the coefficient (on the lhs) from the name (on the rhs)
!   ******************************************************************
    call Parse( Str_LR(i_RHS), ';', Str_Vec )                                                                   ! Cutting the RHS string into multiple elements separated by the ';' character
    if (i_Debug_Loc) call Logger%Write( "i = ", "Str_Vec = ", Str_Vec )                                         ! Debugging
    call Get_Numbers_AtLeftOf_Characters(  Str_Vec, Vec_Coe_R, default='1' )                                    ! Getting LHS number (if none, returning unity)
    call Get_Characters_AtRightOf_Numbers( Str_Vec, Vec_Var_R )                                                 ! Getting RHS character string
    if (i_Debug_Loc) call Logger%Write( "i = ", "Vec_Var_R(i) = ", VecTrim(Vec_Var_R), "Vec_Coe_R = ", Vec_Coe_R )
    Coe_R(iLine,1:size(Vec_Coe_R))      =       Vec_Coe_R                                                       ! Affecting current  value for the RHS character variable
    Var_R(iLine,1:size(Vec_Coe_R))      =       Vec_Var_R                                                       ! Affecting current  value for the RHS character variable

    i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value

  end do                                                                                                        ! End loop on input data


  Var_L =       VecTrim(Var_L)
  Var_R =       VecTrim(Var_R)

  if ( present(i_Mandatory) ) then                                                                              ! If mandatory attribute of the parameter to be affected is present in the argument list, then
    if  (i_Mandatory.and.(.not.i_Found_Loc))    then                                                            ! If the parameter to be affected is mandatory and has not been found, then
      call Error%GetValue(This%Section, '-')                                                                   ! Printing an error message and stopping the code
    end if                                                                                                      ! End if case on mandatory and found indicators
  end if                                                                                                        ! Enf if case on presence of optional argument

  if (i_Debug_Loc) call Logger%Exiting









!
!
!
!     iLine         =       0                                                                                       ! Initialisation of the line index
!
!   NElt_Loc      =       0                                                                                       ! Initialisation of the number of element per RHS-string
!   NElt_Max      =       0                                                                                       ! Initialisation of the number of element per RHS-string
!
!
!
!
!
!   do                                                                                                            ! Loop on input data
!
!     read(This%Unit, "(a)", iostat=ios ) Line                                                                    ! Reading the entire line in the secton file
!     if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
!     if ( ios == IOStat_781 ) exit
!     if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
!
!     if ( Detect_Start_Section(Line) ) exit                                                                   ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
!     call Parse( Line, ':', Str_LR )                                                                             ! Cutting the current line ('Line') into two elements separated by the ':' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
!     if (trim(Str_LR(i_LHS)) /= trim(Par) ) cycle                                                                ! If the LHS-string and the searched parameter are different, then going to the next line
!     iLine       =       iLine + 1                                                                               ! Incrementation of the line index
!     Line        =       Str_LR(i_RHS)                                                                           ! Storing the RHS-string in the 'Line' variable
!     if (i_Debug_Loc) call Logger%Write()
!     if (i_Debug_Loc) call Logger%Write( "iLine = ", iLine, "trim(Line) = ", trim(Line) )
!     call Parse( Line, '=', Str_LR )                                                                             ! Cutting the current line ('Line') into two elements separated by the '=' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
!
!
!     Length      =       max( len(Str_LR(i_LHS)), len(Var_L) )
!
!     allocate( character(Length) :: VarLoc_L(iLine) )                                                                                 ! Allocating the local LHS variable name to the current line number
!     VarLoc_L(iLine)     =       Str_LR(i_LHS)                                                                   ! Affecting current  value in the local LHS character variable
!     if ( iLine > 1 ) VarLoc_L(1:iLine-1) = Var_L(:)                                                             ! Affecting previous value for the LHS character variable
!     call move_alloc( VarLoc_L, Var_L )                                                                          ! Transfering allocation from local to output variable
!     if (i_Debug_Loc) call Logger%Write( "trim(Var_L(iLine)) = ", trim(Var_L(iLine)) )
!     call Parse( Str_LR(i_RHS), ';', Str_Vec )                                                                   ! Cutting the RHS string into multiple elements separated by the ';' character
!     if (i_Debug_Loc) call Logger%Write( "i = ", "Str_Vec = ", Str_Vec )
!
! ! ==============================================================================================================
! !    SEPARATING THE COEFFICIENT (ON THE LHS) FROM THE NAME (ON THE RHS)
! ! ==============================================================================================================
!
!     NElt_Loc    =       size(Str_Vec)                                                                           ! Getting the number of element of the parsed variable
!     if ( allocated(Vec_Coe_R) ) deallocate(Vec_Coe_R)                                                           ! Deallocation if required
!     if ( allocated(Vec_Var_R) ) deallocate(Vec_Var_R)     ! Unneeded                                                      ! Deallocation if required
!
!     allocate( Vec_Coe_R(NElt_Loc) )                                                                                 ! Allocate
! !     allocate( Vec_Var_R(NElt_Loc) )                                                                                 ! Allocate
!     call Get_Numbers_AtLeftOf_Characters( Str_Vec, Vec_Coe_R, default='1' )                                                           ! Getting LHS number (if none, returning unity)
!     call Get_Characters_AtRightOf_Numbers( Str_Vec, Vec_Var_R )                                                                        ! Getting RHS character string
!
!     if (i_Debug_Loc) call Logger%Write( "i = ", "Vec_Var_R(i) = ", VecTrim(Vec_Var_R), "Vec_Coe_R = ", Vec_Coe_R )
!
!     NElt_Max    =       max( NElt_Loc, NElt_Max )                                                                      ! Setting the maximum number of element in the RHS strings taking into account previous RHS strings
!     if (i_Debug_Loc) call Logger%Write( "NElt_Loc = ", NElt_Loc )
!     if (i_Debug_Loc) call Logger%Write( "NElt_Max = ", NElt_Max )
!
!     Length      =       max( len(Vec_Var_R), len(Var_R) )
!
!     allocate( local_Coe_R(iLine,NElt_Max) )                                                                     ! Allocating the local RHS variable name to the current number of element and the maximum number of element in the RHS strings
!     allocate( character(Length) :: local_Var_R(iLine,NElt_Max) )                                                                     ! Allocating the local RHS variable name to the current number of element and the maximum number of element in the RHS strings
!
!     local_Coe_R         =       Zero                                                                         ! Initialisation
!     local_Var_R         =       ''                                                                              ! Initialisation
!     local_Coe_R(iLine,1:NElt_Loc)       =       Vec_Coe_R                                                       ! Affecting current  value for the RHS character variable
!     local_Var_R(iLine,1:NElt_Loc)       =       Vec_Var_R                                                       ! Affecting current  value for the RHS character variable
!     if ( iLine > 1 ) local_Coe_R(1:iLine-1,1:NElt_Max)       =       Coe_R(:,1:NElt_Max)                        ! Affecting previous value for the RHS character variable
!     if ( iLine > 1 ) local_Var_R(1:iLine-1,1:NElt_Max)       =       Var_R(:,1:NElt_Max)                        ! Affecting previous value for the RHS character variable
!     call move_alloc( local_Coe_R, Coe_R )                                                                       ! Transfering allocation from local to output variable
!     call move_alloc( local_Var_R, Var_R )                                                                       ! Transfering allocation from local to output variable
!     i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value
!
!   end do                                                                                                        ! End loop on input data
! !
!   Var_L =       VecTrim(Var_L)
!   Var_R =       VecTrim(Var_R)
!
!   if ( present(i_Mandatory) ) then                                                                              ! If mandatory attribute of the parameter to be affected is present in the argument list, then
!     if  (i_Mandatory.and.(.not.i_Found_Loc))    then                                                            ! If the parameter to be affected is mandatory and has not been found, then
!       call Error%GetValue(This%Section, '-')                                                                   ! Printing an error message and stopping the code
!     end if                                                                                                      ! End if case on mandatory and found indicators
!   end if                                                                                                        ! Enf if case on presence of optional argument
!
!   if (i_Debug_Loc) call Logger%Exiting
!
End Subroutine

! REMARK: Data proceeded by this procedure has the following format:
! ELE_LEVEL: N  =       FUNDAMENTAL
! ELE_LEVEL: N2 =       FUNDAMENTAL
! VIB_LEVEL: N2 =       SILVA
!     VIBRATIONAL_STATE_SPECIFIC_SPECIES: N2      =       LINODASILVA
!     ELECTRONIC_STATE_SPECIFIC_SPECIES:  N       =       FUNDAMENTAL
Subroutine GetValue_c1D_c1D( This, Par, Var_L, Var_R, i_Found, i_Mandatory, i_Debug )

  use String_Library            ,only:  Parse, VecTrim


  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  Par                                     !< Parameter corresponding to the variable to be assigned
  character(:)  ,dimension(:)   ,allocatable    ,intent(inout)  ::  Var_L                                   !< Character on the left-hand-side of the '=' character
  character(:)  ,dimension(:)   ,allocatable    ,intent(inout)  ::  Var_R                                   !< Character on the right-hand-side of the '=' character
  logical                             ,optional ,intent(out)    ::  i_Found                                 !< Indicator that the parameter has been found
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the parameter to be found is mandatory
  logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator

  character(*)                                              ,parameter  ::  ProcName='GetValue_c1D_c1D'            ! Procedure name
  integer                                                       ::  NLine                                   ! Index of line (equal the dimension of Var_L and Var_R)
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer       ,parameter                                      ::  i_LHS=1, i_RHS=2                        ! Left/Right-hand side index
  logical                                                       ::  i_Found_Loc                             ! Local indicator that the parameter has been found
  character(:)          ,dimension(:)   ,allocatable            ::  VarLoc_L                                ! Local LHS character variable
  character(:)          ,dimension(:)   ,allocatable            ::  VarLoc_R                                ! Local RHS character variable
  character(1000)                                               ::  Line                                    ! Character string corresponding to a line of the considered section
  character(:)  ,allocatable    ,dimension(:)                   ::  Line_LR                                 ! Character string vector corresponding the LHS and RHS string of the '=' character
  logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator

  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )

  if (i_Debug_Loc) call Logger%Write( "Calling This%Rewind_Section" )
  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  i_Found_Loc   =       .false.                                                                                 ! Initialisation of the local founding indicator
  NLine         =       0                                                                                       ! Initialisation of the line index
  do                                                                                                            ! Loop on input data

    read(This%Unit, "(a)", iostat=ios ) Line                                                                    ! Reading the entire line in the secton file
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios == IOStat_781 ) exit
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error

    if ( Detect_Start_Section(Line) ) exit                                                                      ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
    call Parse( Line, ':', Line_LR )                                                                            ! Cutting the current line ('Line') into two elements separated by the ':' character ('Line_LR' should have 2 elements, a left and right-hand-side strings)
    if ( trim(Line_LR(i_LHS)) /= trim(Par) ) cycle                                                              ! If the LHS-string and the searched parameter are different, then going to the next line

    NLine       =       NLine + 1                                                                               ! Incrementation of the line index
    if (i_Debug_Loc) call Logger%Write( "NLine = ", NLine )
    Line        =       Line_LR(i_RHS)                                                                          ! Storing the RHS-string in the 'Line' variable
    call Parse( Line, '=', Line_LR )                                                                            ! Cutting the current line ('Line') into two elements separated by the '=' character ('Line_LR' should have 2 elements, a left and right-hand-side strings)
    if (i_Debug_Loc) call Logger%Write( "Line_LR(1) = ", Line_LR(1) )
    if (i_Debug_Loc) call Logger%Write( "Line_LR(2) = ", Line_LR(2) )

    if ( NLine == 1 ) then                                                                                      ! If the first line is being considered, then only the current values are stored in the local variable
      allocate( VarLoc_L, source = [ Line_LR(i_LHS) ] )                                                         ! Allocating the local variable and storing the current value
      allocate( VarLoc_R, source = [ Line_LR(i_RHS) ] )                                                         ! Allocating the local variable and storing the current value
    else                                                                                                        ! If the other than the first line is considered, then previous and current values have to be stored in the local variable
      allocate( VarLoc_L, source = [ Var_L, Line_LR(i_LHS) ] )                                                  ! Allocating the local variable and storing the previous and current values values have to be stored in the local variable
      allocate( VarLoc_R, source = [ Var_R, Line_LR(i_RHS) ] )                                                  ! Allocating the local variable and storing the previous and current values
    end if                                                                                                      ! End if case on the line index being treated

    if (i_Debug_Loc) call Logger%Write( "VarLoc_L = ", VarLoc_L )
    if (i_Debug_Loc) call Logger%Write( "VarLoc_R = ", VarLoc_R )

    call move_alloc( VarLoc_L, Var_L )                                                                          ! Transfering allocation from temporary to final variable
    call move_alloc( VarLoc_R, Var_R )                                                                          ! Transfering allocation from temporary to final variable

    if (i_Debug_Loc) call Logger%Write( "Var_L = ", Var_L )
    if (i_Debug_Loc) call Logger%Write( "Var_R = ", Var_R )

    i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value

  end do                                                                                                        ! End loop on input data

  if ( .not. allocated(Var_L) ) allocate( character(0) :: Var_L(0) )
  if ( .not. allocated(Var_R) ) allocate( character(0) :: Var_R(0) )

  Var_L =       VecTrim(Var_L)
  Var_R =       VecTrim(Var_R)


  if ( present(i_Mandatory) ) then                                                                              ! If mandatory attribute of the parameter to be affected is present in the argument list, then
    if  (i_Mandatory.and.(.not.i_Found_Loc))    then                                                            ! If the parameter to be affected is mandatory and has not been found, then
      call Error%GetValue(This%Section, '-')                                                                   ! Printing an error message and stopping the code
    end if                                                                                                      ! End if case on mandatory and found indicators
  end if                                                                                                        ! Enf if case on presence of optional argument

  if ( present(i_Found) ) i_Found = i_Found_Loc                                                                 ! Setting the output founding indicator to local value if present in argument list

  if (i_Debug_Loc) call Logger%Exiting

End Subroutine


!  COUNTING THE TOTAL NUMBER OF REACTION AND ALLOCATING OUTPUT VARIABLES   *
Subroutine GetValue_ccr2D( This, Reac, Prod, Coef, Kmod, i_Debug )

  use String_Library            ,only:  Parse, Convert_To_String, Convert_To_Integer, Convert_To_Real, VecTrim
  use Parameters_Module         ,only:  Zero

  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  character(:)  ,dimension(:,:) ,allocatable    ,intent(inout)  ::  Reac                                    !< Names of reactant species in each reaction
  character(:)  ,dimension(:,:) ,allocatable    ,intent(inout)  ::  Prod                                    !< Names of product species in each reaction
  real(rkp)     ,dimension(:,:) ,allocatable    ,intent(inout)  ::  Coef                                    !< Reaction rate coefficients
  integer       ,dimension(:,:) ,allocatable    ,intent(inout)  ::  Kmod                                    !< Reaction rate model indicator
  logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator

  logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='GetValue_ccr2D'              ! Procedure name
  integer                                                       ::  NLines                                  ! Number of line (equal the total number of reaction in the kinetic file)
  integer                                                       ::  iLine                                   ! Index of line
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer       ,parameter                                      ::  i_LHS=1, i_RHS=2                        ! Left/Right-hand side index
  character(:)  ,allocatable    ,dimension(:)                   ::  Strings                                 ! Character string vector
  character(500)                                                ::  Line                                    ! Character string corresponding to a line of the considered section
  character(500)                                                ::  ListCoefficients                        ! Character string corresponding to specification of reaction rate coefficients and model indicators
  character(500)                                                ::  List_Species                            ! Character string corresponding to specification of reaction species
  character(500)                                                ::  List_Reactants                          ! Character string corresponding to specification of reaction reactant species
  character(500)                                                ::  List_Products                           ! Character string corresponding to specification of reaction product species
  integer                                                       ::  NStrings                                ! Number of elements in the Strings variable
  integer               ,parameter                              ::  NumReaSpe       =       3               ! Number of species in the list of reactant/product of each reaction
  integer               ,parameter                              ::  NumReaMod       =       2               ! Number of reaction rate model indicators
  integer                                                       ::  i
  integer                                                       ::  NCoeff
  character(:)  ,allocatable                                    ::  ErrMsg                                  ! Error message
  real(rkp)     ,dimension(:,:) ,allocatable                    ::  Coef_tmp
  integer                                                       ::  Length

  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )

! ==============================================================================================================
!    COUNTING THE NUMBER OF LINES, IE THE NUMBER OF REACTIONS
! ==============================================================================================================
  if (i_Debug_Loc) call Logger%Write( "Calling This%Rewind_Section" )
  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  NLines        =       0                                                                                       ! Initialisation of the line index
  do                                                                                                            ! Loop on input data
    read(This%Unit, *, iostat=ios )                                                                             ! Reading the file in order to determine the number of line (so that the output variables can be allocated once and for all and not at each iteration using the move_alloc commmand)
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios == IOStat_781 ) exit
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    NLines      =       NLines + 1                                                                              ! Incrementation of the line index
  end do                                                                                                        ! End loop on input data
  if (NLines == 0) call Error%GetValue(This%Section, '-')                                                      ! If no reaction found, then printing an error message and stopping the code
  if (i_Debug_Loc) call Logger%Write( "NLines = ", NLines )

  if ( allocated(Reac) ) deallocate (Reac)
  if ( allocated(Prod) ) deallocate (Prod)
  if ( allocated(Coef) ) deallocate (Coef)
  if ( allocated(Kmod) ) deallocate (Kmod)
  Length        =       1000
  allocate( character(Length) :: Reac( NumReaSpe, NLines ) )
  allocate( character(Length) :: Prod( NumReaSpe, NLines ) )
  allocate( Coef( 0, NLines ) )                                                                                 ! Initializing the allocation status
  allocate( Kmod( NumReaMod, NLines ) )
  Reac(:,:)     =       ''
  Prod(:,:)     =       ''
  Kmod(:,:)     =       0

  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  iLine         =       0                                                                                       ! Initialisation of the line index
  do                                                                                                            ! Loop on input data

! ==============================================================================================================
!    READING THE ENTIRE LINE CORRESPONDING TO CURRENT REACTION
! ==============================================================================================================
    read(This%Unit, "(a)", iostat=ios ) Line                                                                    ! Reading the entire line in the secton file
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios == IOStat_781 ) exit
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    iLine       =       iLine + 1                                                                               ! Incrementation of the line index
    if (i_Debug_Loc) call Logger%Write()
    if (i_Debug_Loc) call Logger%Write( "iLine = ", iLine, "trim(Line) = ", trim(Line) )
    call Parse( Line, ':', Strings )                                                                            ! Splitting the zentire Line string into two parts: the "species" and the "coefficients" part separated by the ':' character (Strings should have a dimension of two, otherwise, it is an error)
    if ( size(Strings) /= 2 ) then
      ErrMsg = "When reading the reaction " // Convert_To_String(iLine) // " a single ':' character should appear in each line"
      call Error%Raise( ErrMsg, ProcName=ProcName )
    end if

! ==============================================================================================================
!    SETTING THE SPECIES AND THE COEFFICIENT PART OF CURRENT REACTION
! ==============================================================================================================
    List_Species        =       Strings(i_LHS)                                                                  ! Storing the "species" string  into a specific variable
    ListCoefficients    =       Strings(i_RHS)                                                                  ! Storing the "coefficients" string into a specific variable
    if (i_Debug_Loc) call Logger%Write( "List_Species     = ", trim(List_Species) )
    if (i_Debug_Loc) call Logger%Write( "ListCoefficients = ", trim(ListCoefficients) )
    call Parse( List_Species, '=', Strings )                                                                    ! Splitting the "species" string into two parts: the "reactants_list" and the "product_list" part separated by the '=' character
    if ( size(Strings) /= 2 ) then
      ErrMsg = "When reading the reaction " // Convert_To_String(iLine) // " a single '=' character should appear in each line"
      call Error%Raise( ErrMsg, ProcName=ProcName )
    end if

! ==============================================================================================================
!    SETTING THE LIST OF REACTANTS AND PRODUCTS OF CURRENT REACTION
! ==============================================================================================================
    List_Reactants      =       Strings(i_LHS)                                                                  ! Storing the "reactants_list" string  into a specific variable
    List_Products       =       Strings(i_RHS)                                                                  ! Storing the "species" product_list  into a specific variable
    if (i_Debug_Loc) call Logger%Write( "List_Reactants = ", trim(List_Reactants) )
    if (i_Debug_Loc) call Logger%Write( "List_Products  = ", trim(List_Products) )
    call Parse( List_Reactants, '+', Strings, EscRHS=['+','('] )                                                ! Splitting the "reactants_list" string into multiple parts corresponding to individual species names separated by the '+' character
    Reac(1:size(Strings),iLine) =       Strings(:)                                                              ! Storing the reactants names into the output variable
    call Parse( List_Products, '+', Strings, EscRHS=['+','('] )                                                 ! Splitting the "product_list" string into multiple parts corresponding to individual species names separated by the '+' character
    Prod(1:size(Strings),iLine) =       Strings(:)                                                              ! Storing the products names into the output variable

! ==============================================================================================================
!    SETTING THE LIST OF COEFFICIENTS OF CURRENT REACTION
! ==============================================================================================================
    call Parse( ListCoefficients, ' ', Strings )                                                                ! Splitting the "coefficient" string into several parts, each one corresponding to a given coefficient
    NStrings    =       size(Strings)                                                                           ! Getting the number of coefficient: The first two coefficients are the integer numbers corresponding to the model indicator (Kf/Kb) and the remaing real number are the reaction rate coefficients
    NCoeff      =       NStrings - NumReaMod                                                                    ! Setting the number of coefficients (excluding the model indicator)
    if ( NCoeff <= 0 ) then                                                                                     ! If the number of coefficients if <= zero, then the data stored in the reaction file is erroneous
      ErrMsg = "When reading the reaction " // Convert_To_String(iLine) // " no reaction rate coefficients have been specified"
      call Error%Raise( ErrMsg, ProcName=ProcName )
    end if
    if ( NCoeff > size(Coef,1) ) then                                                                           ! The variable need to be extended
      allocate( Coef_tmp(NCoeff,NLines) )
      Coef_tmp  =       Zero
      Coef_tmp(1:size(Coef,1),:)        =       Coef                                                            ! Storing previous values
      call move_alloc( Coef_tmp, Coef )
    end if
    Kmod(1,iLine)       =       Convert_To_Integer( Strings(1) )
    Kmod(2,iLine)       =       Convert_To_Integer( Strings(2) )
    do i = 1,NCoeff
      Coef(i,iLine)     =       Convert_To_Real( Strings(i+2) )
    end do

  end do                                                                                                        ! End loop on input data


! ==============================================================================================================
!    COMPACTING THE REAC AND PROD VARIABLES
! ==============================================================================================================
  Reac  =       VecTrim( Reac )
  Prod  =       VecTrim( Prod )


  if (i_Debug_Loc) call Logger%Exiting

End Subroutine


! REMARK: Data proceeded by this procedure has the following format:
! 567
! 0.00000  5
! 1.02454  6
! ...
! 5.18461  9
! The format of real and integer number does not matter
!   call This%GetValue (  Level%Number, Level%Energy, Level%Degenerancy  )
! This procedure is used to read level dat
!  Argument are intent(out) because there is no Initialisation for level
!  Since the data start by specifying the number of level, the initial loop on not required.
!  However, it is kept since it allows to check if the number of line is correct
Subroutine GetValue_i0D_ri1D ( This, Num, R1D, I1D )
  use Parameters_Module         ,only:  Zero
  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  integer                                       ,intent(out)    ::  Num                                     !< Number of internal level
  real(rkp)     ,dimension(:)   ,allocatable    ,intent(out)    ::  R1D                                     !< Energy of internal level
  integer       ,dimension(:)   ,allocatable    ,intent(out)    ::  I1D                                     !< Degenerancy of internal level
  character(*)                                              ,parameter  ::  ProcName='GetValue_i0D_ri1D'           ! Procedure name
  integer                                                       ::  iLine                                   ! Index of line (equal the total number of reaction in the kinetic file)
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer                                                       ::  idx                                     ! Index of line
! The Rewind_Section procedure cannot be called because the IdxSec variable has not been initialize !!!
! CORRECTION: if the IdxSec variable is initialized, the Rewind_Section should only rewind the file... so the job is done...
! Since This procedure GetValue_i0D_ri1D is called for level data, the level file has been cleaned of comment and blank lines
! Normally, there should be only the data so a rewind call should be sufficient
!   call This%Rewind_Section                                                                                     ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  rewind (This%Unit)                                                                                    ! Rewinding the file to the top
  iLine         =       0                                                                                       ! Initialisation of the line index
  do                                                                                                            ! Loop on input data
    read(This%Unit, *, iostat=ios )                                                                             ! Reading the file in order to determine the number of line (so that the output variables can be allocated once and for all and not at each iteration using the move_alloc commmand)
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios == IOStat_781 ) exit
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                               ! If something went wrong when reading the file, then raise an error
    iLine       =       iLine   +       1                                                                       ! Incrementation of the line index
  end do                                                                                                        ! End loop on input data
  if    (iLine == 0)    call Error%GetValue('-', '-')                                                          ! If no reaction found, then printing an error message and stopping the code            @TODO: be more specific on the error message
  iLine =       iLine   -       1                                                                               ! Substracting a line since the first line of the file (without comment/bank lines) corresponds to the number of data
  rewind (This%Unit)                                                                                    ! Rewinding the file to the top
  read(This%Unit,*, iostat=ios ) Num                                                                    ! Reading the first line corresponding to the number of data to be read
  if    (ios > 0)       call Error%Read( Unit=This%Unit, ProcName=ProcName )                               ! If something went wrong when reading the file, then raise an error
  if    (Num /= iLine)  call Error%GetValue('-', '-')                                                          ! If the two number are different, then there is an error in the data ....              @TODO: be more specific on the error message
  if    (allocated(R1D))        deallocate (R1D)                ! Deallocation is useless since the data has not been allocated before
  if    (allocated(I1D))        deallocate (I1D)
  allocate      ( R1D( Num ) )
  allocate      ( I1D( Num ) )
  R1D(:)        =       Zero
  I1D(:)        =       0
  do iLine = 1,Num                                                                                              ! Loop on input data
    read(This%Unit, *, iostat=ios ) idx, R1D(iLine), I1D(iLine)                                         ! Reading the entire line in the secton file
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                               ! If something went wrong when reading the file, then raise an error
  end do                                                                                                        ! End loop on input data
End Subroutine


Subroutine GetValue_c1D_r2D ( This, C1D, R2D, i_Debug )
  class(File_Type)                                      ,intent(inout)  ::  This                            !< Passed-object dummy argument
  character(*)  ,dimension( :    )      ,allocatable    ,intent(out)    ::  C1D                             !< Character variable
  real(rkp)     ,dimension( :, : )      ,allocatable    ,intent(out)    ::  R2D                             !< Real variable
  logical                                     ,optional ,intent(in)     ::  i_Debug                         !< Debugging indicator

  logical                                                               ::  i_Debug_Loc                     ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='GetValue_c1D_r2D'    ! Procedure name
  integer                                                               ::  NLin                            ! Number of line to be read
  integer                                                               ::  NCol                            ! Number of column to be read
  integer                                                               ::  iLine                           ! Index of line (equal the total Number of reaction in the kinetic file)
  integer                                                               ::  ios                             ! Input/output status indicator

  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )

  if (i_Debug_Loc) call Logger%Write( "Calling This%Rewind_Section" )
  call This%Rewind_Section                                                                                       ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  read(This%Unit,*, iostat=ios ) NLin, NCol                                                                     ! Reading the first line corresponding to the number of line and column to be read
  if    (ios > 0)       call Error%Read( Unit=This%Unit, ProcName=ProcName )                               ! If something went wrong when reading the file, then raise an error
  if (i_Debug_Loc) call Logger%Write( "NLin = ", NLin )
  if (i_Debug_Loc) call Logger%Write( "NCol = ", NCol )
  allocate(     C1D( NLin       )       )                                                                       ! Allocating the character 1D variable to the  number of line
  allocate(     R2D( NLin, NCol )       )                                                                       ! Allocating the real 2D variable to the  number of line and column
  do iLine = 1,NLin                                                                                              ! Loop on input data, i.e. all line to be read
    read(This%Unit, *, iostat=ios ) C1D(iLine), R2D(iLine,:)                                                    ! Reading the entire line in the secton file
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                               ! If something went wrong when reading the file, then raise an error
  end do                                                                                                        ! End loop on input data

  if (i_Debug_Loc) call Logger%Exiting

End Subroutine


! REMARK: Data proceeded by this procedure has the following format:
!     RANGE:    MIN=0;   MAX=1;   STEP=0.5
!     RANGE:    MIN=1;   MAX=10;  STEP=0.8
!     RANGE:    MIN=10;  MAX=100; STEP=1
Subroutine GetValue_3r1D( This, Par, Par1, Par2, Par3, Var1, Var2, Var3, i_Found, i_Mandatory )
  use String_Library            ,only:  Parse, Convert_To_Real
  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  Par                                     !< Parameter corresponding to the variable to be assigned
  character(*)                                  ,intent(in)     ::  Par1                                    !< Sub-parameter number 1 to be assigned
  character(*)                                  ,intent(in)     ::  Par2                                    !< Sub-parameter number 2 to be assigned
  character(*)                                  ,intent(in)     ::  Par3                                    !< Sub-parameter number 3 to be assigned
  real(rkp)     ,dimension(:)   ,allocatable    ,intent(inout)  ::  Var1                                    !< Variable associated to the sub-parameter number 1
  real(rkp)     ,dimension(:)   ,allocatable    ,intent(inout)  ::  Var2                                    !< Variable associated to the sub-parameter number 2
  real(rkp)     ,dimension(:)   ,allocatable    ,intent(inout)  ::  Var3                                    !< Variable associated to the sub-parameter number 3
  logical                             ,optional ,intent(out)    ::  i_Found                                 !< Indicator that the parameter has been found
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the parameter to be found is mandatory
  character(*)                                              ,parameter  ::  ProcName='GetValue_3r1D'               ! Procedure name
  integer                                                       ::  iLine                                   ! Index of line (equal the dimension of Var_L and the first dimension of Var_R/Coe_R)
  integer                                                       ::  ielt                                    ! Index of elements (corresponding to either 'MIN=X', 'MAX=Y' and 'STEP=Z')
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer       ,parameter                                      ::  i_LHS=1, i_RHS=2                        ! Left/Right-hand side index
  logical                                                       ::  i_Found_Loc                             ! Local indicator that the parameter has been found
  real(rkp)     ,dimension(:)   ,allocatable                    ::  local_Var1                              ! Local variable associated to variable 1
  real(rkp)     ,dimension(:)   ,allocatable                    ::  local_Var2                              ! Local variable associated to variable 2
  real(rkp)     ,dimension(:)   ,allocatable                    ::  local_Var3                              ! Local variable associated to variable 3
  character(1000)                                               ::  Str                                     ! Character string corresponding to a line of the considered section
  character(:)  ,allocatable    ,dimension(:)                   ::  Str_LR                                  ! Character string vector corresponding the LHS and RHS string of the ':' character
  character(:)  ,allocatable    ,dimension(:)                   ::  Str_MinMaxStep                          ! Character string vector corresponding the RHS string of a given line (elements "MIN=X;MAX=Y;STEP=Z")
  call This%Rewind_Section                                                                                       ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  i_Found_Loc   =       .false.                                                                                 ! Initialisation of the local founding indicator
  iLine         =       0                                                                                       ! Initialisation of the line index
  do                                                                                                            ! Loop on input data
    read(This%Unit, "(a)", iostat=ios ) Str                                                                     ! Reading the entire line in the secton file
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios == IOStat_781 ) exit
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                               ! If something went wrong when reading the file, then raise an error
    if ( Detect_Start_Section(Str) ) exit                                                                    ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
    call Parse( Str, ':', Str_LR )                                                                              ! Cutting the current line ('Str') into two elements separated by the ':' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
    if  (trim(Str_LR(i_LHS)) /= trim(Par))      cycle                                                           ! If the LHS-string and the searched parameter are different, then going to the next line
    iLine       =       iLine   +       1                                                                       ! Incrementation of the line index
    Str =       Str_LR(i_RHS)                                                                                   ! Storing the RHS-string in the 'Str' variable (Normally, it should have the value 'MIN=X;MAX=Y;STEP=Z')
    call Parse( Str, ';', Str_MinMaxStep )                                                                      ! Cutting the current RHS string into multiple elements separated by the ';' character. ('Str_MinMaxStep' should have 3 elements: 'MIN=X', 'MAX=Y' and 'STEP=Z')
    do ielt = 1,size(Str_MinMaxStep)                                                                            ! Loop on all string elements
      Str       =       Str_MinMaxStep(ielt)                                                                    ! Storing the current element in the 'Str' variable (Normally, it should be Str='KEY=NUM' where 'KEY' is either 'MIN', 'MAX' or 'STEP' and 'NUM' is a real number)
      call Parse( Str, '=', Str_LR )                                                                            ! Cutting the current line ('Str') into two elements separated by the '=' character ('Str_LR' should have 2 elements, a left and right-hand-side strings)
      if        ( trim(Str_LR(i_LHS)) ==  trim(Par1) )  then                                                    ! If the current variable correspond to the sub-parameter number 1
        allocate( local_Var1(iLine) )                                                                           ! Allocating the local LHS variable name to the current line number
        local_Var1(iLine)       =       Convert_To_Real( Str_LR(i_RHS) )                                        ! Converting RHS-string into a number and storing its value in local variable
        if      (iLine>1)       local_Var1(1:iLine-1)   =       Var1(:)                                         ! Affecting previous value into the local variable
        call move_alloc( local_Var1, Var1 )                                                                     ! Transfering allocation from local to output variable
      else if   ( trim(Str_LR(i_LHS)) ==  trim(Par2) )  then                                                    ! If the current variable correspond to the sub-parameter number 2
        allocate( local_Var2(iLine)       )                                                                     ! Allocating the local LHS variable name to the current line number
        local_Var2(iLine)       =       Convert_To_Real( Str_LR(i_RHS) )                                        ! Converting RHS-string into a number and storing its value in local variable
        if      (iLine>1)       local_Var2(1:iLine-1)   =       Var2(:)                                         ! Affecting previous value into the local variable
        call move_alloc( local_Var2, Var2 )                                                                     ! Transfering allocation from local to output variable
      else if   ( trim(Str_LR(i_LHS)) ==  trim(Par3) )  then                                                    ! If the current variable correspond to the sub-parameter number 3
        allocate( local_Var3(iLine)       )                                                                     ! Allocating the local LHS variable name to the current line number
        local_Var3(iLine)       =       Convert_To_Real( Str_LR(i_RHS) )                                        ! Converting RHS-string into a number and storing its value in local variable
        if      (iLine>1)       local_Var3(1:iLine-1)   =       Var3(:)                                         ! Affecting previous value into the local variable
        call move_alloc( local_Var3, Var3 )                                                                     ! Transfering allocation from local to output variable
      else                                                                                                      ! If the current variable does not correspond to any input sub-parameters
        call Error%GetValue(This%Section, Par)                                                                 ! Printing an error message and stopping the code
      end if                                                                                                    ! End if case on sub-parameter
    end do                                                                                                      ! End loop on string elements
    i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value
  end do                                                                                                        ! End loop on input data
  if    (present(i_Found))      i_Found =       i_Found_Loc                                                     ! Setting the output founding indicator to local value if present in argument list
  if    (present(i_Mandatory))  then                                                                            ! If mandatory attribute of the parameter to be affected is present in the argument list, then
    if  (i_Mandatory.and.(.not.i_Found_Loc))    then                                                            ! If the parameter to be affected is mandatory and has not been found, then
      call Error%GetValue(This%Section, '-')                                                                   ! Printing an error message and stopping the code
    end if                                                                                                      ! End if case on mandatory and found indicators
  end if                                                                                                        ! Enf if case on presence of optional argument
End Subroutine


! REMARK: Data proceeded by this procedure has the following format:
! $THERMO_DATA:COEFF:GORDON
!    3  11
!    200  1000  2.210371497D+04 -3.818461820D+02  6.082738360D+00 -8.530914410D-03  1.384646189D-05 -9.625793620D-09  2.519705809D-12  7.108460860D+02 -1.076003744D+01
!   1000  6000  5.877124060D+05 -2.239249073D+03  6.066949220D+00 -6.139685500D-04  1.491806679D-07 -1.923105485D-11  1.061954386D-15  1.283210415D+04 -1.586640027D+01
!   6000 20000  8.310139160D+08 -6.420733540D+05  2.020264635D+02 -3.065092046D-02  2.486903333D-06 -9.705954110D-11  1.437538881D-15  4.938707040D+06 -1.672099740D+03

Subroutine GetValue_r2D( This, R2D )
  class(File_Type)                                      ,intent(inout)  ::  This                            !< Passed-object dummy argument
  real(rkp)     ,dimension( :, : )      ,allocatable    ,intent(out)    ::  R2D                             !< Two-dimensional real array
  character(*)                                              ,parameter  ::  ProcName='GetValue_r2D'        ! Procedure name
  integer                                                               ::  NRow                            ! Number of rows to be read
  integer                                                               ::  NCol                            ! Number of columns to be read
  integer                                                               ::  iRow                            ! Index of row
  integer                                                               ::  ios                             ! Input/output status indicator
  if ( This%Section_Found() ) then                                                                              ! If the current section has been found, then reading the data
    call This%Rewind_Section                                                                                    ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
    read(This%Unit,*, iostat=ios ) NRow, NCol                                                                   ! Reading the first line corresponding to the number of rows and columns to be read
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    allocate( R2D( NRow, NCol ) )                                                                               ! Allocating the real 2D variable to the  number of line and column
    do iRow = 1,NRow                                                                                            ! Loop on input data, i.e. all row to be read
      read(This%Unit, *, iostat=ios ) R2D(iRow,:)                                                               ! Reading the entire line in the secton file
      if (ios > 0) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    end do                                                                                                      ! End loop on input data
  else                                                                                                          ! If the current section has not been found, then setting the default allocation for the output variables
    allocate( R2D(0,0) )                                                                                        ! Setting the default allocation
  end if                                                                                                        ! End if case on ssection presence
End Subroutine

! REMARK: Data proceeded by this procedure has the following format:
! $VISCOSITY_COEFFICIENTS
!   3
!  0.268142000000D-01  0.317783800000D+00 -0.113155513000D+02     # (KG/M/S)
Subroutine GetValue_r1D( This, Var, i_Debug )
  class(File_Type)                              ,intent(inout)  ::  This                                    !< Passed-object dummy argument
  real(rkp)     ,dimension(:)   ,allocatable    ,intent(out)    ::  Var                                     !< Variable to be read
  logical                             ,optional ,intent(in)     ::  i_Debug                                 !< Debugging indicator
  logical                                                       ::  i_Debug_Loc                             ! Local debugging indicator
  character(*)                                              ,parameter  ::  ProcName='GetValue_r1D'                ! Procedure name
  integer                                                       ::  NVar                                    ! Number of variable to be read
  integer                                                       ::  ios                                     ! Input/output status indicator
  i_Debug_Loc   =       This%Get_Debug( i_Debug )                                                               ! Setting local debugging indicator
  if (i_Debug_Loc) call Logger%Entering( ProcName )
  if (i_Debug_Loc) call Logger%Write( "Calling This%Rewind_Section" )
  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  read(This%Unit,*, iostat=ios ) NVar                                                                           ! Reading the number of variable to be read
  if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                      ! If something went wrong when reading the file, then raise an error
  if (i_Debug_Loc) call Logger%Write( "NVar = ", NVar )
  allocate( Var(NVar) )                                                                                         ! Allocating the variable to be read
  read(This%Unit, *, iostat=ios ) Var(:)                                                                        ! Reading all elements of the variable
  if (ios > 0) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                        ! If something went wrong when reading the file, then raise an error
  if (i_Debug_Loc) call Logger%Write( "Var = ", Var(:) )
  if (i_Debug_Loc) call Logger%Exiting
End Subroutine



! Data proceeded by this procedure has the following format:
!       PARAMETER_NAME = 0.268142000000D-01  0.317783800000D+00 -0.113155513000D+02
Subroutine Get_r1D( This, Par, Var, i_Found, i_Mandatory )
  use String_Library            ,only:  Parse, Convert_To_Real
  class(File_Type)                              ,intent(in)     ::  This                                    !< Passed-object dummy argument
  character(*)                                  ,intent(in)     ::  Par                                     !< Parameter corresponding to the variable to be assigned
  real(rkp)     ,dimension(:)   ,allocatable    ,intent(out)    ::  Var                                     !< Variable to be assigned
  logical                             ,optional ,intent(out)    ::  i_Found                                 !< Indicator that the parameter has been found
  logical                             ,optional ,intent(in)     ::  i_Mandatory                             !< Indicator that the parameter to be found is mandatory
  character(*)                                              ,parameter  ::  ProcName='Get_r1D'                      ! Procedure name
  logical       ,parameter                                      ::  i_Debug_Loc=.false.
  integer                                                       ::  ios                                     ! Input/output status indicator
  integer       ,parameter                                      ::  i_LHS=1, i_RHS=2                        ! Left/Right-hand side index
  logical                                                       ::  i_Found_Loc                             ! Local indicator that the parameter has been found
  character(1000)                                               ::  Str                                     ! Character string corresponding to a line of the considered section
  character(:)  ,allocatable    ,dimension(:)                   ::  Strings                                  ! Character string vector corresponding the LHS and RHS string of the '=' character
  if (i_Debug_Loc) call Logger%Entering( ProcName )
  call This%Rewind_Section                                                                                      ! Rewinding the file to the considered section starting point (just after the section declaration '$SECTION')
  i_Found_Loc   =       .false.                                                                                 ! Initialisation of the local founding indicator
  do                                                                                                            ! Loop on input data
    read(This%Unit, "(a)", iostat=ios ) Str                                                                     ! Reading the entire line of the considered section
    if ( ios == IOStat_781 ) exit
    if ( ios == IOStat_End ) exit                                                                               ! If end-of file while reading, then exiting the loop
    if ( ios > 0 ) call Error%Read( Unit=This%Unit, ProcName=ProcName )                                    ! If something went wrong when reading the file, then raise an error
    if (i_Debug_Loc) call Logger%Write( "Str = ", Str )
    if ( Detect_Start_Section(Str) ) exit                                                                       ! If a new section starting point is detected, then the current section has been entirely read and so exiting the loop
    call Parse( Str, '=', Strings )                                                                             ! Cutting the current line ('Str') into two elements separated by the '=' character. The resulting array of characters 'Strings' should have 2 elements: the 1st corresponds to the parameter name, ie. the character to the LHS of the "=" symbol, and the 2nd element corresponds to the parameter values, ie. to the character to the  RHS of the "=" symbol

    if (i_Debug_Loc) call Logger%Write( "Strings = ", Strings )
    if ( trim(Strings(i_LHS)) /= trim(Par) ) cycle                                                               ! If the LHS-string and the searched parameter are different, then going to the next line

    if (i_Debug_Loc) call Logger%Write( "Strings(i_LHS) = ", Strings(i_LHS) )
    if (i_Debug_Loc) call Logger%Write( "Strings(i_RHS) = ", Strings(i_RHS) )

    Str         =       Strings(i_RHS)           ! Coping the RHS string which contains the parameter values
    call Parse( Str, ' ', Strings )              ! Cutting the parameters values into several elements separated by the blank character ' ' => The number of resulting string is the dimension of the output variable

    if (i_Debug_Loc) call Logger%Write( "size(Strings) = ", size(Strings) )

    allocate( Var , source = Convert_To_Real(Strings) )
    if (i_Debug_Loc) call Logger%Write( "size(Var) = ", size(Var) )

    if (i_Debug_Loc) call Logger%Write( "Var = ", Var)
!     allocate( Var( size(Strings) )

    i_Found_Loc =       .true.                                                                                  ! Setting the local founding indicator to true value
    exit                                                                                                        ! Exiting the loop
  end do                                                                                                        ! End loop on input data
  if    (present(i_Found))      i_Found =       i_Found_Loc                                                     ! Setting the output founding indicator to local value if present in argument list
  if    (present(i_Mandatory))  then                                                                            ! If mandatory attribute of the parameter to be affected is present in the argument list, then
    if  (i_Mandatory.and.(.not.i_Found_Loc))    then                                                            ! If the parameter to be affected is mandatory and has not been found, then
      call Error%GetValue(This%Section, Par)                                                                   ! Printing an error message and stopping the code
    end if                                                                                                      ! End if case on mandatory and found indicators
  end if                                                                                                        ! Enf if case on presence of optional argument
  if (i_Debug_Loc) call Logger%Exiting
End Subroutine



End Module
