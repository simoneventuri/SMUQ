SubModule(GPF_Output_Class) GPF_Output_SubClass

  use Logger_Class            ,only:  Logger, LogLevel_HEAVYDEBUG
  use Utilities_Library       ,only:  GetOptArgValue
  use GPF_Class               ,only:  GPF

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure InitializeOutput

  character(*)                                              ,parameter  ::  ProcName = "InitializeOutput"
  logical                                                               ::  Dbg

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling SetOutputFileName" )
  call SetOutputFileName( This, FileName )

  if (Dbg) call Logger%Write( "Calling SetOutputHardCopy" )
  call SetOutputHardCopy( This, HardCopy )

  if (Dbg) call Logger%Write( "Calling SetOutputDirectory" )
  call SetOutputDirectory( This, Directory )

  if (Dbg) call Logger%Write( "Calling This%Set_Command" )
  call This%Set_Command()

  if (Dbg) then
    call Logger%Write( "Calling PrintOutput" )
    call PrintOutput( This )
  end if

  if (Dbg) call Logger%Exiting()

End Procedure

Module Procedure WriteOutput
  if ( len_trim(This%Command) /= 0 ) write(Unit,"(a)") This%Command
End Procedure

Module Procedure GetOutputName
  Name      =   This%Name
End Procedure

Module Procedure GetOutputExtension
  Extension =   This%Extension
End Procedure

Module Procedure GetFileName
  FileName  =   This%FileName
End Procedure

Module Procedure GetDataFileName
  DataFile  =   This%DataFile
End Procedure

Module Procedure GetCommandFileName
  CommandFile =   This%CommandFile
End Procedure

Module Procedure GetDirectory
  Directory =   This%Directory
End Procedure

Module Procedure GetHardCopy
  HardCopy  =   This%HardCopy
End Procedure

Module Procedure SetOutputCommand
  This%Command  =   ''
  if ( This%Presence ) This%Command = 'set output "' // trim(This%FileName) // '"'
End Procedure

! **************************************************************************************************************
! **************************************************************************************************************
!                                       PRIVATE PROCEDURES
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine SetOutputFileName( This, FileName )
  use GPF_Tools                 ,only:  Check_Validity, Get_Extension, Remove_Extension
  use GPF_Parameters            ,only:  Extension_Default, Extension_Valid, KEY_data, KEY_command, Extension_CommandFile
  type(GPF_Output_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Output object
  character(*)                                ,optional ,intent(in)     ::  FileName                        !< Output file name including extension, if provided
!   integer                                                               ::  iDot                            ! Index of the last dot character
  if ( present(FileName) ) then                                                                                 ! If present optional input argument
    This%FileName       =   trim( Check_Name(FileName) )                                                    ! Checking the input FileName (replacing ' ' by '_')and setting its value in the structure
    This%Name           =   Remove_Extension( This%FileName ) ! result (FileName_NoExt)
    This%Extension      =   Get_Extension( This%FileName )
    This%Extension      =   Check_Validity( This%Extension, Extension_Valid, Extension_Default )    ! Checking its validity and setting its value in the structure
!     iDot                =   scan( This%FileName, '.', back=.True. )                                         ! Getting the index of the last '.' character to delimiter the file name from the file extension
!     if ( iDot == 0 ) then                                                                                       ! If no '.' character found, then file extension is absent
!       This%Name         =   This%FileName                                                                   ! Setting the output name to the FileName
!       This%Extension    =   ''                                                                              ! Setting an empty extension
!     else                                                                                                        ! If '.' character found, then file extension is present
!       This%Name         =   This%FileName(1:iDot-1)                                                         ! Extracting the name of the file
!       This%Extension    =   Check_Validity( This%FileName(iDot+1:), Extension_Valid, Extension_Default )    ! Checking its validity and setting its value in the structure
!     end if                                                                                                      ! End if case on dot character index
    This%DataFile       =   KEY_data    // '_' // This%Name                                                 ! Setting the DataFile name without extension (because for multiple graph image, an index is added to the DataFile names)
    This%CommandFile    =   KEY_command // '_' // This%FileName // '.' // Extension_CommandFile                 ! Setting the CommandFile name with extension
  else                                                                                                          ! If absent optional input argument
    This%FileName       =   ''
    This%Name           =   ''
    This%Extension      =   ''
    This%DataFile       =   KEY_data                                                                        ! Setting the DataFile name without extension (because for multiple graph image, an index is added to the DataFile names)
    This%CommandFile    =   KEY_command // '.' // Extension_CommandFile                                     ! Setting the CommandFile name with extension
  end if                                                                                                        ! End if case on presence of input optional argument
  if ( len_trim(This%Extension) /=0 ) then                                                                      ! If extension is present
    This%Presence       =   .True.                                                                          ! Setting presence indicator
  else                                                                                                          ! If extension is absent
    This%Presence       =   .False.                                                                         ! Setting presence indicator
  end if                                                                                                        ! End if case on extension presence
End Subroutine

Subroutine SetOutputHardCopy( This, HardCopy )
  type(GPF_Output_Type)                                 ,intent(inout)  ::  This
  logical                                     ,optional ,intent(in)     ::  HardCopy
  if ( present(HardCopy) ) then
    This%HardCopy       =   HardCopy
  else
    if ( len_trim(This%FileName) /= 0 ) then
      This%HardCopy     =   .True.
    else
      This%HardCopy     =   .False.
    end if
  end if
End Subroutine

Subroutine SetOutputDirectory( This, Directory )
  use GPF_Tools         ,only:  Replace_Character
  type(GPF_Output_Type)                                 ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  Directory
  This%Directory      =   GPF%Get_Default_File_Output_Directory()
  if ( present(Directory) ) then
    if ( len_trim(Directory) /= 0 ) This%Directory = Replace_Character( trim(Directory), ' ', '_' ) // '/'
  end if
End Subroutine

Function Check_Name( Name )
  use GPF_Tools         ,only:  Replace_Character
  character(*)                                          ,intent(in)     ::  Name
  character(:)  ,allocatable                                            ::  Check_Name
  if ( len_trim(Name) == 0 ) then
    Check_Name  =   Set_Default_FileName()
  else
    Check_Name  =   Replace_Character( trim(Name), ' ', '_' )
  end if
End Function

Function Set_Default_FileName() result(FileName)
  use GPF_Parameters            ,only:  KEY_figure
  character(:)  ,allocatable                                            ::  FileName
  character(5)                                                          ::  Char_NFile
  integer                                                               ::  NFile=0
  NFile       =   NFile + 1
  write( Char_NFile, "(i5)" ) NFile
  FileName    =   trim(KEY_figure) // "_" // trim(adjustl(Char_NFile))
End Function

Subroutine PrintOutput( This )
  type(GPF_Output_Type)                                 ,intent(in)     ::  This
  call Logger%Write( "-> HardCopy      = ", This%HardCopy    )
  call Logger%Write( "-> FileName      = ", This%FileName    )
  call Logger%Write( "-> Name          = ", This%Name        )
  call Logger%Write( "-> Extension     = ", This%Extension   )
  call Logger%Write( "-> DataFile      = ", This%DataFile    )
  call Logger%Write( "-> Directory     = ", This%Directory   )
  call Logger%Write( "-> CommandFile   = ", This%CommandFile )
End Subroutine

End SubModule