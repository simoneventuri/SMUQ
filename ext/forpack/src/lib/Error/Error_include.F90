
Module Procedure FinalizeError
  This%Critical    =   .True.
  if ( allocated( This%Units ) ) deallocate( This%Units )
  call This%Title%Free()
  call This%Message%Free()
End Procedure

Module Procedure ErrorOpen
  use String_Library      ,only:  Convert_To_String
  logical                                                               ::  File_Unit_Known
  logical                                                               ::  File_Name_Known
  character(:)  ,allocatable                                            ::  File_Unit
  character(:)  ,allocatable                                            ::  File_Name
  File_Unit       =     Get_FileUnit( Unit=Unit, File=File )
  File_Name       =     GetFileName( Unit=Unit, File=File )
  File_Unit_Known =     ( len_trim(File_Unit) /= 0 )
  File_Name_Known =     ( len_trim(File_Name) /= 0 )
  call This%Set_Title( "Error while opening a file" )
  if ( present(Line)     ) call This%Add_Line( Line  )
  if ( present(Lines)    ) call This%Add_Line( Lines )
  if ( File_Unit_Known   ) call This%Add_Line( "File unit:         " // File_Unit )
  if ( File_Name_Known   ) call This%Add_Line( "File name:         " // File_Name )
  if ( present(iostat)   ) call This%Add_Line( "IO status:         " // Convert_To_String(iostat) )
  if ( present(ProcName) ) call This%Add_Line( "Calling procedure: " // trim(ProcName) )
  call This%Raise()
End Procedure

! Module Procedure ErrorRead
!   use String_Library      ,only:  Convert_To_String
! !   logical                                                               ::  File_Unit_Known
!   logical                                                               ::  File_Name_Known
! !   character(:)  ,allocatable                                            ::  File_Unit
!   character(:)  ,allocatable                                            ::  File_Name
! !   File_Unit       =     Get_FileUnit( Unit=Unit, File=File )
!   File_Name       =     GetFileName( Unit=Unit, File=File )
! !   File_Unit_Known =     ( len_trim(File_Unit) /= 0 )
!   File_Name_Known =     ( len_trim(File_Name) /= 0 )
!   call This%Set_Title( "Error reading file" )
!   if ( present(Line)     )    call This%Add_Line( Line  )
!   if ( present(Lines)    )    call This%Add_Line( Lines )
! !   if ( File_Unit_Known   ) call This%Add_Line( "File unit:         " // File_Unit )
!   if ( present(LineNumber) )  call This%Add_Line( "-> Line number:       " // Convert_To_String(LineNumber) )
! !   if ( present(LineCon)    call This%Add_Line( "-> Line content:      " // Convert_To_String(LineNumber) )
!   if ( File_Name_Known   )    call This%Add_Line( "-> File name:         " // File_Name )
!   if ( present(Status)   )    call This%Add_Line( "-> IO status:         " // Convert_To_String(Status) )
!   if ( present(ProcName) )    call This%Add_Line( "-> Calling procedure: " // trim(ProcName) )
!   call This%Raise()
! End Procedure

Module Procedure ErrorRead
  use String_Library      ,only:  Convert_To_String
  logical                                                               ::  HasFileName
  character(:)  ,allocatable                                            ::  FileName
  FileName      =   GetFileName( Unit=Unit, File=File )
  HasFileName   =   ( len_trim(FileName) /= 0 )
  call This%Set_Title( "Error reading file" )
  if ( present(Message)     ) call This%Add_Line( Message )
  if ( present(Messages)    ) call This%Add_Line( Messages )
  if ( HasFileName          ) call This%Add_Line( "-> File name:         " // FileName )
  if ( present(LineNumber)  ) call This%Add_Line( "-> Line number:       " // Convert_To_String(LineNumber) )
  if ( present(LineContent) ) call This%Add_Line( "-> Line content:      " // trim(LineContent) )
  if ( present(Status)      ) call This%Add_Line( "-> IO status:         " // Convert_To_String(Status) )
  if ( present(ProcName)    ) call This%Add_Line( "-> Calling procedure: " // trim(ProcName) )
  if ( present(SourceLoc)   ) call This%Add_Line( "-> Source location:   " // trim(SourceLoc) )
  call This%Raise()
End Procedure

Module Procedure ErrorWrite
  use String_Library      ,only:  Convert_To_String
  logical                                                               ::  File_Unit_Known
  logical                                                               ::  File_Name_Known
  character(:)  ,allocatable                                            ::  File_Unit
  character(:)  ,allocatable                                            ::  File_Name
  File_Unit       =     Get_FileUnit( Unit=Unit, File=File )
  File_Name       =     GetFileName( Unit=Unit, File=File )
  File_Unit_Known =     ( len_trim(File_Unit) /= 0 )
  File_Name_Known =     ( len_trim(File_Name) /= 0 )
  call This%Set_Title( "Error while writing to a file" )
  if ( present(Line)     ) call This%Add_Line( Line  )
  if ( present(Lines)    ) call This%Add_Line( Lines )
  if ( File_Unit_Known   ) call This%Add_Line( "File unit:         " // File_Unit )
  if ( File_Name_Known   ) call This%Add_Line( "File name:         " // File_Name )
  if ( present(iostat)   ) call This%Add_Line( "IO status:         " // Convert_To_String(iostat) )
  if ( present(ProcName) ) call This%Add_Line( "Calling procedure: " // trim(ProcName) )
  call This%Raise()
End Procedure

Module Procedure ErrorRewind
  use String_Library      ,only:  Convert_To_String
  logical                                                               ::  File_Unit_Known
  logical                                                               ::  File_Name_Known
  character(:)  ,allocatable                                            ::  File_Unit
  character(:)  ,allocatable                                            ::  File_Name
  File_Unit       =     Get_FileUnit( Unit=Unit, File=File )
  File_Name       =     GetFileName( Unit=Unit, File=File )
  File_Unit_Known =     ( len_trim(File_Unit) /= 0 )
  File_Name_Known =     ( len_trim(File_Name) /= 0 )
  call This%Set_Title( "Error while rewinding a file" )
  if ( present(Line)     ) call This%Add_Line( Line  )
  if ( present(Lines)    ) call This%Add_Line( Lines )
  if ( File_Unit_Known   ) call This%Add_Line( "File unit:         " // File_Unit )
  if ( File_Name_Known   ) call This%Add_Line( "File name:         " // File_Name )
  if ( present(iostat)   ) call This%Add_Line( "IO status:         " // Convert_To_String(iostat) )
  if ( present(ProcName) ) call This%Add_Line( "Calling procedure: " // trim(ProcName) )
  call This%Raise()
End Procedure

Module Procedure ErrorClose
  use String_Library      ,only:  Convert_To_String
  logical                                                               ::  File_Unit_Known
  logical                                                               ::  File_Name_Known
  character(:)  ,allocatable                                            ::  File_Unit
  character(:)  ,allocatable                                            ::  File_Name
  File_Unit       =     Get_FileUnit( Unit=Unit, File=File )
  File_Name       =     GetFileName( Unit=Unit, File=File )
  File_Unit_Known =     ( len_trim(File_Unit) /= 0 )
  File_Name_Known =     ( len_trim(File_Name) /= 0 )
  call This%Set_Title( "Error while closing a file" )
  if ( present(Line)     ) call This%Add_Line( Line  )
  if ( present(Lines)    ) call This%Add_Line( Lines )
  if ( File_Unit_Known   ) call This%Add_Line( "File unit:         " // File_Unit )
  if ( File_Name_Known   ) call This%Add_Line( "File name:         " // File_Name )
  if ( present(iostat)   ) call This%Add_Line( "IO status:         " // Convert_To_String(iostat) )
  if ( present(ProcName) ) call This%Add_Line( "Calling procedure: " // trim(ProcName) )
  call This%Raise()
End Procedure

Module Procedure ErrorAllocate
  use String_Library      ,only:  Convert_To_String
  call This%Set_Title( "Error while allocating a variable" )
  if ( present(Line)     ) call This%Add_Line( Line  )
  if ( present(Lines)    ) call This%Add_Line( Lines )
  if ( present(Name)     ) call This%Add_Line( "Variable name:     " // Name )
  if ( present(ProcName) ) call This%Add_Line( "Calling procedure: " // trim(ProcName) )
  if ( present(Stat)     ) call This%Add_Line( "Error status:      " // Convert_To_String(Stat) )
  if ( present(ErrMsg)   ) call This%Add_Line( "Error message:     " // trim(ErrMsg) )
  call This%Raise()
End Procedure

Module Procedure ErrorDeallocate
  use String_Library      ,only:  Convert_To_String
  call This%Set_Title( "Error while deallocating a variable" )
  if ( present(Line)     ) call This%Add_Line( Line  )
  if ( present(Lines)    ) call This%Add_Line( Lines )
  if ( present(Name)     ) call This%Add_Line( "Variable name:     " // Name )
  if ( present(ProcName) ) call This%Add_Line( "Calling procedure: " // trim(ProcName) )
  if ( present(Stat)     ) call This%Add_Line( "Error status:      " // Convert_To_String(Stat) )
  if ( present(ErrMsg)   ) call This%Add_Line( "Error message:     " // trim(ErrMsg) )
  call This%Raise()
End Procedure

Module Procedure ErrorNotImplemented_0d
  call This%Raise( Line=Line, Title="Not implemented", Unit=Unit, Units=Units, ProcName=ProcName )
End Procedure

Module Procedure RaiseErrorGeneric

! # ifdef USE_PFUNIT
!   use pfunit_mod
! # endif
  use String_Library      ,only:  Convert_To_String

  integer                                                               ::  iUnit                             ! Index of file unit numbers
  integer                                                               ::  iLine                             ! Index of line
  integer       ,dimension(:) ,allocatable                              ::  ErrUnit                           ! List of file unit numbers
  type(Text_Type)                                                ::  Message                           ! Locall error message

! ==============================================================================================================
!   DEALING WITH THE OPTIONAL INPUT ARGUMENTS
! ==============================================================================================================
  if ( present(Title)    ) call This%Set_Title( Title )
  if ( present(Line)     ) call This%Add_Line( Line  )
  if ( present(Lines)    ) call This%Add_Line( Lines )
!   if ( present(ProcName) ) call This%Add_Line( "Calling procedure: " // trim(ProcName) )
  call SetCallingProcedure( This, ProcName )
#ifdef COARRAY
  call This%Add_Line( "Images:            " // Convert_To_String(This_Image()) // "/" // Convert_To_String(Num_Images()) )
#endif
! ==============================================================================================================


! ==============================================================================================================
!   SETTING THE FILE UNITS WHERE THE ERROR HAS TO BE WRITTEN
! ==============================================================================================================
  call This%Get_Units( ErrUnit, Unit, Units )                                                                   ! Getting the error units
! ==============================================================================================================


! ==============================================================================================================
!   SETTING THE ERROR MESSAGE
! ==============================================================================================================
  if (This%Title%Is_Set) then
    call Message%AddLine( This%Title%Value )
    call Message%AddFrame( FChar='#', HSpace=1 )
    call Message%AddLine( "" )
  end if
  call Message%AddLine( This%Message )
  call Message%AddFrame( FChar='#', Space=1 )
  call Message%AddLine( "", At_Start = .True. )
  call Message%AddLine( "", At_End   = .True. )
  call Message%AddPrefix( " " )
! ==============================================================================================================


! ==============================================================================================================
!   WRITING THE ERROR MESSAGE
! ==============================================================================================================
  do iUnit = 1,size(ErrUnit)
    do iLine = 1,Message%GetNumberOfLines()
      write(ErrUnit(iUnit),"(a)") Message%GetLine(iLine)
    end do
    if (This%Critical) write(ErrUnit(iUnit),"(/,a)") "Stopping the code"
  end do
! ==============================================================================================================


! ==============================================================================================================
!   STOPPING THE CODE
! ==============================================================================================================
  if (This%Critical) then
! #   ifdef USE_PFUNIT
!       call throw( This%Title%Value )
! #   else
      error stop
! #   endif
  end if


! ==============================================================================================================
!   CLEANING-UP THE ERROR OBJECT
! ==============================================================================================================
  call This%Title%Free()
  call This%Message%Free()
! ==============================================================================================================

End Procedure

Module Procedure RaiseErrorFromChar0D
  call This%Raise( Line=ErrMsg, Title=Title, Unit=Unit, Units=Units, ProcName=ProcName )
End Procedure

Module Procedure RaiseErrorFromChar1D
  call This%Raise( Lines=ErrMsg, Title=Title, Unit=Unit, Units=Units, ProcName=ProcName )
End Procedure

Module Procedure RaiseErrorFromText
  call This%Raise( Lines=ErrMsg%GetLines(), Title=Title, Unit=Unit, Units=Units, ProcName=ProcName )
End Procedure

Module Procedure Set_Title
  call This%Title%Set_Value( trim(Title) )
End Procedure

! This procedure get the length of the error message.
Module Procedure Get_Error_Length
  Length        =       0
  if ( This%Title%Is_Set ) Length = This%Title%Length()
  if ( This%Message%GetNumberOfLines() /= 0 ) then
    Length      =       max( Length , This%Message%GetLength() )
  end if
End Procedure

Module Procedure Add_Error_Line_0d
  call This%Message%AddLine( Line )
End Procedure

Module Procedure Add_Error_Line_1d
  use String_Library      ,only:  VecTrim
  call This%Message%AddLine( VecTrim(Lines) )
End Procedure

Module Procedure SetErrorOutputUnit_0d
  if ( allocated(This%Units) ) deallocate(This%Units)
  allocate( This%Units , source = [Unit] )
End Procedure

Module Procedure SetErrorOutputUnit_1d
  if ( allocated(This%Units) ) deallocate(This%Units)
  allocate( This%Units , source = Units )
End Procedure

Module Procedure Set_Critical
  This%Critical   =     Critical
End Procedure

Module Procedure Get_Units
  use ,intrinsic :: iso_fortran_env ,only:  Error_Unit
  if ( present(Unit) .and. present(Units) ) then
    allocate( ErrUnit, source = [Unit,Units] )
  else if ( present(Unit) .and. .Not.present(Units) ) then
    allocate( ErrUnit, source = [Unit] )
  else if ( .Not.present(Unit) .and. present(Units) ) then
    allocate( ErrUnit, source = Units )
  else if ( .Not.present(Unit) .and. .Not.present(Units) ) then
    if ( allocated(This%Units) ) then
      allocate( ErrUnit, source = This%Units )
    else
      allocate( ErrUnit, source = [Error_Unit] )
    end if
  end if
End Procedure


Function Get_FileUnit( Unit, File ) result(FileUnit)
  use String_Library      ,only:  Convert_To_String
  integer                                     ,optional ,intent(in)     ::  Unit
  character(*)                                ,optional ,intent(in)     ::  File
  character(:)  ,allocatable                                            ::  FileUnit
  integer                                                               ::  Local_Unit
  Local_Unit    =     0
  if ( present(Unit) ) then
    Local_Unit  =     Unit
  else
    if ( present(File) ) then
      inquire( File=trim(File), Number=Local_Unit )                                                               ! Getting the file unit from its name
    end if
  end if
  FileUnit      =     ""
  if ( Local_Unit /= 0 ) FileUnit = Convert_To_String(Local_Unit)
End Function

Function GetFileName( File, Unit ) result(FileName)
  character(*)                                ,optional ,intent(in)     ::  File
  integer                                     ,optional ,intent(in)     ::  Unit
  character(:)  ,allocatable                                            ::  FileName
  FileName            =     ""
  if ( present(File) ) then
    FileName          =     trim(File)
  else
    if ( present(Unit) ) then
      inquire( Unit=Unit, Name=FileName )
    end if
  end if
End Function



Subroutine SetCallingProcedure( This, ProcName )
  use String_Library            ,only:  Parse
  type(Error_Type)                                      ,intent(inout)  ::  This
  character(*)                                ,optional ,intent(in)     ::  ProcName
!   character(*)                                              ,parameter  ::  Prefix = "└> "
  character(*)                                              ,parameter  ::  Prefix = "-> "
  integer                                                               ::  i
  character(:)  ,allocatable                                            ::  ListProcName(:), Line
  if ( .Not. present(ProcName) ) return
!   call This%Add_Line( "Calling procedure: " // trim(ProcName) )
  call Parse( ProcName, ">", ListProcName )
  if ( size(ListProcName) == 1 ) then
    call This%Add_Line( "Calling procedure: " // trim(ProcName) )
  else
    call This%Add_Line( "Calling procedure:" )
    do i = 1,size(ListProcName)
      Line    =   " " // repeat("   ",i-1) // Prefix // trim((ListProcName(i)))
      call This%Add_Line( Line )
    end do
  end if
End Subroutine
