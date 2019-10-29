Program ErrorExample

  use Error_Class        ,only:  Error
  use String_Library            ,only:  Convert_To_String, Text_Type

  implicit none

  character(*)  ,parameter    ::    ProcName = "Test_Error_Class"
  integer                     ::    Unit, i
  character(:)  ,allocatable  ::    String
  type(Text_Type)                                                       ::  ErrMsg

  open( NewUnit=Unit, File="error.log", Action="WRITE" )

  call Error%Set_Critical(.False.)
  call Error%Set_FileUnit( [Unit] )

  write(Unit,"('size(Error%Units) = ',g0)") size(Error%Units)
  do i = 1,size(Error%Units)
    write(Unit,"('i = ',g0,3x,'Error%Units(i) = ',g0)") i, Error%Units(i)
  end do


  String    =   'call Error%Raise( "Simple error message with only a single line")'
  write(Unit,"(/,'Test 1: ',a)") String
  call Error%Raise( "Simple error message with only a single line")

  String    =   'call Error%Raise( [ "Simple error message with multiple lines", "sot hat the error message can be", "splitted on several lines"] )'
  write(Unit,"('Test 2: ',a)") String
  call Error%Raise( [ "Simple error message with multiple lines", "sot hat the error message can be", "splitted on several lines"] )

  String    =   'call Error%Raise( [ "Same calling sequence than the previous one", "but adding the ProcName optional argument."], ProcName=ProcName )'
  write(Unit,"('Test 3: ',a)") String
  call Error%Raise( [ "Same calling sequence than the previous one", "but adding the ProcName optional argument."], ProcName=ProcName )

  String    =   'call Error%Raise( [ "Same calling sequence than the previous one", "but adding the Title optional argument."], ProcName=ProcName, Title="Critical error" )'
  write(Unit,"('Test 4: ',a)") String
  call Error%Raise( [ "Same calling sequence than the previous one", "but adding the Title optional argument."], ProcName=ProcName, Title="Critical error" )



        call ErrMsg%AddLine(&
                [ "Unknow internal level model for sub-species "//'N(1)'//" of species "//'N'//"."  ,&
                  "Current value: This%Internal_Level_Model = " // Convert_To_String(1)     ,&
                  "Possible values:                                  "                                                                              ])
        do i = 1,4
          call ErrMsg%AddLine( " * " //Convert_To_String(i)//": blabla" )
        end do
        call Error%Raise( ErrMsg, Title = "Unknow internal level model", ProcName = ProcName )




  close( Unit )

End Program