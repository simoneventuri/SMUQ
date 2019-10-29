Module GPF_Movie_Class

  use GPF_Parameters            ,only:  DbgUnit
  use GPF_Command_Class         ,only:  GPF_Command_Type
  use GPF_Multiplot_Class       ,only:  GPF_Multiplot_Type
  use GPF_Graph_Class           ,only:  GPF_Graph_Type
  use GPF_Arrow_Class           ,only:  GPF_Arrow_Type

!   use GPF_Term_Class            ,only:  GPF_Term_Type
!   use GPF_Output_Class          ,only:  GPF_Output_Type

  implicit none

  private

  public  ::  GPF_Movie_Type

  Type  ,extends(GPF_Command_Type)                              ::  GPF_Movie_Type
    private
!     character(:)        ,allocatable                            ::  Global_Options
!     character(:)        ,allocatable                            ::  InFile_Options
!     character(:)        ,allocatable                            ::  OutFile_Options
!     character(:)        ,allocatable                            ::  InFile
!     character(:)        ,allocatable                            ::  OutFile
  contains
    private
    procedure   ,public   ::  Create          =>  Create_Movie                            !< Creates a movie
    procedure   ,public   ::  Write           =>  Write_Movie_Command                     !< Sets the Movie command, ie the script file
    procedure   ,public   ::  Set_Command     =>  Set_Movie_Command                       !< Sets the Movie command


    procedure             ::  Set_InFile      =>  Set_Movie_InFile                        !< Sets the movie input files

  End Type

!   call Movie%Create(
! !   ffmpeg [global options] [[infile options][-i infile]]... {[outfile options] outfile}...

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Create_Movie( This, Debug,   &
                InFile                  )
  implicit none
  class(GPF_Movie_Type)                                 ,intent(out)    ::  This                            !< Passed-object dummy argument corresponding to the Movie object to be created
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
  character(*)                                ,optional ,intent(in)     ::  InFile                          !< Input file corresponding to the images used to create the movie

  call This%Set_Debug( Debug )                                                                                  ! Setting debugging indicator
  if (This%i_Debug) write(DbgUnit,"(4x,'[Create_Movie]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(4x,'[Create_Movie]: Calling This%Set_InFile')")
  call This%Set_InFile( InFile )                                                                                ! Setting the input files

! ffmpeg -i Spark_/Plot/Test_MultiPlot_%03d.png  -vcodec huffyuv output.avi


  if (This%i_Debug) write(DbgUnit,"(4x,'[Create_Movie]: Exiting',/)")
End Subroutine

Subroutine Set_Movie_InFile( This, InFile )
  implicit none
  class(GPF_Movie_Type)                                 ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Movie object
  character(*)                                ,optional ,intent(in)     ::  InFile                          !< Input file corresponding to the images used to create the movie
  if (This%i_Debug) write(DbgUnit,"(4x,'[Set_Movie_InFile]: Entering')")


  if ( present(InFile) ) then

  else
  end if


! ffmpeg -i Spark_/Plot/Test_MultiPlot_%03d.png  -vcodec huffyuv output.avi


  if (This%i_Debug) write(DbgUnit,"(4x,'[Set_Movie_InFile]: Exiting',/)")
End Subroutine




Subroutine Write_Movie_Command( This, Unit )
  implicit none
  class(GPF_Movie_Type)                                 ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Movie object
  integer                                               ,intent(in)     ::  Unit                            !< File unit number of the file where to command has to be written
  if (This%i_Debug) write(DbgUnit,"(6x,'[Write_Movie_Command]: Entering')")
  if (This%i_Debug) write(DbgUnit,"(6x,'[Write_Movie_Command]: Unit = ',g0)") Unit
!   if ( len_trim(This%Command) /= 0 ) write(Unit,"(a)") This%Command
!   if (This%i_Debug) write(DbgUnit,"(6x,'[Write_Movie_Command]: Opening command file')")
!   open(NewUnit=This%Unit_Command, File=This%File%GetCommandFileName(), Status='REPLACE', Iostat=ios)               ! Opening the command file
!
!   if (This%i_Debug) write(DbgUnit,"(6x,'[Write_Movie_Command]: Calling Write_Header')")
!   call Write_Header( This%Unit_Command )                                                                       ! Writing the file header
!
!   if (This%i_Debug) write(DbgUnit,"(6x,'[Write_Movie_Command]: Calling This%File%Write')")
!   call This%File%Write( This%Unit_Command )                                                                     ! Writing the File object's commands in the command file
!
!   if (This%i_Debug) write(DbgUnit,"(6x,'[Write_Movie_Command]: Calling This%Multiplot%Initialize')")
!   call This%Multiplot%Initialize( This%Unit_Command )                                                           ! Writing the Multiplot initialization command
!
!   do iGraph = 1,size(This%Graph)
!
!     if (This%i_Debug) write(DbgUnit,"(6x,'[Write_Movie_Command]: iGraph = ',i0)") iGraph
!
!     if (This%i_Debug) write(DbgUnit,"(6x,'[Write_Movie_Command]: Calling This%Multiplot%Write_Comment')")
!     call This%Multiplot%Write_Comment( This%Unit_Command, iGraph )                                              ! Writing the Multiplot comment
!
!     if (This%i_Debug) write(DbgUnit,"(6x,'[Write_Movie_Command]: Calling This%Graph(iGraph)%Write')")
!     call This%Graph(iGraph)%Write( This%Unit_Command )                                                          ! Writing the Graph commands in the command file
!
!   end do
!
!   if (This%i_Debug) write(DbgUnit,"(6x,'[Write_Movie_Command]: Calling This%Multiplot%Finalize')")
!   call This%Multiplot%Finalize( This%Unit_Command )                                                             ! Writing the Multiplot finalization command
!
!   write(This%Unit_Command,"(/,a)") '  q'                                                                        ! Writing the command to exit gnuplot

  if (This%i_Debug) write(DbgUnit,"(6x,'[Write_Movie_Command]: Exiting',/)")
End Subroutine

Subroutine Set_Movie_Command( This )
  implicit none
  class(GPF_Movie_Type)                                ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Movie object
  if (This%i_Debug) write(DbgUnit,"(6x,'[Set_Movie_Command]: Nothing done')")
End Subroutine

End Module