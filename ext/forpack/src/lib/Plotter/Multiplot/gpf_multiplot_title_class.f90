Module GPF_Multiplot_Title_Class

  use GPF_Parameters            ,only:  DbgUnit, rkp
  use GPF_Command_Class         ,only:  GPF_Command_Type

  use GPF_Font_Class            ,only:  GPF_Font_Type

  implicit none

  private

  public  ::  GPF_Multiplot_Title_Type

  Type  ,extends(GPF_Command_Type) ::  GPF_Multiplot_Title_Type
    private
    character(:)        ,allocatable                    ::  Text
    type(GPF_Font_Type)                                 ::  Font                                            !< Terminal Font object
    character(:)        ,allocatable                    ::  Enhanced
  contains
    private
    procedure             ::  Set_Text        =>  Set_Multiplot_Title_Text                        !< Sets the Multiplot-Title text
    procedure             ::  Set_Font        =>  Set_Multiplot_Title_Font                        !< Sets the Multiplot-Title font
    procedure             ::  Set_Enhanced    =>  Set_Multiplot_Title_Enhanced                    !< Sets the Multiplot-Title enhanced indicator
    procedure   ,public   ::  Set_Command     =>  Set_Multiplot_Title_Command                     !< Sets the Multiplot-Title command string
  End Type

  Interface             GPF_Multiplot_Title_Type
    Module Procedure    Construct_Multiplot_Title
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         CONSTRUCTOR PROCEDURES                                             *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Multiplot_Title( Debug, Text, Font_Name, Font_Size, Enhanced ) result (This)

  implicit none

  type(GPF_Multiplot_Title_Type)                                        ::  This                            !< Multiplot-Title object to be constructed
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
  character(*)                                ,optional ,intent(in)     ::  Text                            !< Title text f the multiplot graph
  character(*)                                ,optional ,intent(in)     ::  Font_Name                       !< Font name for the multiplot title
  character(*)                                ,optional ,intent(in)     ::  Font_Size                       !< Font size for the multiplot title
  logical                                     ,optional ,intent(in)     ::  Enhanced                        !< Enhanced indicator for the multiplot graph

  character(*)                                              ,parameter  ::  Keyword='title'                 !< Keyword of current command

  call This%Set_Debug( Debug )                                                                                  ! Setting debugging indicator
  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Title]: Entering')")

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Title]: Calling This%Set_Text')")
  call This%Set_Text( Text )                                                                                    ! Setting the Multiplot-Title text

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Title]: Calling This%Set_Font')")
  call This%Set_Font( Font_Name, Font_Size )                                                                    ! Setting the Multiplot-Title font

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Title]: Calling This%Set_Enhanced')")
  call This%Set_Enhanced( Enhanced )                                                                            ! Setting the Multiplot-Title enhanced indicator

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Title]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Title]: Calling This%Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the Multiplot-Title command string

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot_Title]: Exiting',/)")

End Function


! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Set_Multiplot_Title_Text( This, Text )
  use GPF_Tools                 ,only:  Add_Apostroph
  implicit none
  class(GPF_Multiplot_Title_Type)                       ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Multiplot-Title object
  character(*)                                ,optional ,intent(in)     ::  Text                            !< Title text of the multiplot graph
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Multiplot_Title_Text]: Entering')")
  This%Text     =       ""
  if ( present(Text) ) then                                                                                     ! If present optional input argument
    if ( len_trim(Text) /= 0 ) This%Text = Add_Apostroph( Text ) // " "                                         ! Setting the title text if not a empty string (adding apostroph and a blank space at the end of the string)
  end if                                                                                                        ! End if case on optional input argument presence
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Multiplot_Title_Text]: This%Text = ',a)") This%Text
    write(DbgUnit,"(12x,'[Set_Multiplot_Title_Text]: Exiting',/)")
  end if
End Subroutine

Subroutine Set_Multiplot_Title_Font( This, Font_Name, Font_Size )
  implicit none
  class(GPF_Multiplot_Title_Type)                       ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Multiplot-Title object
  character(*)                                ,optional ,intent(in)     ::  Font_Name                       !< Font name for the multiplot title
  character(*)                                ,optional ,intent(in)     ::  Font_Size                       !< Font size for the multiplot title
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Multiplot_Title_Font]: Entering')")
  This%Font%Command     =       ""
  if ( len_trim(This%Text) /= 0 ) then
    if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Multiplot_Title_Font]: Calling This%Font%Initialize')")
    call This%Font%Initialize( Font_Name, Font_Size, This%i_Debug )                                     ! Setting the font component of the Multiplot-Title object
  end if
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Multiplot_Title_Font]: This%Font%Command = ',a)") This%Font%Command
    write(DbgUnit,"(12x,'[Set_Multiplot_Title_Font]: Exiting',/)")
  end if
End Subroutine



Subroutine Set_Multiplot_Title_Enhanced( This, Enhanced )
  use GPF_Parameters            ,only:  KEY_enhanced
  implicit none
  class(GPF_Multiplot_Title_Type)                       ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Multiplot-Title object
  logical                                     ,optional ,intent(in)     ::  Enhanced                        !< Enhanced indicator for the multiplot graph
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Multiplot_Title_Enhanced]: Entering')")
  This%Enhanced =       ""
  if ( present(Enhanced) ) then
    if ( Enhanced ) This%Enhanced = KEY_enhanced
  end if
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Multiplot_Title_Enhanced]: This%Enhanced = ',a)") This%Enhanced
    write(DbgUnit,"(12x,'[Set_Multiplot_Title_Enhanced]: Exiting',/)")
  end if
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Multiplot_Title_Enhanced]: Exiting',/)")
End Subroutine

Subroutine Set_Multiplot_Title_Command( This )
  implicit none
  class(GPF_Multiplot_Title_Type)                       ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Multiplot-Title object
  character(*)                                              ,parameter  ::  Prefix='title '                 ! Prefix string
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Multiplot_Title_Command]: Entering')")

  This%Command  =       ""
  if ( len_trim(This%Text)         /= 0 )       This%Command = This%Command // This%Text
  if ( len_trim(This%Font%Command) /= 0 )       This%Command = This%Command // This%Font%Command
  if ( len_trim(This%Enhanced)     /= 0 )       This%Command = This%Command // This%Enhanced

  This%Presence =       .False.
  if ( len_trim(This%Command) /= 0 ) then
    This%Presence       =       .True.
    This%Command        =       This%Keyword // ' ' // This%Command
  end if

  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Multiplot_Title_Command]: This%Command = ',a)") This%Command
    write(DbgUnit,"(12x,'[Set_Multiplot_Title_Command]: Exiting',/)")
  end if
end Subroutine

End Module