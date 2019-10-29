Module GPF_Multiplot_Class

  use GPF_Parameters            ,only:  DbgUnit, rkp
  use GPF_Command_Class         ,only:  GPF_Command_Type

  use GPF_Multiplot_Title_Class ,only:  GPF_Multiplot_Title_Type
  use GPF_Multiplot_Layout_Class,only:  GPF_Multiplot_Layout_Type

  implicit none

  private

  public  ::  GPF_Multiplot_Type

  Type  ,extends(GPF_Command_Type) ::  GPF_Multiplot_Type
    private
    type(GPF_Multiplot_Title_Type)                      ::  Title                                           !< Title object
    type(GPF_Multiplot_Layout_Type)                     ::  Layout                                          !< Layout object
    character(:)        ,allocatable                    ::  Command_Initialize                              !< Initialization command
    character(:)        ,allocatable                    ::  Command_Finalize                                !< Finalization command
  contains
    private
    procedure   ,public   ::  Initialize      =>  Initialize_Multiplot                            !< Writes the Multiplot initialization command
    procedure   ,public   ::  Write_Comment   =>  Write_Multiplot_Comment                         !< Writes the Multiplot comments
    procedure   ,public   ::  Finalize        =>  Finalize_Multiplot                              !< Writes the Multiplot finalization command
    procedure   ,public   ::  Set_Command     =>  Set_Multiplot_Command
  End Type

  Interface             GPF_Multiplot_Type
    Module Procedure    Construct_Multiplot
  End Interface

  contains

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         CONSTRUCTOR PROCEDURES                                             *
! **************************************************************************************************************
! **************************************************************************************************************

Function Construct_Multiplot( Debug,                                                    &
                Multiplot_Title, Multiplot_Title_Font_Name, Multiplot_Title_Font_Size, Multiplot_Title_Enhanced,        &                       ! Arguments related to the Multiplot title
                Multiplot_NRows, Multiplot_NCols,                                       &                       ! Arguments related to the Multiplot layout dimensions
                Multiplot_Xscale, Multiplot_Yscale,                                     &                       ! Arguments related to the Multiplot layout scales
                Multiplot_Xoffset, Multiplot_Yoffset, Graphs )                          &                       ! Arguments related to the Multiplot layout offsets
                result(This)

  use GPF_Graph_Class           ,only:  GPF_Graph_Type

  implicit none

  type(GPF_Multiplot_Type)                                              ::  This                            !< Multiplot object to be constructed
  logical                                     ,optional ,intent(in)     ::  Debug                           !< Debugging indicator
  character(*)                                ,optional ,intent(in)     ::  Multiplot_Title                 !< Title of the multiplot graph
  character(*)                                ,optional ,intent(in)     ::  Multiplot_Title_Font_Name       !< Font name for the multiplot title
  character(*)                                ,optional ,intent(in)     ::  Multiplot_Title_Font_Size       !< Font size for the multiplot title
  logical                                     ,optional ,intent(in)     ::  Multiplot_Title_Enhanced        !< Enhanced indicator for the multiplot graph
  integer                                     ,optional ,intent(in)     ::  Multiplot_NRows                 !< Number of rows for the muliplot layout
  integer                                     ,optional ,intent(in)     ::  Multiplot_NCols                 !< Number of colums for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Xscale                !< Scale along X for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Yscale                !< Scale along Y for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Xoffset               !< Offset along X for the muliplot layout
  real(rkp)                                   ,optional ,intent(in)     ::  Multiplot_Yoffset               !< Offset along Y for the muliplot layout
  type(GPF_Graph_Type)  ,dimension(:)         ,optional ,intent(in)     ::  Graphs

  character(*)                                              ,parameter  ::  Keyword='multiplot'             !< Keyword of current command


!   integer               ::  iGraph
  integer       ,target ::  NGraphs
!   logical       ::  i_Graphs_Layout    ! Indicator that all elements in the Graph object have a defined layout (if so, the layout does not need to be added to the Multiplot object)
  integer       ,pointer        ::  Multiplot_NGraphs       ! Number of Graphs
!   integer       ::  Multiplot_NGraphs

  call This%Set_Debug( Debug )                                                                                  ! Setting debugging indicator
  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot]: Entering')")





!   Multiplot_NGraphs     =       2

  Multiplot_NGraphs     =>  null()

  if ( present(Graphs) ) then

    NGraphs        =       size(Graphs)

    if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot]: NGraphs = ',i0)") NGraphs

    if ( NGraphs > 1 ) then
      Multiplot_NGraphs =>  NGraphs
!       This%Presence     =       .True.
!       Multiplot_NGraphs =>  NGraphs

!       i_Graphs_Layout   =       .True.                 ! Initializing the Graphs layout indicator to true
!       do iGraph = 1,size(Graphs)
!         i_Graphs_Layout =       i_Graphs_Layout .and. Graphs(iGraph)%Layout%Get_Presence()
!       end do                                                                                            ! At the end of the loop the i_Graphs_Layout indicator is true if all graphs have a defined layout

!     If at least one graph does not have a defined layout, then the layout option is added by force to the Multiplot layout command
!     using the number of graphs
!     So, we defined a interger pointer variable Multiplot_NGraphs which points to:
!      - the null() variable if
!      - the number of graphs if the forcing in required
!      This variable is then passed to the Multiplot-Layout constructor procedure.


    end if

  end if



  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot]: Calling GPF_Multiplot_Title_Type')")
  This%Title    =                                       &                                                       ! Setting the Multiplot Title object ...
        GPF_Multiplot_Title_Type( Debug,                &                                                       ! ... by calling its constructor
                Multiplot_Title, Multiplot_Title_Font_Name, Multiplot_Title_Font_Size, Multiplot_Title_Enhanced ) ! Arguments related to the Multiplot title

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot]: Calling GPF_Multiplot_Layout_Type')")
  This%Layout   =                                       &                                                       ! Setting the Multiplot Layout object ...
        GPF_Multiplot_Layout_Type( Debug,               &                                                       ! ... by calling its constructor
                Multiplot_NRows, Multiplot_NCols,       &                                                       ! Arguments related to the Multiplot layout dimensions
                Multiplot_Xscale, Multiplot_Yscale,     &                                                       ! Arguments related to the Multiplot layout scales
                Multiplot_Xoffset, Multiplot_Yoffset,   &                                                       ! Arguments related to the Multiplot layout offsets
                Multiplot_NGraphs                       )


  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot]: Calling This%Set_Keyword')")
  call This%Set_Keyword( Keyword )                                                                              ! Setting the command keyword

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot]: Calling Set_Command')")
  call This%Set_Command()                                                                                       ! Setting the Multiplot command string

  This%Presence = This%Title%Get_Presence() .or. This%Layout%Get_Presence()                                     ! Setting the presence indicator

  if (This%i_Debug) write(DbgUnit,"(10x,'[Construct_Multiplot]: Exiting',/)")

End Function

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PUBLIC PROCEDURES                                                  *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Initialize_Multiplot( This, Unit )
  implicit none
  class(GPF_Multiplot_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Multiplot object
  integer                                               ,intent(in)     ::  Unit                            !< File unit number
  if ( len_trim(This%Command_Initialize) /= 0 ) write(Unit,"(/,a,/)") This%Command_Initialize                   ! Writing the initialization command
End Subroutine

Subroutine Write_Multiplot_Comment( This, Unit, iGraph )
  implicit none
  class(GPF_Multiplot_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Multiplot object
  integer                                               ,intent(in)     ::  Unit                            !< File unit number
  integer                                               ,intent(in)     ::  iGraph                          !< Index of current Graph being plotted
  if ( len_trim(This%Command_Initialize) /= 0 ) then
    write(Unit,"('# Graph ',i3)") iGraph
  end if
End Subroutine

Subroutine Finalize_Multiplot( This, Unit )
  implicit none
  class(GPF_Multiplot_Type)                             ,intent(in)     ::  This                            !< Passed-object dummy argument corresponding to the Multiplot object
  integer                                               ,intent(in)     ::  Unit                            !< File unit number
  if ( len_trim(This%Command_Finalize) /= 0 )   write(Unit,"(/a,/)") This%Command_Finalize                      ! Writing the finalization command
End Subroutine

! **************************************************************************************************************
! **************************************************************************************************************
! *                                         PRIVATE PROCEDURES                                                 *
! **************************************************************************************************************
! **************************************************************************************************************

Subroutine Set_Multiplot_Command( This )
  implicit none
  class(GPF_Multiplot_Type)                             ,intent(inout)  ::  This                            !< Passed-object dummy argument corresponding to the Multiplot object
  if (This%i_Debug) write(DbgUnit,"(12x,'[Set_Multiplot_Command]: Entering')")
  This%Command_Initialize       =       ''                                                                      ! Initializing the initialization command string
  This%Command_Finalize         =       ''                                                                      ! Initializing the finalization command string
  if ( This%Title%Presence )    This%Command_Initialize = This%Command_Initialize  // This%Title%Command        ! Setting the multiplot title options if required
  if ( This%Layout%Presence )   This%Command_Initialize = This%Command_Initialize  // This%Layout%Command       ! Setting the multiplot layout options if required
  if ( This%Command_Initialize /= '' )  then                                                                    ! If the initialization command is defined (ie, if it is not an empty string)
    This%Command_Initialize     =       'set '   // This%Keyword // This%Command_Initialize                     ! Setting the multiplot prefix for the initialization command
    This%Command_Finalize       =       'unset ' // This%Keyword                                                ! Setting the multiplot prefix for the finalization command
  end if                                                                                                        ! End if case on initialization command length
  if (This%i_Debug) then
    write(DbgUnit,"(12x,'[Set_Multiplot_Command]: This%Command_Initialize = ',a)") This%Command_Initialize
    write(DbgUnit,"(12x,'[Set_Multiplot_Command]: This%Command_Finalize   = ',a)") This%Command_Finalize
    write(DbgUnit,"(12x,'[Set_Multiplot_Command]: Exiting',/)")
  end if
End Subroutine

End Module