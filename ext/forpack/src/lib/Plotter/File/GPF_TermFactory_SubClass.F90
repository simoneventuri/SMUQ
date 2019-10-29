SubModule(GPF_TermFactory_Class) GPF_TermFactory_SubClass

  use Logger_Class            ,only:  Logger
  use Utilities_Library       ,only:  GetOptArgValue

  implicit none

  logical               ,parameter      ::  DefaultDebug = .False.

  contains

Module Procedure BuildTerm

  use GPF_Parameters            ,only:  KEY_wxt, KEY_pngcairo, KEY_postscript, KEY_pdf
  use GPF_Term_WXT_Class        ,only:  GPF_Term_WXT_Type
  use GPF_Term_PNGCairo_Class   ,only:  GPF_Term_PNGCairo_Type
  use GPF_Term_POSTSCRIPT_Class ,only:  GPF_Term_POSTSCRIPT_Type
  use GPF_Term_PDF_Class        ,only:  GPF_Term_PDF_Type

  character(*)                                              ,parameter  ::  ProcName = "BuildTerm"
  logical                                                               ::  Dbg
  character(:)  ,allocatable                                            ::  TerminalName

  Dbg   =   GetOptArgValue(DefaultDebug,Debug)
  if (Dbg) call Logger%Entering( ProcName )

  if (Dbg) call Logger%Write( "Calling GetTerminalName" )
  TerminalName  =   GetTerminalName( Extension, TermType )
  if (Dbg) call Logger%Write( "-> TerminalName = ", TerminalName )

  if (Dbg) call Logger%Write( "Allocating Terminal object" )
  select case (TerminalName)
    case (KEY_wxt);         allocate( GPF_Term_WXT_Type         :: Term )
    case (KEY_pngcairo);    allocate( GPF_Term_PNGCairo_Type    :: Term )
    case (KEY_postscript);  allocate( GPF_Term_POSTSCRIPT_Type  :: Term )
    case (KEY_pdf);         allocate( GPF_Term_PDF_Type         :: Term )
  end select
  if (Dbg) call Logger%Write( "-> Term%GetName() = ", Term%GetName() )

  if (Dbg) call Logger%Exiting()

End Procedure


Function GetTerminalName( Extension, TermType ) result(Terminal)
  use GPF_Parameters            ,only:  Terminal_Default, Terminal_Valid, Extension_Default, Extension_Valid, Extension_To_Terminal
  use GPF_Tools                 ,only:  Check_Validity, Find_Position
  character(*)                                          ,intent(in)     ::  Extension
  character(*)                                ,optional ,intent(in)     ::  TermType
  character(:)  ,allocatable                                            ::  Terminal
  character(:)  ,allocatable                                            ::  Extension_
  integer                                                               ::  i
  if ( present(TermType) ) then
    Terminal    =   Check_Validity( TermType, Terminal_Valid, Terminal_Default )
  else
    Terminal    =   Get_Terminal_From_Extension( Extension )
    Extension_  =   Check_Validity( Extension, Extension_Valid, Extension_Default )
    i           =   Find_Position( Extension_, Extension_Valid  )
    Terminal    =   Terminal_Default
    if ( i /= 0 ) Terminal = trim( Extension_To_Terminal(i) )
  end if
End Function

Function Get_Terminal_From_Extension( Term_Ext ) result(Terminal)
  use GPF_Parameters    ,only:  Extension_Default, Extension_Valid, Terminal_Default, Extension_To_Terminal
  use GPF_Tools         ,only:  Find_Position, Check_Validity
  character(*)                                          ,intent(in)     ::  Term_Ext                        !< File Extension used to set terminal type
  character(:)  ,allocatable                                            ::  Terminal                        !< Output terminal type
  logical                                                               ::  i_Debug_Loc                     ! Local debugging indicator
  character(:)  ,allocatable                                            ::  Extension                       ! Local file extension
  integer                                                               ::  iExt                            ! Index of current file extension in the valid extension variable
  Extension     =       Check_Validity( Term_Ext, Extension_Valid, Extension_Default )                          ! Checking input extension validity
  iExt          =       Find_Position( Extension, Extension_Valid  )
  if ( iExt /= 0 ) then
    Terminal    =       trim( Extension_To_Terminal(iExt) )
  else
    Terminal    =       Terminal_Default
  end if
End Function

End SubModule