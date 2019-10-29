SubModule(Registration_Class) Registration_Object_SubClass

  implicit none

!   character(*)  ,parameter  ::  Unknown = "Unknown"

  contains

! This procedure identifies if the input variable "Input" is owned by the
! current object. The input variable "Input" is a polymorphic variable which
! can be either a character string, an InputParameter object or a InputSection
! object. To identify if the current object owns this variables the list of
! input keys associated to the object are first extracted using the the
! procedure "GetProperties". Then, the actual identification depends on the
! actual type of input variable passed-in:
! * Character
!     If the input variable is a character, then this character is direclty
!     processed as explained below.
! * InputParameter_Type
!     If the input variable is a object of type "InputParameter_Type", then
!     the raw value of the parameter is extracted and the resulting string is
!     processed as explained below.
! * InputSection_Type
!     If the input variable is a object of type "InputSection_Type", then
!     the name os the section is extracted and the resulting string is
!     processed as explained below.
! The extracted string is then processed the same way, independenty of the
! type of the input variable. This string is expected to have a function
! format as in "Keyword( ... )". This string is owned by the current object
! if the function name "Keyword" matched any elements in the array of
! characters "InputKeys". This is checked by using the "IsFunction" procedure.
! When calling this procedure, the optional argument "CaseSensitive=.False."
! makes the matching of strings case insensitive. Also, the optional argument
! "NoArg=.True." allows the function string to de defined without any arguments
! and parentesis, as in "Keyword" (This is requied for InputSection_Type).
Module Procedure IdentifiedRegisterObject

  use Logger_Class            ,only:  Logger, LogLevel_NOLOGS, LogLevel_HEAVYDEBUG
  use String_Library          ,only:  IsFunction
  use Input_Library           ,only:  InputSection_Type, InputParameter_Type

  character(*)                                              ,parameter  ::  ProcName="IdentifiedRegisterObject"
  logical                                                               ::  LogOn
  character(:)  ,allocatable                                            ::  CurrentKey, InputKeys(:)

  LogOn   =   .False.
  if ( present(LogLevel) ) then
    call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_HEAVYDEBUG )
    LogOn =   Logger%On()
  end if

  if (LogOn) call Logger%Write( "Getting valid input keys for object '"//This%GetName()//"' of category '"//This%GetCategory()//"'" )
  if (LogOn) call Logger%Write( "-> Calling This%GetInputKeys" )
  InputKeys   =   This%GetInputKeys()
  if (LogOn) call Logger%Write( "-> InputKeys = ", InputKeys )

  if (LogOn) call Logger%Write( "Identifying input variable based in variable type" )
  Identified     =   .False.
  select type (Input)
    type is ( character(*) )
      if (LogOn) call Logger%Write( "-> Input variable type 'character': Input = ", Input )
      CurrentKey    =   Input
    type is ( InputParameter_Type )
      if (LogOn) call Logger%Write( "-> Input variable type 'InputParameter_Type': Input%GetRaw() = ", Input%GetRaw() )
      CurrentKey    =   Input%GetRaw()
    type is ( InputSection_Type )
      if (LogOn) call Logger%Write( "-> Input variable type 'InputSection_Type'" )
      CurrentKey    =   Input%GetName()
  end select

  if (LogOn) call Logger%Write( "-> CurrentKey = ", CurrentKey )
  Identified  =   IsFunction( CurrentKey, InputKeys, NoArg=.True., CaseSensitive=.False. )
  if (LogOn) call Logger%Write( "-> Identified = ", Identified )

  if ( present(LogLevel) ) call Logger%Exiting()

End Procedure

! This procedure register the object to a list of self-registering object.
! If a list is provided in input, then the object is added to this list.
! Otherwise, the object is added to the global list "RegisterObjectsList".
Module Procedure RegisterRegisterObject
  if ( present(List) ) then
    call List%Add( This )
  else
    call RegisterObjectsList%Add( This )
  end if
End Procedure

! This procedure outputs a self-registering object.
! If a list is provided in input, then the object is added to this list.
! Otherwise, the object is added to the global list "RegisterObjectsList".
Module Procedure OutputRegisterObjectToString

  use Utilities_Library       ,only:  AddElementToArray
  use String_Library          ,only:  Inline, Indent, LenTrim

  integer                                                   ,parameter  ::  L=18
  character(L)                                                          ::  Key
  character(:)  ,allocatable                                            ::  Value, Values(:)

  Value   =   This%GetName()
  Key     =   "Name:"
  call AddElementToArray( Key//Value , Strings )

  Value   =   This%GetCategory()
  Key     =   "Category:"
  call AddElementToArray( Key//Value , Strings )

  Value   =   Inline( This%GetInputKeys(), Separator=",", Trimed=.True. )
  if ( len_trim(Value) > 0 ) then
    Key   =   "InputKeys:"
    call AddElementToArray( Key//Value , Strings )
  end if

  Value   =   This%GetAuthor()
  if ( len_trim(Value) > 0 ) then
    Key   =   "Author:"
    call AddElementToArray( Key//Value , Strings )
  end if

  Value   =   This%GetDescription()
  if ( len_trim(Value) > 0 ) then
    Key   =   "Description:"
    call AddElementToArray( Key//Value , Strings )
  end if

  Values  =   This%GetHelp()
  if ( (size(Values) > 0) .and. (LenTrim(Values) > 0) ) then
    Key           =   "Help:"
    call AddElementToArray( trim(Key)        , Strings )
    call AddElementToArray( Indent(Values,2) , Strings )
  end if

End Procedure


Module Procedure GetPropertiesRegisterObject
End Procedure

Module Procedure GetRegisterObjectName
  call This%GetProperties( Name=Name )
  if ( .Not. allocated(Name) ) Name = "<Child>"//This%GetCategory()
End Procedure

Module Procedure GetRegisterObjectDescription
  call This%GetProperties( Description=Description )
  if ( .Not. allocated(Description) ) Description = ""
End Procedure

! This procedure returns the set of input keys associated to a RegisterObject.
! The actual value of these input keys needs to be implemented by the child objects
! in the "GetProperties" deferred type-bound procedure. These keys will be used by
! the application code to identify whether a set of inputs are owned by the current
! objects. This is done in the application code by calling the "Identified" deferred
! type-bound procedure. The 'deferred' attribute means that both the operations of
! setting ot the value of "InputKeys" in the "GetProperties" procedure and the
! identification of the inputs in the "Identified" procedues are delegated to the
! child objects. However, the key points here is that the application code expects
! to get a nicely allocated array of character strings in the variable "InputKeys".
! If the child object does set its value in the "GetProperties" procedure (either
! by mistake or by design), then we will have a problem (seg. fault). This prevent
! this, after calling the "GetProperties" procedure, we check that the returned
! variable is actually allocated, and, if not, we allocated it to a 0-rank array.
Module Procedure GetRegisterObjectInputKeys
  use String_Library            ,only:  EmptyString
  call This%GetProperties( InputKeys=InputKeys )
  if ( .Not. allocated(InputKeys) ) call EmptyString(InputKeys)
End Procedure

! This procedure returns the author associated to a RegisterObject.
! The actual value is to be implemented by the child objects in the "GetProperties"
! deferred type-bound procedure. Setting a value for this data field is optional and
! so it can be omitted from in the implementation of the child object. Thus, if the
! child object does not set its value in the "GetProperties" procedure we set by
! default an empty string.
Module Procedure GetRegisterObjectAuthor
  call This%GetProperties( Author=Author )
  if ( .Not. allocated(Author) ) Author = ""
End Procedure

Module Procedure GetRegisterObjectHelp
  use String_Library            ,only:  EmptyString
  call This%GetProperties( Help=Help )
  if ( .Not. allocated(Help) ) call EmptyString(Help)
End Procedure

End SubModule