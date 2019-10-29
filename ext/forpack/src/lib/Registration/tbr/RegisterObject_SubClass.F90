SubModule(Registration_Class) Registration_RegisterObject_SubClass

  implicit none

  contains

Module Procedure RegisterRegisterObject
!   use SelfRegistration_Module    ,only:  RegisterObjectsList
  call RegisterObjectsList%Add( This )
End Procedure

Module Procedure GetRegisterObjectName
  call This%GetProperties( Name=Name )
End Procedure

! Module Procedure GetRegisterObjectCategory
!   call This%GetProperties( Category=Category )
! End Procedure

Module Procedure GetRegisterObjectDescription
  call This%GetProperties( Desc=Desc )
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

! This procedure returns the version associated to a RegisterObject.
! The actual value is to be implemented by the child objects in the "GetProperties"
! deferred type-bound procedure. Setting a value for this data field is optional and
! so it can be omitted from in the implementation of the child object. Thus, if the
! child object does not set its value in the "GetProperties" procedure we set by
! default an empty string.
Module Procedure GetRegisterObjectVersion
  call This%GetProperties( Version=Version )
  if ( .Not. allocated(Version) ) Version = ""
End Procedure

! This procedure returns the institution associated to a RegisterObject.
! The actual value is to be implemented by the child objects in the "GetProperties"
! deferred type-bound procedure. Setting a value for this data field is optional and
! so it can be omitted from in the implementation of the child object. Thus, if the
! child object does not set its value in the "GetProperties" procedure we set by
! default an empty string.
Module Procedure GetRegisterObjectInstitution
  call This%GetProperties( Institution=Institution )
  if ( .Not. allocated(Institution) ) Institution = ""
End Procedure

Module Procedure GetRegisterObjectHelp
  use String_Library            ,only:  EmptyString
  call This%GetProperties( Help=Help )
  if ( .Not. allocated(Help) ) call EmptyString(Help)
End Procedure

End SubModule