function( add_target_sources targetName )

  if ( NOT $<TARGET_EXISTS:${targetName}> ) 
    message( FATAL_ERROR 
      "Did not find specified target to add sources to: ${targetName}"
    )
  endif()

  foreach( arg IN LISTS ARGN )
    if( EXISTS arg )
      target_sources( ${targetName}
        PRIVATE ${arg}
      )
    else()
      message( FATAL_ERROR
        "Source file not found for target ${targetName}: ${arg}"
      )
    endif()
  endforeach()

endfunction()

