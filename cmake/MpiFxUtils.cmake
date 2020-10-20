# Register custom commands for processing source files with fypp (.fpp -> .f90)
#
# Args:
#     oldfiles [in]: List of files to preprocess (must have .fpp suffix)
#     newfiles [out]: List of preprocessed files (will have .f90 suffix).
#
function(fypp_preprocess oldfiles newfiles)

  set(_newfiles)
  foreach(oldfile IN LISTS oldfiles)
    string(REGEX REPLACE "\\.fpp" ".f90" newfile ${oldfile})
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${newfile}
      COMMAND ${FYPP} ${FYPP_FLAGS} ${CMAKE_CURRENT_SOURCE_DIR}/${oldfile} ${CMAKE_CURRENT_BINARY_DIR}/${newfile}
      MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${oldfile})
    list(APPEND _newfiles ${CMAKE_CURRENT_BINARY_DIR}/${newfile})
  endforeach()
  set(${newfiles} ${_newfiles} PARENT_SCOPE)

endfunction()


# Returns the parameters needed to create a pkg-config export file
#
# Args:
#     pkgconfig_requires [out]: Value for the Requires field.
#     pkgconfig_libs [out]: Value for the Libs field.
#     pkgconfig_libs_private [out]: Value for the Libs.private field.
#     pkgconfig_c_flags [out]: Value for the cflags field.
#     pkgconfig_prefix [out]: Value for the installation prefix.
#
function(get_pkgconfig_params pkgconfig_requires pkgconfig_libs pkgconfig_libs_private
    pkgconfig_c_flags)

  set(_pkgconfig_requires)
  
  set(_pkgconfig_libs "-L${CMAKE_INSTALL_FULL_LIBDIR} -lmpifx")
  
  set(_pkgconfig_libs_private "${CMAKE_EXE_LINKER_FLAGS}")
  
  set(_pkgconfig_c_flags "-I${CMAKE_INSTALL_FULL_INCLUDEDIR}/${INSTALL_MODULEDIR}")

  set(${pkgconfig_requires} "${_pkgconfig_requires}" PARENT_SCOPE)
  set(${pkgconfig_libs} "${_pkgconfig_libs}" PARENT_SCOPE)
  set(${pkgconfig_libs_private} "${_pkgconfig_libs_private}" PARENT_SCOPE)
  set(${pkgconfig_c_flags} "${_pkgconfig_c_flags}" PARENT_SCOPE)

endfunction()


# Sets up the build type.
function (setup_build_type)
  set(default_build_type "RelWithDebInfo")
  if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to ${default_build_type} as none was specified")
    set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE STRING "Build type" FORCE)
  endif()
endfunction()
