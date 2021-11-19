cmake_minimum_required(VERSION 3.18)

project(fuuid4
  VERSION 0.1.0
  LANGUAGES Fortran
  )

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

string(COMPARE EQUAL
  "${PROJECT_NAME}" "${CMAKE_PROJECT_NAME}"
  PROJECT_IS_TOP_LEVEL
)

# We need this early on to set the install location of .mod files
include(GNUInstallDirs)

##################################################
# Build options

option(FUUID4_USE_LIBUUID "Enable support for using a system UUID generator" ON)
option(FUUID4_BUILD_EXAMPLES "Build the FUUID4 examples" ${PROJECT_IS_TOP_LEVEL})
option(FUUID4_BUILD_TESTS "Build the FUUID4 tests" ${PROJECT_IS_TOP_LEVEL})
option(FUUID4_DOWNLOAD_PFUNIT "Download pFUnit as part of the configuration" ${FUUID4_BUILD_TESTS})

##################################################
# fuuid4 library

add_library(fuuid4
  src/fuuid4.F90
  )
add_library(fuuid4::fuuid4 ALIAS fuuid4)

# Location of built .mod files
set(fuuid4_module_directory ${CMAKE_CURRENT_BINARY_DIR}/mod)
set(fuuid4_module_install_directory "${CMAKE_INSTALL_INCLUDEDIR}" CACHE STRING
  "Fortran module installation directory")

set_target_properties(fuuid4 PROPERTIES
  Fortran_FORMAT FREE
  Fortran_MODULE_DIRECTORY ${fuuid4_module_directory}
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib
  ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib
  )

target_include_directories(fuuid4 PUBLIC
  $<BUILD_INTERFACE:${fuuid4_module_directory}>
  $<INSTALL_INTERFACE:${fuuid4_module_install_directory}>
  )

if (FUUID4_USE_LIBUUID)
  # First call quiet as we fallback to our own generator
  find_package(Libuuid REQUIRED)
  if (Libuuid_FOUND)
    find_package(Libuuid REQUIRED) # Repeat find call to report version found
    target_link_libraries(fuuid4 PUBLIC Libuuid::libuuid)
    target_compile_definitions(fuuid4 PRIVATE FUUID4_HAS_LIBUUID)
  else()
    message(STATUS "libuuid not found, using fallback UUID generator")
    set(FUUID4_USE_LIBUUID FALSE)
  endif()
endif()

##################################################
# Examples and tests

if (FUUID4_BUILD_EXAMPLES)
  add_subdirectory(examples)
endif()

if (FUUID4_BUILD_TESTS)
  if (FUUID4_DOWNLOAD_PFUNIT)
    message(STATUS "Downloading pFUnit...")
    include(FetchContent)
    set(FUUID4_DOWNLOAD_PFUNIT_URL "https://github.com/Goddard-Fortran-Ecosystem/pFUnit"
      CACHE STRING "URL of pFUnit repository")

    set(FUUID4_PFUNIT_LOCATION "${FUUID4_DOWNLOAD_PFUNIT_URL}")

    FetchContent_Declare(
      pfunit
      GIT_REPOSITORY ${FUUID4_DOWNLOAD_PFUNIT_URL}
      GIT_TAG        main
      )

    set(SKIP_OPENMP YES CACHE BOOL "" FORCE)
    set(SKIP_MPI YES CACHE BOOL "" FORCE)
    # Workaround for bug in current main at time of commit
    set(BUILD_SHARED ${BUILD_SHARED_LIBS})
    if (NOT CMAKE_Fortran_FLAGS)
      if (CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
        # Workaround for an issue in pFUnit: preprocessor macro
        # expansion of the current source directory, which may
        # truncate the line
        set(CMAKE_Fortran_FLAGS "-ffree-line-length-none" CACHE STRING "" FORCE)
        message(STATUS "Setting CMAKE_Fortran_FLAGS = ${CMAKE_Fortran_FLAGS}")
      endif()
    endif()
    FetchContent_MakeAvailable(pfunit)
  else()
    find_package(PFUNIT REQUIRED)
    set(FUUID4_PFUNIT_LOCATION "${PFUNIT_ROOT}")
  endif()

  include(CTest)
  enable_testing()

  add_custom_target(build-check)
  add_pfunit_test(fuuid4_tests "${CMAKE_CURRENT_SOURCE_DIR}/tests/test_uuid.pf" "" "")
  target_link_libraries(fuuid4_tests fuuid4::fuuid4)
  add_dependencies(build-check fuuid4_tests)

  add_custom_target(check
    COMMAND ctest --output-on-failure
    DEPENDS fuuid4_tests
    )
  add_dependencies(check build-check)
else()
  set(FUUID4_PFUNIT_LOCATION "Not used")
endif()

##################################################
# Installation

install(TARGETS fuuid4
  EXPORT fuuid4Targets
  LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  )
install(DIRECTORY "${fuuid4_module_directory}/"
  DESTINATION "${fuuid4_module_install_directory}"
  FILES_MATCHING PATTERN "*.mod"
  )

include(CMakePackageConfigHelpers)
write_basic_package_version_file(
  fuuid4ConfigVersion.cmake
  VERSION ${PACKAGE_VERSION}
  COMPATIBILITY SameMajorVersion
  )

install(EXPORT fuuid4Targets
  FILE fuuid4Targets.cmake
  NAMESPACE fuuid4::
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/fuuid4"
  )

configure_package_config_file(cmake/fuuid4Config.cmake.in fuuid4Config.cmake
  INSTALL_DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/fuuid4Config.cmake"
  )

# CMake configuration files
install(
  FILES
      "${CMAKE_CURRENT_BINARY_DIR}/fuuid4Config.cmake"
      "${CMAKE_CURRENT_BINARY_DIR}/fuuid4ConfigVersion.cmake"
      "${CMAKE_CURRENT_SOURCE_DIR}/cmake/FindLibuuid.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/fuuid4"
  )

export(EXPORT fuuid4Targets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/fuuid4Targets.cmake"
  NAMESPACE fuuid4::
  )

export(PACKAGE fuuid4)

