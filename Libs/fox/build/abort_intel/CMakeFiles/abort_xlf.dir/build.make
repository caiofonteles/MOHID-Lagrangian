# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.11

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /Applications/CMake.app/Contents/bin/cmake

# The command to remove a file.
RM = /Applications/CMake.app/Contents/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/cmake

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/abort_intel

# Include any dependencies generated for this target.
include CMakeFiles/abort_xlf.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/abort_xlf.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/abort_xlf.dir/flags.make

CMakeFiles/abort_xlf.dir/abort_xlf.f90.o: CMakeFiles/abort_xlf.dir/flags.make
CMakeFiles/abort_xlf.dir/abort_xlf.f90.o: /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/cmake/abort_xlf.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/abort_intel/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object CMakeFiles/abort_xlf.dir/abort_xlf.f90.o"
	/usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/cmake/abort_xlf.f90 -o CMakeFiles/abort_xlf.dir/abort_xlf.f90.o

CMakeFiles/abort_xlf.dir/abort_xlf.f90.i: cmake_force
	@echo "Preprocessing Fortran source to CMakeFiles/abort_xlf.dir/abort_xlf.f90.i"
	/usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/cmake/abort_xlf.f90 > CMakeFiles/abort_xlf.dir/abort_xlf.f90.i

CMakeFiles/abort_xlf.dir/abort_xlf.f90.s: cmake_force
	@echo "Compiling Fortran source to assembly CMakeFiles/abort_xlf.dir/abort_xlf.f90.s"
	/usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/cmake/abort_xlf.f90 -o CMakeFiles/abort_xlf.dir/abort_xlf.f90.s

# Object files for target abort_xlf
abort_xlf_OBJECTS = \
"CMakeFiles/abort_xlf.dir/abort_xlf.f90.o"

# External object files for target abort_xlf
abort_xlf_EXTERNAL_OBJECTS =

abort_xlf: CMakeFiles/abort_xlf.dir/abort_xlf.f90.o
abort_xlf: CMakeFiles/abort_xlf.dir/build.make
abort_xlf: CMakeFiles/abort_xlf.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/abort_intel/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking Fortran executable abort_xlf"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/abort_xlf.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/abort_xlf.dir/build: abort_xlf

.PHONY : CMakeFiles/abort_xlf.dir/build

CMakeFiles/abort_xlf.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/abort_xlf.dir/cmake_clean.cmake
.PHONY : CMakeFiles/abort_xlf.dir/clean

CMakeFiles/abort_xlf.dir/depend:
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/abort_intel && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/cmake /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/cmake /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/abort_intel /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/abort_intel /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/abort_intel/CMakeFiles/abort_xlf.dir/DependInfo.cmake
.PHONY : CMakeFiles/abort_xlf.dir/depend

