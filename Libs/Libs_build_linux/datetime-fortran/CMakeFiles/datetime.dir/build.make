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
CMAKE_SOURCE_DIR = /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux

# Include any dependencies generated for this target.
include datetime-fortran/CMakeFiles/datetime.dir/depend.make

# Include the progress variables for this target.
include datetime-fortran/CMakeFiles/datetime.dir/progress.make

# Include the compile flags for this target's objects.
include datetime-fortran/CMakeFiles/datetime.dir/flags.make

datetime-fortran/CMakeFiles/datetime.dir/datetime.f90.o: datetime-fortran/CMakeFiles/datetime.dir/flags.make
datetime-fortran/CMakeFiles/datetime.dir/datetime.f90.o: ../datetime-fortran/datetime.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object datetime-fortran/CMakeFiles/datetime.dir/datetime.f90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/datetime.f90 -o CMakeFiles/datetime.dir/datetime.f90.o

datetime-fortran/CMakeFiles/datetime.dir/datetime.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/datetime.dir/datetime.f90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/datetime.f90 > CMakeFiles/datetime.dir/datetime.f90.i

datetime-fortran/CMakeFiles/datetime.dir/datetime.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/datetime.dir/datetime.f90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/datetime.f90 -o CMakeFiles/datetime.dir/datetime.f90.s

datetime-fortran/CMakeFiles/datetime.dir/mod_clock.f90.o: datetime-fortran/CMakeFiles/datetime.dir/flags.make
datetime-fortran/CMakeFiles/datetime.dir/mod_clock.f90.o: ../datetime-fortran/mod_clock.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building Fortran object datetime-fortran/CMakeFiles/datetime.dir/mod_clock.f90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_clock.f90 -o CMakeFiles/datetime.dir/mod_clock.f90.o

datetime-fortran/CMakeFiles/datetime.dir/mod_clock.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/datetime.dir/mod_clock.f90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_clock.f90 > CMakeFiles/datetime.dir/mod_clock.f90.i

datetime-fortran/CMakeFiles/datetime.dir/mod_clock.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/datetime.dir/mod_clock.f90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_clock.f90 -o CMakeFiles/datetime.dir/mod_clock.f90.s

datetime-fortran/CMakeFiles/datetime.dir/mod_constants.f90.o: datetime-fortran/CMakeFiles/datetime.dir/flags.make
datetime-fortran/CMakeFiles/datetime.dir/mod_constants.f90.o: ../datetime-fortran/mod_constants.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building Fortran object datetime-fortran/CMakeFiles/datetime.dir/mod_constants.f90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_constants.f90 -o CMakeFiles/datetime.dir/mod_constants.f90.o

datetime-fortran/CMakeFiles/datetime.dir/mod_constants.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/datetime.dir/mod_constants.f90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_constants.f90 > CMakeFiles/datetime.dir/mod_constants.f90.i

datetime-fortran/CMakeFiles/datetime.dir/mod_constants.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/datetime.dir/mod_constants.f90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_constants.f90 -o CMakeFiles/datetime.dir/mod_constants.f90.s

datetime-fortran/CMakeFiles/datetime.dir/mod_datetime.f90.o: datetime-fortran/CMakeFiles/datetime.dir/flags.make
datetime-fortran/CMakeFiles/datetime.dir/mod_datetime.f90.o: ../datetime-fortran/mod_datetime.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building Fortran object datetime-fortran/CMakeFiles/datetime.dir/mod_datetime.f90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_datetime.f90 -o CMakeFiles/datetime.dir/mod_datetime.f90.o

datetime-fortran/CMakeFiles/datetime.dir/mod_datetime.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/datetime.dir/mod_datetime.f90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_datetime.f90 > CMakeFiles/datetime.dir/mod_datetime.f90.i

datetime-fortran/CMakeFiles/datetime.dir/mod_datetime.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/datetime.dir/mod_datetime.f90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_datetime.f90 -o CMakeFiles/datetime.dir/mod_datetime.f90.s

datetime-fortran/CMakeFiles/datetime.dir/mod_strftime.f90.o: datetime-fortran/CMakeFiles/datetime.dir/flags.make
datetime-fortran/CMakeFiles/datetime.dir/mod_strftime.f90.o: ../datetime-fortran/mod_strftime.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building Fortran object datetime-fortran/CMakeFiles/datetime.dir/mod_strftime.f90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_strftime.f90 -o CMakeFiles/datetime.dir/mod_strftime.f90.o

datetime-fortran/CMakeFiles/datetime.dir/mod_strftime.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/datetime.dir/mod_strftime.f90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_strftime.f90 > CMakeFiles/datetime.dir/mod_strftime.f90.i

datetime-fortran/CMakeFiles/datetime.dir/mod_strftime.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/datetime.dir/mod_strftime.f90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_strftime.f90 -o CMakeFiles/datetime.dir/mod_strftime.f90.s

datetime-fortran/CMakeFiles/datetime.dir/mod_timedelta.f90.o: datetime-fortran/CMakeFiles/datetime.dir/flags.make
datetime-fortran/CMakeFiles/datetime.dir/mod_timedelta.f90.o: ../datetime-fortran/mod_timedelta.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Building Fortran object datetime-fortran/CMakeFiles/datetime.dir/mod_timedelta.f90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_timedelta.f90 -o CMakeFiles/datetime.dir/mod_timedelta.f90.o

datetime-fortran/CMakeFiles/datetime.dir/mod_timedelta.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/datetime.dir/mod_timedelta.f90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_timedelta.f90 > CMakeFiles/datetime.dir/mod_timedelta.f90.i

datetime-fortran/CMakeFiles/datetime.dir/mod_timedelta.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/datetime.dir/mod_timedelta.f90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran/mod_timedelta.f90 -o CMakeFiles/datetime.dir/mod_timedelta.f90.s

# Object files for target datetime
datetime_OBJECTS = \
"CMakeFiles/datetime.dir/datetime.f90.o" \
"CMakeFiles/datetime.dir/mod_clock.f90.o" \
"CMakeFiles/datetime.dir/mod_constants.f90.o" \
"CMakeFiles/datetime.dir/mod_datetime.f90.o" \
"CMakeFiles/datetime.dir/mod_strftime.f90.o" \
"CMakeFiles/datetime.dir/mod_timedelta.f90.o"

# External object files for target datetime
datetime_EXTERNAL_OBJECTS =

lib/libdatetime.a: datetime-fortran/CMakeFiles/datetime.dir/datetime.f90.o
lib/libdatetime.a: datetime-fortran/CMakeFiles/datetime.dir/mod_clock.f90.o
lib/libdatetime.a: datetime-fortran/CMakeFiles/datetime.dir/mod_constants.f90.o
lib/libdatetime.a: datetime-fortran/CMakeFiles/datetime.dir/mod_datetime.f90.o
lib/libdatetime.a: datetime-fortran/CMakeFiles/datetime.dir/mod_strftime.f90.o
lib/libdatetime.a: datetime-fortran/CMakeFiles/datetime.dir/mod_timedelta.f90.o
lib/libdatetime.a: datetime-fortran/CMakeFiles/datetime.dir/build.make
lib/libdatetime.a: datetime-fortran/CMakeFiles/datetime.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Linking Fortran static library ../lib/libdatetime.a"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && $(CMAKE_COMMAND) -P CMakeFiles/datetime.dir/cmake_clean_target.cmake
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/datetime.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
datetime-fortran/CMakeFiles/datetime.dir/build: lib/libdatetime.a

.PHONY : datetime-fortran/CMakeFiles/datetime.dir/build

datetime-fortran/CMakeFiles/datetime.dir/clean:
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran && $(CMAKE_COMMAND) -P CMakeFiles/datetime.dir/cmake_clean.cmake
.PHONY : datetime-fortran/CMakeFiles/datetime.dir/clean

datetime-fortran/CMakeFiles/datetime.dir/depend:
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/datetime-fortran /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/Libs_build_linux/datetime-fortran/CMakeFiles/datetime.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : datetime-fortran/CMakeFiles/datetime.dir/depend

