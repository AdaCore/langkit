<%
    lib_prefix = ctx.ada_api_settings.lib_name.upper() + "_"
%>

MKFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
CUR_DIR := $(dir $(MKFILE))

CC=gcc

# Allow Makefile users to set the build mode through the
# ${lib_prefix}BUILD_MODE environment variable.
${lib_prefix}BUILD_MODE?=debug
BUILD_MODE=$(${lib_prefix}BUILD_MODE)

ifeq ($(OS), Windows_NT)
${'\t'}SYS := $(shell $(CC) -dumpmachine)
${'\t'}ifneq (, $(findstring mingw, $(SYS)))
${'\t'}${'\t'}RM=rm -f
${'\t'}PATHSEP2=/
${'\t'}else ifneq (, $(findstring cygwin, $(SYS)))
${'\t'}${'\t'}RM=rm -f
${'\t'}PATHSEP2=/
${'\t'}else
${'\t'}${'\t'}RM=del /F /Q
${'\t'}PATHSEP2=${'\\\\'}
${'\t'}endif

${'\t'}JNI_INCLUDE=$(JAVA_HOME)$(PATHSEP2)include$(PATHSEP2)win32
${'\t'}LIB_FILE_NAME := ${cfg.library.language_name.lower}lang_jni.dll
else
${'\t'}RM=rm -f
${'\t'}PATHSEP2=/
${'\t'}JNI_INCLUDE=$(JAVA_HOME)/include/linux
${'\t'}LIB_FILE_NAME := lib${cfg.library.language_name.lower}lang_jni.so
endif

PATHSEP=$(strip $(PATHSEP2))
C_FILE := jni$(PATHSEP)jni_impl.c
O_FILE := jni$(PATHSEP)jni_impl.o
LIB_FILE := jni$(PATHSEP)$(LIB_FILE_NAME)

COMMON_C_OPTS=\
  -fPIC \
  -g \
  -Wall \
  -I$(JAVA_HOME)$(PATHSEP)include \
  -I$(JNI_INCLUDE)
ifeq ($(BUILD_MODE), debug)
BUILD_MODE_OPTS=-O0 -Werror
else ifeq ($(BUILD_MODE), prod)
BUILD_MODE_OPTS=-Ofast
else ifeq ($(BUILD_MODE), prof)
BUILD_MODE_OPTS=-O3
endif

C_OPT=$(COMMON_C_OPTS) $(BUILD_MODE_OPTS)

LD_OPT=-shared -fPIC \
-L$(CUR_DIR)..$(PATHSEP)lib$(PATHSEP)relocatable$(PATHSEP)prod \
-L$(CUR_DIR)..$(PATHSEP)lib$(PATHSEP)relocatable$(PATHSEP)dev

all: $(LIB_FILE)

$(LIB_FILE): $(O_FILE)
${'\t'}$(CC) $(LD_OPT) -o $(LIB_FILE) $(O_FILE) \
-l${cfg.library.language_name.lower}lang

$(O_FILE): $(C_FILE)
${'\t'}$(CC) $(C_OPT) -c -o $(O_FILE) $(C_FILE)

clean:
${'\t'}$(RM) $(O_FILE)
${'\t'}$(RM) $(LIB_FILE)

ifndef VERBOSE
.SILENT:
endif
