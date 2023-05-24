MKFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
CUR_DIR := $(dir $(MKFILE))

CC=gcc

ifeq ($(OS), Windows_NT)
${'\t'}SYS := $(shell $(CC) -dumpmachine)
${'\t'}ifneq (, $(findstring mingw, $(SYS)))
${'\t'}${'\t'}RM=rm -f
${'\t'}else ifneq (, $(findstring cygwin, $(SYS)))
${'\t'}${'\t'}RM=rm -f
${'\t'}else
${'\t'}${'\t'}RM=del /F /Q
${'\t'}endif

${'\t'}PATHSEP2=${'\\\\'}
${'\t'}JNI_INCLUDE=$(JAVA_HOME)\\include\\win32
${'\t'}C_FILE := $(CUR_DIR)jni\\jni_impl.c
${'\t'}O_FILE := $(CUR_DIR)jni\\jni_impl.o
${'\t'}LIB_FILE := $(CUR_DIR)jni\\${ctx.lang_name.lower}lang_jni.dll
else
${'\t'}RM=rm -f
${'\t'}PATHSEP2=/
${'\t'}JNI_INCLUDE=$(JAVA_HOME)/include/linux
${'\t'}C_FILE := $(CUR_DIR)jni/jni_impl.c
${'\t'}O_FILE := $(CUR_DIR)jni/jni_impl.o
${'\t'}LIB_FILE := $(CUR_DIR)jni/lib${ctx.lang_name.lower}lang_jni.so
endif

PATHSEP=$(strip $(PATHSEP2))

C_OPT=-fPIC -O3 -Wall -Werror -I$(JAVA_HOME)$(PATHSEP)include \
-I$(JNI_INCLUDE)

LD_OPT=-shared -fPIC \
-L$(CUR_DIR)..$(PATHSEP)lib$(PATHSEP)relocatable$(PATHSEP)prod \
-L$(CUR_DIR)..$(PATHSEP)lib$(PATHSEP)relocatable$(PATHSEP)dev

all: $(LIB_FILE)

$(LIB_FILE): $(O_FILE)
${'\t'}$(CC) $(LD_OPT) -o $(LIB_FILE) $(O_FILE) \
-l${ctx.lang_name.lower}lang

$(O_FILE): $(C_FILE)
${'\t'}$(CC) $(C_OPT) -c -o $(O_FILE) $(C_FILE)

clean:
${'\t'}$(RM) $(O_FILE)
${'\t'}$(RM) $(LIB_FILE)

ifndef VERBOSE
.SILENT:
endif
