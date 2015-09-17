UNAME := $(shell uname)

all :
	stack build
ifeq ($(UNAME), Linux)
	~/git/ed/.stack-work/install/x86_64-linux/lts-3.5/7.10.2/bin/ed-exe
else
	~/git/ed/.stack-work/install/i386-windows/lts-3.5/7.10.2/bin/ed-exe.exe
endif
