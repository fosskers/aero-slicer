PLATFORM ?= PLATFORM_DESKTOP_GLFW
CL_MODE  ?= DEV
WINE     ?= $(HOME)/".wine/drive_c/Program Files/Steel Bank Common Lisp"

aero-slicer: dev
	CL_MODE=$(CL_MODE) sbcl --load build.lisp

ecl: dev lib/liblisp-raylib.so.550
	CL_MODE=$(CL_MODE) ecl --load build.lisp

dev: lib/ lib/liblisp-raylib.so lib/liblisp-raylib-shim.so raylib.h shim.h

lib/:
	mkdir lib/

lib/liblisp-raylib.so.550:
	ln -s liblisp-raylib.so lib/liblisp-raylib.so.550

lib/liblisp-raylib.so:
	cd vendored/raylib/ && $(MAKE) PLATFORM=$(PLATFORM)
	mv vendored/raylib/lib/liblisp-raylib.so lib/

lib/liblisp-raylib-shim.so: lib/liblisp-raylib.so
	mv vendored/raylib/lib/liblisp-raylib-shim.so lib/

raylib.h:
	ln -s vendored/raylib/c/raylib.h raylib.h

shim.h:
	ln -s vendored/raylib/c/shim.h shim.h

clean:
	-rm raylib.h shim.h aero-slicer aero-slicer.exe
	rm -rf lib/
	cd vendored/raylib/ && $(MAKE) clean

# --- Windows --- #

windows: lib/ lib/lisp-raylib.dll lib/lisp-raylib-shim.dll raylib.h shim.h
	CL_MODE=$(CL_MODE) wine $(WINE)/sbcl.exe --load build.lisp

lib/lisp-raylib.dll:
	cd vendored/raylib/ && $(MAKE) PLATFORM=$(PLATFORM) windows
	mv vendored/raylib/lib/lisp-raylib.dll vendored/raylib/lib/liblisp-raylibdll.a lib/

lib/lisp-raylib-shim.dll: lib/lisp-raylib.dll
	mv vendored/raylib/lib/lisp-raylib-shim.dll lib/
