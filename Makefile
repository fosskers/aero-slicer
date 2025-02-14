PLATFORM ?= PLATFORM_DESKTOP_GLFW
CL_MODE  ?= DEV

aero-fighter: dev
	CL_MODE=$(CL_MODE) sbcl --load build.lisp

ecl: dev lib/libaero-fighter-raylib.so.550
	CL_MODE=$(CL_MODE) ecl --load build.lisp

dev: lib/ lib/libaero-fighter-raylib.so lib/libaero-fighter-shim.so raylib.h shim.h

lib/:
	mkdir lib/

lib/libaero-fighter-raylib.so.550:
	ln -s libaero-fighter-raylib.so lib/libaero-fighter-raylib.so.550

lib/libaero-fighter-raylib.so:
	cd vendored/raylib/src/ && $(MAKE) PLATFORM=$(PLATFORM)
	mv vendored/raylib/src/libaero-fighter-raylib.so lib/libaero-fighter-raylib.so

lib/libaero-fighter-shim.so: raylib/shim.c raylib/raylib.h
	cd raylib && gcc -O3 -fPIC -shared -o libaero-fighter-shim.so shim.c
	mv raylib/libaero-fighter-shim.so lib/

raylib/raylib.h:
	ln -s ../vendored/raylib/src/raylib.h raylib/raylib.h

raylib.h:
	ln -s vendored/raylib/src/raylib.h raylib.h

shim.h:
	ln -s raylib/shim.h shim.h

clean:
	-rm raylib.h shim.h aero-fighter raylib/raylib.h
	rm -rf lib/
	cd vendored/raylib/src/ && $(MAKE) clean
