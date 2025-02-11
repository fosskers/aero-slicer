PLATFORM ?= PLATFORM_DESKTOP_GLFW

aero-fighter: dev
	sbcl --load build.lisp

dev: lib/ lib/libraylib.so.550 lib/libshim.so raylib.h shim.h

lib/:
	mkdir lib/

lib/libraylib.so.550: lib/libraylib.so
	ln -s libraylib.so lib/libraylib.so.550

lib/libraylib.so:
	cd vendored/raylib/src/ && $(MAKE) PLATFORM=$(PLATFORM)
	mv vendored/raylib/src/libraylib.so.5.5.0 lib/libraylib.so

lib/libshim.so: raylib/shim.c raylib/raylib.h
	cd raylib && gcc -O3 -fPIC -shared -o libshim.so shim.c
	mv raylib/libshim.so lib/

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
