SRC   =  server.cpp ../dlib-17.22/dlib/all/source.cpp
# For normal use
#CPPFLAGS =  -I"../dlib-17.22/" -DDEBUG -DDLIB_NO_GUI_SUPPORT -O3 -Wall -fPIC -msse2 -march=pentium4 -mfpmath=sse
# For debugging
CPPFLAGS =  -I"../dlib-17.22/" -DDEBUG -DDLIB_NO_GUI_SUPPORT -g -O -Wall -fPIC


SYS = $(shell uname)

TMP = $(SRC:.cpp=.o)
OBJ = $(TMP:.c=.o)

libserver.so: $(OBJ)
ifeq ($(SYS),Darwin)
	g++ $(CPPFLAGS) -dynamiclib -lm -o libserver.dylib $(OBJS)
else
	g++ -shared -Wl,-soname,libserver.so.1 -lnsl -lpthread -lm -o libserver.so $(OBJ)
endif

clean:
	rm $(OBJ)

default: libserver.so

