SRC   =  parser.cpp server.cpp ../dlib-17.22/dlib/all/source.cpp
SRC  +=  ../c/icp_base.c ../c/angle.c ../c/point.c ../c/geometry.c ../c/pose.c ../c/util.c ../c/icp.c ../c/imrp.c ../c/idc.c

# For normal use
#CPPFLAGS =  -I"../dlib-17.22/" -DDEBUG -DDLIB_NO_GUI_SUPPORT -O3 -Wall -fPIC -msse2 -march=pentium4 -mfpmath=sse
# For debugging
CPPFLAGS =  -I"../dlib-17.22/" -DDEBUG -DDLIB_NO_GUI_SUPPORT -g -O -Wall -fPIC
CFLAGS =  -std=gnu99 -g -O -Wall -fPIC -msse2 -march=pentium4 -mfpmath=sse


SYS = $(shell uname)

TMP = $(SRC:.cpp=.o)
OBJ = $(TMP:.c=.o)

libidcserver.so: $(OBJ)
ifeq ($(SYS),Darwin)
	g++ $(CPPFLAGS) -dynamiclib -lm -o libidcserver.dylib $(OBJS)
else
	g++ -shared -Wl,-soname,libidcserver.so.1 -lnsl -lpthread -lm -o libidcserver.so $(OBJ)
endif

clean:
	rm $(OBJ)

default: libidcserver.so

