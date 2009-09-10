OBJS   =  icp_base.o angle.o point.o geometry.o pose.o util.o icp.o imrp.o
# For debugging
CFLAGS =  -std=gnu99 -O -Wall -fPIC -msse2 -march=pentium4 -mfpmath=sse
# For real use
#CFLAGS =  -std=gnu99 -funroll-loops -O3 -fPIC -msse2 -march=pentium4 -mfpmath=sse
SYS    =  $(shell uname)

libicp.so: $(OBJS)
ifeq ($(SYS),Darwin)
	gcc $(CFLAGS) -dynamiclib -o libicp.dylib $(OBJS)
else
	gcc $(CFLAGS) -shared -o libicp.so $(OBJS)
endif

icp_so: libicp.so

grind: grind.o $(OBJS)
	gcc $(CFLAGS) -o grind grind.o $(OBJS)

clean:
	rm $(OBJS)

default: icp_so

