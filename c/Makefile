OBJS   =  icp_base.o angle.o point.o geometry.o pose.o util.o icp.o imrp.o
CFLAGS =  -std=gnu99 -funroll-loops -O3 -fpic -msse2 -march=pentium4 -mfpmath=sse
SYS    =  $(shell uname)

libicp.so: $(OBJS)
ifeq ($(SYS),Darwin)
	gcc $(CFLAGS) -dynamiclib -o libicp.dylib $(OBJS)
else
	gcc $(CFLAGS) -shared -o libicp.so $(OBJS)
endif

icp_so: libicp.so


default: icp_so

