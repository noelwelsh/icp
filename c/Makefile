OBJS   =  icp_base.o angle.o point.o geometry.o pose.o util.o icp.o imrp.o idc.o
# For debugging
CFLAGS =  -std=gnu99 -O -Wall -fPIC -msse2 -march=pentium4 -mfpmath=sse
#CFLAGS  =  -std=gnu99 -O -Wall -fPIC -lm
# For real use
#CFLAGS =  -std=gnu99 -funroll-loops -O3 -fPIC -msse2 -march=pentium4 -mfpmath=sse 
SYS    =  $(shell uname)

libicp.so: $(OBJS)
ifeq ($(SYS),Darwin)
	gcc $(CFLAGS) -dynamiclib -lm -o libicp.dylib $(OBJS)
else
	gcc $(CFLAGS) -shared -lm -o libicp.so $(OBJS)
endif

icp_so: libicp.so

grind: grind.o $(OBJS)
	gcc $(CFLAGS) -lm -o grind grind.o $(OBJS)

scan_match: front_end.o austin-dump.o $(OBJS)
	gcc $(CFLAGS) -lm -o scan_match front_end.o austin-dump.o $(OBJS)

clean:
	rm $(OBJS)

default: icp_so

