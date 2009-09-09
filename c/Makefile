OBJS   =  icp_base.o angle.o point.o
CFLAGS =  -std=gnu99 -funroll-loops -O3 -fpic

libicp.so: $(OBJS)
	gcc $(CFLAGS) -shared -o libicp.so $(OBJS)

icp_so: libicp.so


default: icp_so

