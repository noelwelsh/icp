#include <dlib/sockets.h>
#include <dlib/server.h>
#include <dlib/sockstreambuf.h>

#include <iostream>
#include <string>

extern "C" {
#include "../c/idc.h"
}

#include "server.h"
#include "parser.h"

using namespace std;
using namespace dlib;


void write_response(iostream& stream, double xt, double yt, double a) 
{
  stream << "OH HAI\n";

  stream.write((char*)&xt, sizeof(double));
  stream.write((char*)&yt, sizeof(double));
  stream.write((char*)&a, sizeof(double));
    
  stream << "KTHX\n";
}


class scan_match_server : public server::kernel_1a_c
{
  void on_connect(connection& conn) 
  {
    cout << "New connection\n";
    
    sockstreambuf::kernel_2a buf(&conn);
    iostream stream(&buf);

    // This command causes our stream to flush its output
    // buffers whenever you ask it for more data.
    stream.tie(&stream);
    
    try {
      do {
        read_prelude(stream);
        int n = read_n(stream);
        polar_t ref_pts[n], new_pts[n];
        
        read_points(stream, n, ref_pts);
        read_points(stream, n, new_pts);
        
        double xt = read_double(stream);
        double yt = read_double(stream);
        double a  = read_double(stream);
        double r  = read_double(stream);
        
        read_postlude(stream);
        
        double xt_out, yt_out, a_out;
        
        idc(ref_pts, new_pts, n,
            xt, yt, a, r,
            100, 0.001,
            &xt_out, &yt_out, &a_out);
        
        write_response(stream, xt_out, yt_out, a_out);
      } while(read_continuation(stream));
    } catch (exception& e) {
      cout << "on_connect: Caught exception \n" << e.what() << "\n while processing connection. Closing connection.\n";
    } catch (...) {
      cout << "on_connect: Caught unknown exception while processing connection. Closing connection.\n";
    }
  }
};


scan_match_server serve;

void start_server()
{
  serve.set_listening_port(3478);

  try {
    serve.start();
  } catch (exception& e) {
    cout << "start_server: Caught exception in start_server" << e.what() << "\n";
  } catch (...) {
    cout << "start_server: Caught unknown exception in start_server\n";
  }
}

thread_function t(start_server);

extern "C" void stop() 
{
  serve.clear();
  return;
}







  
