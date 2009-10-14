#include <dlib/sockets.h>
#include <dlib/server.h>
#include <dlib/sockstreambuf.h>

#include <iostream>
#include <string>

//#include "server.h"

using namespace std;
using namespace dlib;

class scan_match_server : public server::kernel_1a_c
{
  int read_prelude(iostream& stream)
  {
    string buff;
    
    // getline clashes with another function
    std::getline(stream, buff);
    if (buff.compare("OH HAI") == 0) {
      cout << "OH HAI TO YOU!\n";
      return 0;
    } else {
      cout << "Got unexpected " << endl;
      cout << buff << endl;
      cout << "from client " << buff.size() << endl;
      return -1;
    }
  }


  void on_connect(connection& conn) 
  {
    sockstreambuf::kernel_2a buf(&conn);
    iostream stream(&buf);

    // This command causes our stream to flush its output
    // buffers whenever you ask it for more data.
    stream.tie(&stream);
    
    try {
      read_prelude(stream);
    } catch (exception& e) {
      cout << "on_connect: Caught exception " << e.what() << " while processing connection. Closing connection.\n";
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







  
