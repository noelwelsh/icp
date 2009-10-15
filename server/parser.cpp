#include <iostream>
#include <string>

#include <dlib/string.h>
#include <dlib/byte_orderer.h>

extern "C" {
#include "../c/point.h"
}

using namespace std;
using namespace dlib;

byte_orderer_kernel_1 orderer;

class unexpected_input : public exception 
{
  private:
  string reason;
  
  public:
  unexpected_input(const char* msg = "")
    : reason(msg)
  {
  }

  unexpected_input(string msg)
    : reason(msg)
  {
  }
  

  ~unexpected_input() throw()
  {
  }
  

  const char* what() const throw() 
  {
    return reason.c_str();
  }
};

  
bool read_prelude(iostream& stream) 
{
  cout << "read_prelude\n";
  
  string buff;
  
  getline(stream, buff);
  if (buff.compare("OH HAI") == 0) {
      return true;
  } else {
    throw unexpected_input(buff);
  }
}

bool read_postlude(iostream& stream) 
{
  cout << "read_postlude\n";

  string buff;
  
  getline(stream, buff);
  if (buff.compare("KTHX") == 0) {
      cout << "KTHX\n";
      return true;
  } else {
    throw unexpected_input(buff);
  }
}

bool read_continuation(iostream& stream) 
{
  cout << "read_continuation\n";

  string buff;
  
  getline(stream, buff);
  if (buff.compare("MOAR") == 0) {
    cout << "MOAR!\n";
    return true;
  } else if (buff.compare("KTHXBAI") == 0) {
    cout << "BAI!\n";
    return false;
  } else {
    throw unexpected_input(buff);
  }
}

int read_n(iostream& stream) 
{
  cout << "read_n\n";
  
  string buff;
  
  getline(stream, buff);
  if (buff.compare("NIZ") == 0) {
      cout << "NIZ\n";
      string n;
      getline(stream, n);
      return string_cast<int>(n);
  } else {
    throw unexpected_input(buff);
  }
}

double read_double(iostream& stream) 
{
  double buff;

  stream.read((char*)&buff, sizeof(double));

  if(stream.fail()) {
    throw unexpected_input("Failed reading double.");
  } else {
    orderer.little_to_host<double>(buff);
    return buff;
  }
}


void read_points(iostream& stream, int n, polar_t pts[]) 
{
  for (int i = 0; i < n; i++) {
    pts[i].r = read_double(stream);
    pts[i].a = read_double(stream);
  }

  return;
}

