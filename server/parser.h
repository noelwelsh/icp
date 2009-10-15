#ifndef __PARSER_H_
#define __PARSER_H_

#include <iostream>

#include "../c/point.h"

using namespace std;

class unexpected_input : public exception 
{
  public:
  unexpected_input(const char*);
  unexpected_input(string);
  ~unexpected_input() throw();
  const char* what() const throw();
};

bool   read_prelude(iostream&);
bool   read_postlude(iostream&);
bool   read_continuation(iostream&);
int    read_n(iostream&);
void   read_points(iostream&, int, polar_t[]);
double read_double(iostream&);

#endif
