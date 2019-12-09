#include <iostream>
#include <exception>

struct MyException : public std::exception {
  const char * what () const throw () {
    return "Example C++ Exception!";
  }
};

int main() {
  std::cout << "The end is near!" << std::endl;
  MyException exception = MyException();
  std::cout << "But not here yet." << std::endl;
  throw exception;
  std::cout << "Lamentations, our process is cut short." << std::endl;

  return 0;
}
