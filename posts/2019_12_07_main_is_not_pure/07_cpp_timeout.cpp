#include <chrono>
#include <iostream>
#include <random>
#include <thread>

int myLongFunc() {
  std::random_device generator;
  std::uniform_int_distribution<int> distribution(1,10);
  int rand_int = distribution(generator);
  std::cout << "Random int result: " << rand_int << std::endl;
  if (rand_int == 10) {
    std::cout << "We're in luck, this will be fast." << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(2));
  } else {
    std::cout << "We're less lucky, this will be slow." << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(10));
  }
  return 100;
}

int main() {
  std::cout << "Hello world, let's talk to a DB!" << std::endl;
  std::cout << myLongFunc() << std::endl;
  return 0;
}
