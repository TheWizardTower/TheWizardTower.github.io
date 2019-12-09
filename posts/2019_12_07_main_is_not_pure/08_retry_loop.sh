#!/bin/bash

pwd

g++ 07_cpp_timeout.cpp -o example

for ii in $(seq 1 15); do
  timeout 5s ./example
  if [[ $? -eq 0 ]];
  then
    break;
  fi
done
