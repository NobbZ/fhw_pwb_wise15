

all: main

llvm:
	clang++ -Wall -pedantic -std=c++11 -stdlib=libc++ -O4 -S -emit-llvm ./main.cpp

main: 
	clang++ -Wall -pedantic -std=c++11 -stdlib=libc++ -O4 ./main.cpp
