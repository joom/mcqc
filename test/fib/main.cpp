#include <iostream>
#include "benchmark.h"
#include "Fib.cpp"

using namespace nat;

int main() {
    tic();
	// Nats, autogenerated at Fib.cpp
	std::cout << fib(36) << std::endl;
    // Benchmark, stop
	toc();
    return 0;
}

