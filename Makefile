LLVM_DIR=/usr/local/llvm10ra/

liveness-analysis:
	mkdir build -p && cd build && cmake -DLLVM_DIR=$(LLVM_DIR) -DCMAKE_BUILD_TYPE=Debug .. && make -j

clean:
	rm -rf build && mkdir build