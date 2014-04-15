all:
	cd build && make
	cd examples && make

clean:
	cd build && make clean
	cd examples && make clean
