all:
	cd src && make
	cd examples && make

clean:
	cd src && make clean
	cd examples && make clean
