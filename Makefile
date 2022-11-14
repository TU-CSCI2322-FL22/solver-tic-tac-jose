# Commands:

name := jose
testName := esoj

.PHONY: build init test clean doc deploy stage

build: 
	ghc --make -O -o $(name) Main.hs

test:
	ghc --make -O -o $(testName) -main-is Testing.runTests Testing.hs
	./esoj.exe

prof:
	ghc --make -prof -o $(name) Main.hs

all: build

# Cleaning commands:
clean:
	rm -f $(name)
	rm -f *.hi
	rm -f *.o
