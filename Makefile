# Commands:

name := foo

.PHONY: build init test clean doc deploy stage

build: 
	ghc --make -O -o $(name) Main.hs

prof:
	ghc --make -prof -o $(name) Main.hs

all: build

# Cleaning commands:
clean:
	rm -f $(name)
	rm -f *.hi
	rm -f *.o
