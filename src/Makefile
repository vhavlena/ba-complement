#
# Makefile for Buchi automata complementation
# Author: Vojtech Havlena, 2019
#

RELEASE_FLAGS=-O2 -odir obj -hidir obj
TEST_FLAGS=-odir obj -hidir obj
DEBUG_FLAGS=-odir obj -hidir obj -prof -fprof-auto -rtsopts

all:
	ghc $(TEST_FLAGS) --make Main.hs

debug:
	ghc $(DEBUG_FLAGS) --make Main.hs

release:
	ghc $(RELEASE_FLAGS) --make Main.hs

clean:
	rm obj/*.o obj/*.hi
