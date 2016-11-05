
PROG=compile-net
FILES=data.hs compil.hs parser.hs main.hs

all: $(PROG)

$(PROG) : $(FILES)
	ghc $^ -o $@

clean:
	@touch tmp.o
	rm -f *.o *.hi $(PROG)

.PHONY:all clean

