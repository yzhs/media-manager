all: new old

new: new.hs
	ghc --make new.hs

old: new
	cp new old

install:
	install -s new old /usr/local/bin

clean:
	-rm -f *.hi *.o new old
