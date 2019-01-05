all: new old

new: Main.hs
	cabal new-build

old: new
	ln -s new old

install:
	install -s new old /usr/local/bin

clean:
	-rm -f *.hi *.o new old
