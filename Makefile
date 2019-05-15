TARGETS= zfs.exe #zfsServer.exe zfsClient.exe

.PHONY: all build clean %.exe

all: build link

build:
	jbuilder build

link: $(TARGETS)

%.exe:
	if [ ! -f $* ]; then ln -s _build/default/$@ $* ; fi

test:
	jbuilder build @runtest

install:
	jbuilder install

clean:
	rm -rf _build .utop *.install $(basename $(TARGETS))
