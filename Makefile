TARGETS= zfs_server.exe zfs_client.exe
EXAMPLES=shelter.exe simple.exe

.PHONY: all build clean %.exe

all: build link

build:
	dune build --profile release

link: $(TARGETS) $(EXAMPLES)

%.exe:
	if [ ! -d executables ]; then mkdir executables; fi
	if [ ! -f executables/$@ ]; then ln -s ../$$(find _build -name $@) executables/$@ ; fi

test:
	dune build @runtest

install:
	dune install

clean:
	dune clean;
	cd executables; rm -f $(TARGETS) $(EXAMPLES); cd ..
