# Instruction
To run ocaml ZFS, make sure you have all dependencies ('core' and 'async') by running

> opam install core async

Then, run:

> make

> ./zfsServer
OR
> python3 spawn_all.py 

Where spawn_all starts a distributed ZFS server (with two shards and a master) and the normal zfsServer starts a non-distributed ZFS server.

Then, in one or more other terminals run:

> ./zfsClient

After starting a server, you can also run:

> make test

In order to two tests which both calculate the frequency of words in the file system.

Expected output:
```
1: 1
2: 1
3: 1
File: 3
```