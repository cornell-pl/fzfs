# Install

## Requirements
- Dune >= 1.6
- OCaml version 4.05.0 (exactly for now)

## Instructions
To run OCaml ZFS and TxForest, make sure you have all dependencies ('core', 'async', 'ppx_deriving', and 'dune') by running

> opam install dune core async ppx_deriving

Then, run:

> make

> ./executables/zfs_server.exe .

Now you are ready to run TxForest or a simple client. To explore the built-in
virtual filesystem, in one or more other terminals run:

> ./executables/zfs_client.exe

And type help to get a list of commands.

### TxForest

The concept of TxForest is similar to
[Forest](https://dl.acm.org/citation.cfm?id=2034814) and 
[Incremental Forest](https://dl.acm.org/citation.cfm?id=2984034). Refer to those papers for
details and a high level overview of the project.

Differences between TxForest and Incremental Forest are listed below, as is a
link to the code of Incremental Forest.

#### Examples

##### Simple

This is a very simple example with 3 available transactions. Run it as follows:

> ./executables/simple.exe -id [0-3] -name "[a-d]" -trans [0-2] 

Transaction description:
- Transaction 0 takes no further arguments and prints a description of the filestore
- Transaction 1 takes an 'id' and looks up the file with that id
- Transaction 2 takes a name (a,b,c, or d) and looks up the file with that name

##### Shelter

This is a much larger example with 14 transactions (1-14). Run:

> ./executables/shelter.exe -help

To see the interface.

#### Differences from [Incremental Forest](https://github.com/padsproj/oforest)

See **forest/examples/shelter.ml** for an example of using the TxForest API
(which is specified in **forest/forest.mli**).

- There are no delays or cursors as these are embedded in the Zipper. That is,
  as users walk through the zipper, data is automatically loaded as necessary.
- Several specifications are not yet available or unnecessary, including:

  - s option (N/A)
  - s where e (N/A)
  - link (N/A)
  - \<s\>  (No longer necessary)

- The interface and representation types are entirely different. See
  **forest/forest.mli** for both. The 'fetch_result' type is the representation
  type of each existing specification. In essence, this comes from the fact that
  only local data is made available to the user.
- A constructed specification should be used in one of two ways:
  
  - ```ocaml
    let open Core.Result.Let_syntax in
    let%map zipper = mk_zipper specification path in
    let t = my_transaction zipper in
    finish t
    ```
    
    With the 'mk_zipper' operation, which takes a specification and a filesystem
    path and instantiates the specification at the path. This constructs a Forest
    zipper which can then be traversed and manipulated with the TxForest API.
    The 'finish' operation will try to commit the transaction.
  
  - ```ocaml
    run_txn ~f:my_transaction specification path ()
    ```

    With the 'run_txn' function, which uses a specification, path, and a
    transaction (function on Forest zippers) to start a new transaction, run the
    transaction function given, then attempt to commit. It will then redo these
    steps until the commit succeeds.

- ZFS does not currently connect to a real filesystem and instead uses an
  internal virtual FS. To add additional folders, extend the 'FS.init' function in
  **src/lib/ZFSUtils.ml**. Start by adding a child to 'root' with your main
  folder as an empty directory, then add whatever else you want:
  
  ```ocaml
    add_child root main_folder_name empty_dir;
    add_child (Filename.concat root main_folder_name) some_file (FFile contents);
    add_child (Filename.concat root main_folder_name) some_folder empty_dir;
  ```
  etc