# model-hs
Pretty simple markov chain. Constructing new sentences from some input file. Was written just because I have so much freetime...

### build
```bash
$ calab install random
$ git clone https://github.com/vulpes-solis/model-hs.git
$ cd ./model-hs
$ ghc main.hs -o <name>
```
When the compilation is finished you'll get the `<name>` binary executable file. For form something just launch it with base textfile path:
```bash 
./name /some/file.txt
```

### l'exemple
```
$ ./chain ./res/somedata.txt
Form everything some input skip newline new sentence. Это some nonsense for the
```
