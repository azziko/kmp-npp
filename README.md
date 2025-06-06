# KMP String Search in Haskell

## Compile:

Compile the program before running it:

```bash
ghc kmp.hs -o kmp
```

## Run:

To run the program provide needle as the first argument and the path to a file you would like to search in as the second argument:

### Example 1

```bash
$ ./kmp text test.txt
1: Some text here
        ^^^^
4: There is some text here
                 ^^^^
5: And also I replicate text twice in the next text line
                        ^^^^                   ^^^^
6: texttext
   ^^^^^^^^
8: TextTexttExtteeextxxxtTexT
   ^^^^^^^^^^^^          ^^^^
```

### Example 2
```bash 
$ ./kmp baba test.txt
8: bababababababooobabab
   ^^^^^^^^^^^^    ^^^^ 
```