# **Red-Black Tree Word Sorter**

This project processes a large text file, extracts all the unique words (case-insensitive and alphabetic only), and sorts them in alphabetical order using a **Red-Black Tree**. The sorted words are then written into an output file.

---

## **Table of Contents**

- [**Red-Black Tree Word Sorter**](#red-black-tree-word-sorter)
  - [**Table of Contents**](#table-of-contents)
  - [**Requirements**](#requirements)
  - [**Installation**](#installation)
  - [**How to Compile the Project**](#how-to-compile-the-project)
  - [**How to Run Tests**](#how-to-run-tests)

---

## **Requirements**

To run this project, you need:

1. **GHC (Glasgow Haskell Compiler)**  
   - Install via [Haskell Platform](https://www.haskell.org/ghcup/) 

2. **Doctest** (for running inline tests):
   - Install with Cabal:
     ```bash
     cabal update
     cabal install doctest
     ```

3. **war_and_peace.txt**  
   - Place a plain text file named `war_and_peace.txt` in the same directory as the program.

## **Installation**

1. Clone the repository:
   ```bash
   git clone <your-repository-url>
   cd <project-folder>

## **How to Compile the Project**

1. Open a terminal or command prompt.
2. Navigate to the project folder where `Main.hs` is located.
3. Compile the program using GHC with optimization:
   ```bash
   ghc -O2 -o Main Main.hs
This will generate an executable named `Main` (or `Main.exe` on Windows).

## **How to Run Tests**
This project includes inline examples that can be tested using Doctest.

1. Ensure doctest is installed (see the requirements above).
2. Run doctest on the Haskell source file:
    ```bash
    doctest Main.hs
If all tests pass, there will be no output. Errors will display any mismatches between expected and actual results.