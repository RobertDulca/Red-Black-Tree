# **Red-Black Tree Word Sorter**

This project processes a large text file, extracts all the unique words (case-insensitive and alphabetic only), and sorts them in alphabetical order using a **Red-Black Tree**. The sorted words are then written into an output file.

---

## **Table of Contents**
1. [Requirements](#requirements)
2. [Installation](#installation)
3. [Usage](#usage)
4. [How It Works](#how-it-works)
5. [Testing](#testing)
6. [Example Input and Output](#example-input-and-output)

---

## **Requirements**

To run this project, you need:

1. **GHC (Glasgow Haskell Compiler)**  
   - Install via [Haskell Platform](https://www.haskell.org/platform/) or directly with `ghcup`:
     ```bash
     curl https://get-ghcup.haskell.org -sSf | sh
     ```

2. **Doctest** (for running inline tests):
   - Install with Cabal:
     ```bash
     cabal update
     cabal install doctest
     ```

3. **war_and_peace.txt**  
   - Place a plain text file named `war_and_peace.txt` in the same directory as the program.

---

## **Installation**

1. Clone the repository:
   ```bash
   git clone <your-repository-url>
   cd <project-folder>
