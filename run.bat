@echo off
REM run.bat - Script to compile, run, and test the Haskell program on Windows.

REM Step 1: Clean old compiled files
echo Cleaning up old files...
del /Q Main.exe Main.hi Main.o output.txt >nul 2>&1

REM Step 2: Compile the Haskell source file
echo Compiling Main.hs...
ghc -O2 -o Main Main.hs
IF %ERRORLEVEL% NEQ 0 (
    echo Compilation failed!
    exit /b %ERRORLEVEL%
)

REM Step 3: Run the program
echo Running the program...
Main.exe
IF %ERRORLEVEL% NEQ 0 (
    echo Program execution failed!
    exit /b %ERRORLEVEL%
)

REM Step 4: Check for output file
IF EXIST output.txt (
    echo Program executed successfully. Check "output.txt" for results.
) ELSE (
    echo Error: "output.txt" was not created!
    exit /b 1
)

REM Step 5: Run tests with Doctest
echo Running doctests...
doctest Main.hs
IF %ERRORLEVEL% NEQ 0 (
    echo Doctests failed!
    exit /b %ERRORLEVEL%
)

echo All tasks completed successfully!
pause
