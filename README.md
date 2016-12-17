# Advent of Code for 2016 in F# #

## Compiling

The default task is to compile and run the tests.

- Windows: Open a command window and run `build.cmd`
- OS X / Linux: Open terminal and run `./build.sh`

To bulid executables run:

- Windows: `build.cmd build`
- OS X / Linux: `./build.sh build`

## Executables

The artifacts are created in the `build/` directory.
They each take one command line argument, which is in most cases the input file name.

- `day01.exe file_name`
- `day02.exe file_name`
- `day03.exe file_name`
- `day04.exe file_name`
- `day05.exe puzzle_input`
- `day06.exe file_name`
- `day07.exe file_name`
- `day08.exe file_name`
- `day09.exe file_name`
- `day10.exe file_name`
- `day12.exe file_name`
- `day14.exe puzzle_input`

### Note

I've been executing the artifacts on OS X using:
`mono build/day##.exe {arg}`
