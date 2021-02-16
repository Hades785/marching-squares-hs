# Marching Squares - Haskell

Simple implementation of the
[marching squares algorithm](https://en.wikipedia.org/wiki/Marching_squares)
in Haskell, with a GUI to manipulate the input data and visualise the output.

## Requirements

- [Stack](https://docs.haskellstack.org/en/stable/README/)

## Building and running

To simply build: `stack build`  
To build and run: `stack run`

## How to use

Once the program is running, click on any of the grid's nodes (circles) to
toggle their state. The default state is off, so clicking once on any of them
will toggle them on.

By toggling multiple nodes adjacent to each other, you can visualise how the
marching squares algorithm builds a contour around them.

## Code structure

- `Main.hs`: Creates the window.
- `Game.hs`: Execution loop and data structures.
- `Rendering.hs`: Rendering tasks. This is where the isolines are calculated,
  and thus the core of the marching squares algorithm in itself.