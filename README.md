# OCaml Rubik's Cube

OCaml Rubik's Cube is a full modelisation of a Rubik's cube, with graphical representation and interactive animation

## User manual

Once the graphical Window is opened, you have a few actions that you can make to the cube :

* `Esc` : Exit
* `z,q,s,d` : Move the point of view around the Cube (Yes, this is AZERTY equivalent of w,a,s,d)
* `a` : Reset the point of view
* `m` : Shuffle the cube
* `l` : Reinitialize the cube
* `1,2,3,4,5,6` : Select a face by its central color (1: white, 2: blue, 3: orange, 4: green, 5: red, 6: yellow). Once you have selected a face, you can turn it by typing :
  * `1` : Trun clockwise
  * `2` : Half-turn
  * `3` : Turn anti-clockwise
  * `4` : Cancel selection
* `Backspace` : Undo last movement
* `Space` : Step-by-step resolution
* `Return` : Full resolution

Please note that the resolution algorith never was finished, and can even end on an infinite loop ...


## Building

If you have a x64 computer running Linux, you just need to execute the file `ocaml_rubiks_cube_compiled`.

In any other situation, you will either need to build it yourself, or to run it interactively. In both cases, you will need the OCaml graphics library.

* To complite the program natively :
`ocamlopt graphics.cmxa ocaml_rubiks_cube.ml -o ocaml_rubiks_cube_compiled`

* To compile it in bytecode :
`ocamlc graphics.cma ocaml_rubiks_cube.ml -o rubik_bytecode`

You can then run it with `ocamlmktop`

* You can use a program such as WinCaml, but you will need to uncomment the first line of the file


## Remarks

As you might have imagined, this code was initially not written to be publicly publised and shared, it was written for a school project and should be considered as such.

It was also written before our discovery of git, that's why most of the history and some related instructions are lost. It's a kind of prehistory !
