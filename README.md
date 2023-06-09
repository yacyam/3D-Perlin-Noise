# 3D Perlin Noise Generator
<i>Final Project for CS3110</i>

Our project is an interactive 3D Perlin Noise generator. It displays
  on the screen a three-dimensional terrain in which the user can view
  from many different angles. As the user, you can color the noise to
  look like rivers and mountains, or even like a volcano, and can create
  your own noise by drawing on the screen or inputting a seed value. This
  project also offers visually stunning ways of mutating the noise in 
  real-time, simulating changes in environments.


<img width="993" alt="Screenshot 2023-05-20 at 5 39 25 PM" src="https://github.com/yacyam/3D-Perlin-Noise/assets/119266083/cddad459-bf7a-4662-964d-908e493f1c49">

<h3>Team:</h3>
<ul>
  <li>Yacqub</li>
  <li>Mitch</li>
  <li>Harriet</li>
</ul>

<h3>Project Install Guide:</h3>

First: Install OPAM and OCaml
  - Carefully Follow the guide at 
    https://cs3110.github.io/textbook/chapters/preface/install.html
    to install OPAM and OCaml. If Homebrew or MacPorts is not installed,
    follow the guide at either https://brew.sh/ or https://www.macports.org/install.php
    to continue with the OPAM installation.
  - Make sure at the very end you're able to run utop within your terminal,
    and if not, make sure to follow the debugging tools in the textbook guide.

Second: Update opam
  - Make sure to run 'opam update' and 'opam upgrade' to make sure you're on the
    latest version of opam before installing.

Third: Run 'opam install raylib'
  - Accept all prompts given while installing raylib to opam, and let
    the installation fully finish.
  - Make sure you installed Raylib.1.0.0 or a later version. If not you may still
    need to run 'opam update' and 'opam upgrade'.
  - After this, you can run 'make build' and 'make play' to display the window.


<h3>Resources Used for Perlin Algorithm (<i>credit</i>):</h3>

https://rtouti.github.io/graphics/perlin-noise-algorithm (By: Touti Raouf, touti.raouf@gmail.com)
  - Used to learn general overview of perlin noise implementation
  - Used to learn about linear interpolation and fade function

https://adrianb.io/2014/08/09/perlinnoise.html (By: Adrian Biagioli, Attributed under: Attribution 4.0 International (CC BY 4.0))
  - Used to learn about and utilize permutation table, along with general overview of algorithm
  - Utilized for knowledge about interpolation and how to calculate pixel colors
