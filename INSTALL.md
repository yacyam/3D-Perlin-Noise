Project Install Guide:

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

