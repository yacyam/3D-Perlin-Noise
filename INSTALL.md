Project Install Guide:

First: Install OPAM and OCaml
  - Carefully Follow the guide at 
    https://cs3110.github.io/textbook/chapters/preface/install.html
    to install OPAM and OCaml. If Homebrew or MacPorts is not installed,
    follow the guide at either https://brew.sh/ or https://www.macports.org/install.php
    to continue with the OPAM installation.
  - Make sure at the very end you're able to run utop within your terminal,
    and if not, make sure to follow the debugging tools in the textbook guide.


Second: Run opam install graphics in Command Line
  - Accept all prompts given while installing graphics to opam, and when 
    prompted to install XQuartz, accept, and let installation finish
  - Once finished, completely reboot your computer to allow for XQuartz 
    to initialize on startup, make sure that when you run opam list in command line,
    graphics is underlined
  - Once XQuartz is initialized, through Command Line, make the toplevel for this 
    folder the current directory, run make build and then make utop to display the
    software on the screen

