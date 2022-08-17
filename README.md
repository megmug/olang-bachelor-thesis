# ABOUT

O is an imperative, statically typed and object-oriented mini-language.
The implementation is done using the Haskell programming language.
It provides the compiler as well as the runtime environment which is a simulated abstract machine.

# HOW TO USE

Save your program in a text file.
If you have nix installed, type "nix-shell" on the terminal in the project root to derive a compatible development environment.
Then run "stack run olang" with the program file path as parameter.
Example: "stack run olang ./resources/example-programs/ackermann.olang"

# REQUIREMENTS

- Operating system:
    * Linux
    * Microsoft Windows 10
    * Microsoft Windows 11
    * Mac OS - if it doesn't work, see the comment in stack.yaml for a workaround!
- AND EITHER working haskell-stack >= 2.7.5 installation OR (recommended for full dev-environment) working nix >= 2.6 installation