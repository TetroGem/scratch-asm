# How to Compile:
- Install Rust
- Clone this repo
- cd to this repo's directory
- Run `cargo run [input-file] [output-dir]` (e.g. `cargo run C:/.../my_program.ascm C:/.../out`)
- Load the generated .sb3 file into Scratch!

# Language Guide:
## Scopes
Every body of commands needs to be within a scope. Scopes are created by writing a header for the scope, which can be one of two things:
- `MAIN` (the entry point of the program, runs when the flag is clicked)
- `FN @function` (a function that can be called by other functions)
After the type of scope, variables to be used within the scope's body can also be defined. All of these variables are stack-allocated and are automatically dropped at the end of the scope's body.
Example scope header with variables:
`FN @add $a $b $ret`
These variables will be replaced with their stack addresses at compile-time.
## Tags
Tags can be inserted into scope bodies to allow for jumping between lines of instructions being ran. Tags are created with the `TAG` instruction, followed by the tag name starting with `#`. An example of creating a tag is:
`TAG #loop`
## Commands
The following commands are currently available:
- `CLEAR` = clears stdout
- `OUT [msg]` = prints `[msg]` to the current line of stdout
- `OUTL` = prints a newline character to stdout
- `IN [dest]` = requests input from the user and stores the input into address `[dest]`
- `COPY [val] [dest]` = copies `[val]` into address `[dest]`
- `JMP [tag]` = jumps to the given tag in the source code and continues executing from there (can only jump to tags within the same scope as the `JMP` command)
- `JMPF [cond] [tag]` = jumps to the tag `[tag]` with the same behavior as `JMP`, but only if `[cond]` is true
- `CALL [fn] [...args]` = calls the function `[fn]`, assigning each value in `[...args]` to each of the function's variables in order (variables are not set if not given)
- `CALLF [cond] [fn] [...args]` = calls the function `[fn]` with the same behavior as `CALL`, but only if `[cond]` is true
- `ADD [l] [r] [dest]` = performs `l + r` and stores the result into address `[dest]`
- `SUB [l] [r] [dest]` = performs `l - r` and stores the result into address `[dest]`
- `MUL [l] [r] [dest]` = performs `l * r` and stores the result into address `[dest]`
- `DIV [l] [r] [dest]` = performs `l / r` and stores the result into address `[dest]`
- `MOD [l] [r] [dest]` = performs `l % r` and stores the result into address `[dest]`
- `EQ [l] [r] [dest]` = performs `l == r` and stores the result into address `[dest]`
- `GT [l] [r] [dest]` = performs `l > r` and stores the result into address `[dest]`
- `LT [l] [r] [dest]` = performs `l < r` and stores the result into address `[dest]`
- `RAND [min] [max] [dest]` = generates a random number between `[min]` and `[max]` (inclusive), and store the result into address `[dest]`
- `TIME [dest]` = stores the number of seconds elapsed since the program began into address `[dest]`
- `WAIT [seconds]` = pauses program execution/sleeps for `[seconds]` seconds
## Values
In ScratchASM:
- String and Numbers are the same type, and are written in code with string syntax (e.g., `"1"`, `"Hello!"`)
- Booleans are stored as Numbers (true = `"1"`, false = `"0"`) and are never any other value (passing in another other value for something expecting a Boolean is undefined behavior)
- Variables are always preceded by `$`
- Functions are always preceded by `@`
- Tags are always preceded by `#`
- Instructions are case-insensitive
## Operators
ScratchASM has two special operators:
- `;` denotes anything after it on the same line as a comment (will not be ran as code)
- `*` before a value dereferences it, meaning the value of that expression will be equal to the value at the stack address following the `*`. `*`'s can be chained, allowing for pointers to pointers to pointers to pointers to...
## Examples
View the `examples` folder for example programs!
