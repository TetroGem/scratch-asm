# How to Compile:
- Install Rust
- Clone this repo
- cd to this repo's directory
- Run `cargo run [input-file] [output-dir]` (e.g. `cargo run C:/.../my_program.ascm C:/.../out`)
- Load the generated .sb3 file into Scratch!

# Language Guide:
## Functions
Every body of commands needs to be underneath a function header. These headers can be one of two things:
- MAIN (the entry point of the program, runs when the flag is clicked)
- DEF @function (a function that can be called by other functions)
After the type of function, variables to be used within the function's body can also be defined. All of these variables are stack-allocated and are automatically dropped at the end of the function's body.
Example function header with variables:
`DEF @add $a $b $ret`
These variables will be replaced with their stack address at compile-time.
## Commands
The following commands are currently available:
- `OUT [msg]` = prints `[msg]` to the current line of stdout
- `OUTL` = prints a newline character to stdout
- `IN [dest]` = requests input from the user and stores the input into address `[dest]`
- `COPY [val] [dest]` = copies `[val]` into address `[dest]`
- `CALL [fn] [...args]` = calls the function `[fn]`, assigning each value in `[...args]` to each of the function's variables in order (variables are not set if not given)
- `CALLF [cond] [fn] [...args]` = calls the function `[fn]` with the same behavior as `CALL`, but only if [cond] is true
- `ADD [l] [r] [dest]` = performs `l + r` and stores the result into address `[dest]`
- `SUB [l] [r] [dest]` = performs `l - r` and stores the result into address `[dest]`
- `MUL [l] [r] [dest]` = performs `l * r` and stores the result into address `[dest]`
- `DIV [l] [r] [dest]` = performs `l / r` and stores the result into address `[dest]`
- `MOD [l] [r] [dest]` = performs `l % r` and stores the result into address `[dest]`
- `EQ [l] [r] [dest]` = performs `l == r` and stores the result into address `[dest]`
- `GT [l] [r] [dest]` = performs `l > r` and stores the result into address `[dest]`
- `LT [l] [r] [dest]` = performs `l < r` and stores the result into address `[dest]`
## Values
In ScratchASM:
- String and Numbers are the same type, and are written in code with string syntax (e.g., "1", "Hello!")
- Booleans are stored as Numbers (true = "1", false = "0") and are never any other value (passing in another other value for something expecting a Boolean is undefined behavior)
- Variables are always preceded by $
- Functions are always preceded by @
- Instructions are case-insensitive
## Operators
ScratchASM has two special operators:
- ; denotes anything after it on the same line as a comment (will not be ran as code)
- * before a value dereferences it, meaning the value of that expression will be equal to the value at the stack address following the *. *'s can be chained, allowing for pointers to pointers to pointers to pointers to...
## Examples
View the `examples` folder for example programs!
