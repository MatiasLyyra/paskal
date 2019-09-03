# Paskal

Compiler for pascal like language.

## Building

The project requires Go LLVM bindings. Please see https://github.com/llvm/llvm-project/tree/master/llvm/bindings/go for their installation. After the bindings are installed and built, simple `go build` should compile the project.

## Usage and Hello world

```
program main

begin
  printf("Hello, world!")
end
```

Compiling the above paskal program `paskal hello.pas hello.o`. Clang can be then used to link the program to make an executable e.g. `clang hello.o -o hello`.

## Currently supported features

### Datatypes
  - Real
  - Integer
  - Boolean
  - Character
  - Static arrays
  - String (mostly supported)
  - Pointers to above mentioned types

### If statements

```
program main
function random: integer
begin
  random := 42
end

begin
  if random() is 42 then begin
    // ...
  end else begin
    // ...
  end
end
```

### Arrays

```
program main
var arr : array[2] of real
realPointer : ^real

begin
  arr[0] := 1.0
  realPointer := @arr[1]
  ^realPointer := 5.0
  printf("Array %f %f", arr[0], arr[1])
end
```

## TODO

- Support for structs
- Support for dynamically allocated memory
- Standard library like functions