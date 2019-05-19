# Chez-GL

This is an interface to OpenGL's core profile for the Chez Scheme system.


# Features

* Support for OpenGL 4.6 core profile
* Convenient high level interface
* Lower level ftype interface provided
* Supported on Windows, macOS, and Linux
* No dependencies; no GLEW, no GLAD, no libepoxy, etc.

# Quickstart Guide

0. Install the [Chez Scheme system](https://github.com/cisco/ChezScheme).

1. Install Chez-GL where the Chez Scheme system can find it.
```bash
export CHEZSCHEMELIBDIRS="/path/to/chez-gl/lib:$CHEZSCHEMELIBDIRS"
```

2. Start running the test examples. Examples use [Chez-SDL](https://github.com/steven741/chez-sdl) for windowing.
```bash
scheme --script simple.ss
```

3. Read the [documentation](https://steven741.github.io/chez-gl/).