# Package

version       = "0.1.0"
author        = "Avahe Kellenberger"
description   = "Benchmarking maze solver in Nim"
license       = "MIT"
srcDir        = "src"
bin           = @["maze_bench_nim"]


# Dependencies

requires "nim >= 1.6.0"
requires "pixie >= 3.0.4"

task benchmark, "Runs the benchmark":
  exec "nim r -d:danger --opt:speed --passC:'-flto -march=native' --passL:-flto --gc:arc src/maze_bench_nim.nim"

