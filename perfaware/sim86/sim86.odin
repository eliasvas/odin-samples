package sim86

import "core:fmt"
import "core:os"
import "core:strings"
import "core:c"

Program :: struct {
  memory_base : ^u8,
  count : u8,
  itr : u8,
}

main :: proc() {
  asm_path := os.args[1]
  data, ok := os.read_entire_file(asm_path, context.allocator)
  if !ok {
      fmt.println("Could not read file: ", asm_path)
      return
  } else {
    fmt.println(";", asm_path, ":")
    sim86_decode(data)
  }
  defer delete(data, context.allocator)
}
