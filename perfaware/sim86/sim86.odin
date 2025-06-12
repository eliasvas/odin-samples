package sim86

import "core:fmt"
import "core:os"
import "core:strings"

main :: proc() {
  asm_path := os.args[1]
  data, ok := os.read_entire_file(asm_path, context.allocator)
  if !ok {
      fmt.println("Could not read file: ", asm_path)
      return
  } else {
    fmt.println(";", asm_path, ":")
    sim86_decode(string(data))
    //fmt.println("bits 16")
    //fmt.println("mov ax,bx")
  }
  defer delete(data, context.allocator)
}
