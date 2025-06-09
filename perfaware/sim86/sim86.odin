package sim86

import "core:fmt"
import "core:os"

main :: proc() {
  asm_path := os.args[1]
  data, ok := os.read_entire_file(asm_path, context.allocator)
  if !ok {
      fmt.println("Could not read file: ", asm_path)
      return
  } else {
    fmt.println("Data read from: ", asm_path)
    fmt.println(string(data))
  }
  defer delete(data, context.allocator)
}
