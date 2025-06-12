package sim86

import "core:fmt"
import "core:os"
import "core:strings"


ADD_MASK :: 0b100010

Sim86_Register_Name :: enum {
  ax,bx,cx,dx,sp,bp,si,di,al,cl,dl,bl,ah,ch,dh,bh
}

calc_register_for_mov :: proc(reg : u8, w : bool) -> Sim86_Register_Name {
  switch reg {
  case 0b000:
    return (w) ? .ax : .al
  case 0b001:
    return (w) ? .cx : .cl
  case 0b010:
    return (w) ? .dx : .dl
  case 0b011:
    return (w) ? .bx : .bl
  case 0b100:
    return (w) ? .sp : .ah
  case 0b101:
    return (w) ? .bp : .ch
  case 0b110:
    return (w) ? .si : .dh
  case 0b111:
    return (w) ? .di : .bh
  case:
    return .ax
  }
}

sim86_decode :: proc(data : string) {
  reader : strings.Reader
  strings.reader_init(&reader, string(data))
  for {
    c,err := strings.reader_read_byte(&reader); if err != .None do break
    if ((c >> 2) & ADD_MASK) == ADD_MASK {
      byte1 := c
      W := bool((byte1 >> 0) & 0b1) // stands for wide I think
      D := bool((byte1 >> 1) & 0b1) // stands for direction I think
      byte2,_ := strings.reader_read_byte(&reader)
      mod := u8((byte2 >> 6) & 0b11) // Mode
      reg := u8((byte2 >> 3) & 0b111) // Register
      rm  := u8((byte2 >> 0) & 0b111) // R/M

      if D {
        fmt.printf("mov ")
        fmt.print(calc_register_for_mov(reg, W))
        fmt.printf(",")
        fmt.print(calc_register_for_mov(rm, W))
      } else {
        fmt.printf("mov ")
        fmt.print(calc_register_for_mov(rm, W))
        fmt.printf(",")
        fmt.print(calc_register_for_mov(reg, W))
      }
    }

  }
  fmt.println()
}
