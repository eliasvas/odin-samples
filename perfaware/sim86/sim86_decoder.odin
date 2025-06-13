package sim86

import "core:fmt"
import "core:os"
import "core:strings"



Sim86_Register_Name :: enum {
  ax,bx,cx,dx,sp,bp,si,di,al,cl,dl,bl,ah,ch,dh,bh
}
calc_reg_name :: proc(reg : u8, w : bool) -> Sim86_Register_Name {
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

calc_effective_address :: proc(rm : u8, mod : u8, w : bool, disp : u32) ->string {

  // signed displacement shenanigans
  sdisp : i16 = transmute(i16)u16(disp & 0xFFFF)
  if mod != 0b10 do sdisp = cast(i16)transmute(i8)u8(disp & 0xFF)
  str_disp := (sdisp >= 0) ? fmt.tprint("+",abs(sdisp)) : fmt.tprint("-",abs(sdisp))

  switch mod {
    case 0b11:
      return fmt.tprint(calc_reg_name(rm, w))
    case 0b00:
      fallthrough
    case 0b01:
      fallthrough
    case 0b10:
      switch rm {
      case 0b000:
        return (disp != 0) ? fmt.tprint("bx + si", str_disp) : fmt.tprint("bx + si")
      case 0b001:
        return (disp != 0) ? fmt.tprint("bx + di", str_disp) : fmt.tprint("bx + di")
      case 0b010:
        return (disp != 0) ? fmt.tprint("bp + si", str_disp) : fmt.tprint("bp + si")
      case 0b011:
        return (disp != 0) ? fmt.tprint("bp + di", str_disp) : fmt.tprint("bp + di")
      case 0b100:
        return (disp != 0) ? fmt.tprint("si", str_disp) : fmt.tprint("si")
      case 0b101:
        return (disp != 0) ? fmt.tprint("di", str_disp) : fmt.tprint("di")
      case 0b110:
        if mod == 0b00 do return fmt.tprint(disp) // for special IMMEDIATE ADDRESS CASE per the 8086 manual
        return (disp != 0) ? fmt.tprint("bp", str_disp) : fmt.tprint("bp")
      case 0b111:
        return (disp != 0) ? fmt.tprint("bx", str_disp) : fmt.tprint("bx")
      }
    case:
  }
  return "invalid"
} 

decode_immediate_to_register_mov_op :: proc(byte1 : u8, reader : ^strings.Reader) ->string {
  W := bool((byte1 >> 3) & 0b1) // Wide (TM)
  reg := u8((byte1 >> 0) & 0b111) // Register
  disp : u32
  if W {
      byte2,_ := strings.reader_read_byte(reader)
      byte3,_ := strings.reader_read_byte(reader)
      disp = (u32(byte3) << 8) | u32(byte2)
  } else {
      byte2,_ := strings.reader_read_byte(reader)
      disp = u32(byte2)
  }

  reg_name := fmt.tprint(calc_reg_name(reg, W))
  return fmt.tprintf("mov %s,%d", reg_name, disp)
}

decode_memory_to_accumulator_mov_op :: proc(byte1 : u8, reader : ^strings.Reader) ->string {
  W := bool((byte1 >> 0) & 0b1) // Wide (TM)
  byte2,_ := strings.reader_read_byte(reader)
  byte3,_ := strings.reader_read_byte(reader)
  addr := u32(u32(byte3) << 8) | u32(byte2)

  reg_name := "ax"
  return fmt.tprintf("mov %s,[%d]", reg_name, addr)
}

decode_accumulator_to_memory_mov_op :: proc(byte1 : u8, reader : ^strings.Reader) ->string {
  W := bool((byte1 >> 0) & 0b1) // Wide (TM)
  byte2,_ := strings.reader_read_byte(reader)
  byte3,_ := strings.reader_read_byte(reader)
  addr := u32(u32(byte3) << 8) | u32(byte2)

  reg_name := "ax"
  return fmt.tprintf("mov [%d],%s", addr, reg_name)
}

decode_immediate_to_register_memory_mov_op :: proc(byte1 : u8, reader : ^strings.Reader) -> string {
  W := bool((byte1 >> 0) & 0b1) // Wide (TM)
  byte2,_ := strings.reader_read_byte(reader)
  mod := u8((byte2 >> 6) & 0b11) // Mode
  rm  := u8((byte2 >> 0) & 0b111) // R/M

  operand1 : string

  // Depending on the mode find the second operand
  switch mod {
    case 0b11: // Register Mode (no displacement) (R/M is second register)
      disp := u32(0)
      operand1 = calc_effective_address(rm, mod, W, disp)
    case 0b00: // Memory Mode (no displacement) + if R/M == 110 then 16-bit disp. (R/M indicates effective address calculaction)
      disp := u32(0)
      if rm == 0b110 { // special case, if rm is 110 then we got 2 byte immediate disp
        disp1,_ := strings.reader_read_byte(reader)
        disp2,_ := strings.reader_read_byte(reader)
        disp = (u32(disp2) << 8) | u32(disp1)
      }
      operand1 = calc_effective_address(rm, mod, W, disp)
      operand1 = fmt.tprintf("[%s]",operand1)
    case 0b01: // Memory Mode (8-bit displacement follows) (R/M indicates effective address calculaction)
      disp1,_ := strings.reader_read_byte(reader)
      disp := u32(disp1)
      operand1 = calc_effective_address(rm, mod, W, disp)
      operand1 = fmt.tprintf("[%s]",operand1)
    case 0b10: // Memory Mode (16-bit displacement follows) (R/M indicates effective address calculaction)
      disp1,_ := strings.reader_read_byte(reader)
      disp2,_ := strings.reader_read_byte(reader)
      disp := (u32(disp2) << 8) | u32(disp1)
      operand1 = calc_effective_address(rm, mod, W, disp)
      operand1 = fmt.tprintf("[%s]",operand1)
    case:
  }


  data : u32
  if W {
      data1,_ := strings.reader_read_byte(reader)
      data2,_ := strings.reader_read_byte(reader)
      data = (u32(data2) << 8) | u32(data1)
  } else {
      data1,_ := strings.reader_read_byte(reader)
      data = u32(data1)
  }
  operand2 := fmt.tprint(data)



  if W {
    return fmt.tprintf("mov %s, word %s", operand1, operand2)
  } else {
    return fmt.tprintf("mov %s, byte %s", operand1, operand2)
  }
}

decode_register_mem_mov_op :: proc(byte1 : u8, reader : ^strings.Reader) -> string {
  W := bool((byte1 >> 0) & 0b1) // Wide (TM)
  D := bool((byte1 >> 1) & 0b1) // Direction
  byte2,_ := strings.reader_read_byte(reader)
  mod := u8((byte2 >> 6) & 0b11) // Mode
  reg := u8((byte2 >> 3) & 0b111) // Register
  rm  := u8((byte2 >> 0) & 0b111) // R/M

  operand1 := fmt.tprint(calc_reg_name(reg, W))
  operand2 : string;

  // Depending on the mode find the second operand
  switch mod {
    case 0b11: // Register Mode (no displacement) (R/M is second register)
      disp := u32(0)
      operand2 = calc_effective_address(rm, mod, W, disp)
    case 0b00: // Memory Mode (no displacement) + if R/M == 110 then 16-bit disp. (R/M indicates effective address calculaction)
      disp := u32(0)
      if rm == 0b110 { // special case, if rm is 110 then we got 2 byte immediate disp
        byte3,_ := strings.reader_read_byte(reader)
        byte4,_ := strings.reader_read_byte(reader)
        disp = (u32(byte4) << 8) | u32(byte3)
      }
      operand2 = calc_effective_address(rm, mod, W, disp)
      operand2 = fmt.tprintf("[%s]",operand2)
    case 0b01: // Memory Mode (8-bit displacement follows) (R/M indicates effective address calculaction)
      byte3,_ := strings.reader_read_byte(reader)
      disp := u32(byte3)
      operand2 = calc_effective_address(rm, mod, W, disp)
      operand2 = fmt.tprintf("[%s]",operand2)
    case 0b10: // Memory Mode (16-bit displacement follows) (R/M indicates effective address calculaction)
      byte3,_ := strings.reader_read_byte(reader)
      byte4,_ := strings.reader_read_byte(reader)
      disp := (u32(byte4) << 8) | u32(byte3)
      operand2 = calc_effective_address(rm, mod, W, disp)
      operand2 = fmt.tprintf("[%s]",operand2)
    case:
  }

  // If D is set, swap operands!! (this exists because 2nd op can be extended with 
  // the rest of the op, for example use an equation via Mode or add a displacement via R/M)
  if D do operand1,operand2 = operand2,operand1 

  return fmt.tprintf("mov %s,%s", operand2, operand1)
}

sim86_decode :: proc(data : string) {
  reader : strings.Reader
  strings.reader_init(&reader, string(data))
  for {
    byte1,err := strings.reader_read_byte(&reader); if err != .None do break
    cmd : string
    if (byte1 >> 2) & 0b111111 == 0b100010{
      cmd = decode_register_mem_mov_op(byte1, &reader)
    } else if (byte1 >> 4) & 0b1111 == 0b1011 {
      cmd = decode_immediate_to_register_mov_op(byte1, &reader)
    } else if (byte1 >> 1) & 0b1111111 == 0b1010000 {
      cmd = decode_memory_to_accumulator_mov_op(byte1, &reader)
    } else if (byte1 >> 1) & 0b1111111 == 0b1010001 {
      cmd = decode_accumulator_to_memory_mov_op(byte1, &reader)
    } else if (byte1 >> 1) & 0b1111111 == 0b1100011 {
      cmd = decode_immediate_to_register_memory_mov_op(byte1, &reader)
    }

    fmt.println(cmd)
  }
}
