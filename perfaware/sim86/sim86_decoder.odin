package sim86

import "core:fmt"
import "core:os"
import "core:strings"

Arithmetic_Op_Name :: enum { add, sub, cmp }
calc_arithmetic_op_name :: proc(arop : u8) -> Arithmetic_Op_Name {
  if arop == 0b000 do return .add
  if arop == 0b101 do return .sub
  if arop == 0b111 do return .cmp
  return .add
}
Sim86_Register_Name :: enum { ax,bx,cx,dx,sp,bp,si,di,al,cl,dl,bl,ah,ch,dh,bh }
calc_reg_name :: proc(reg : u8, w : bool) -> Sim86_Register_Name {
  switch reg {
  case 0b000: return (w) ? .ax : .al
  case 0b001: return (w) ? .cx : .cl
  case 0b010: return (w) ? .dx : .dl
  case 0b011: return (w) ? .bx : .bl
  case 0b100: return (w) ? .sp : .ah
  case 0b101: return (w) ? .bp : .ch
  case 0b110: return (w) ? .si : .dh
  case 0b111: return (w) ? .di : .bh
  case: return .ax
  }
}

calc_data :: proc(w : bool, S : bool, reader : ^strings.Reader) -> u32 {
  data : u32
  if w  && !S {
      data1,_ := strings.reader_read_byte(reader)
      data2,_ := strings.reader_read_byte(reader)
      data = (u32(data2) << 8) | u32(data1)
  } else {
      data1,_ := strings.reader_read_byte(reader)
      data = u32(data1)
  }
  return data
}

// transforms a sequence of bytes to the equivalent twos comlement (signed) number
calc_twos_complement_u16 :: proc(num : u16) -> i16 { return transmute(i16)num }
calc_twos_complement_u8 :: proc(num : u8) -> i16 { return i16(transmute(i8)num) }
calc_twos_complement :: proc{ calc_twos_complement_u16, calc_twos_complement_u8, }

calc_effective_address :: proc(rm : u8, mod : u8, w : bool, disp : u32) ->string {
  // convert disp to twos complement
  sdisp := (mod == 0b10) ? calc_twos_complement_u16(u16(disp & 0xFFFF)) : calc_twos_complement_u8(u8(disp & 0xFF))
  str_disp := (sdisp >= 0) ? fmt.tprint("+",abs(sdisp)) : fmt.tprint("-",abs(sdisp))

  address : string
  // TODO: maybe this could be table driven?! YES
  switch mod {
    case 0b11: return fmt.tprint(calc_reg_name(rm, w))
    case 0b00: fallthrough
    case 0b01: fallthrough
    case 0b10:
      switch rm {
      case 0b000: address = fmt.tprint("bx + si")
      case 0b001: address = fmt.tprint("bx + di")
      case 0b010: address = fmt.tprint("bp + si")
      case 0b011: address = fmt.tprint("bp + di")
      case 0b100: address = fmt.tprint("si")
      case 0b101: address = fmt.tprint("di")
      case 0b110: address = (mod == 0b00) ? fmt.tprint(disp) : fmt.tprint("bp") // special IMMEDIATE ADDRESS case in 8086 manual
      case 0b111: address = fmt.tprint("bx")
      }
    case:
  }

  if mod != 0b00 && disp != 0 do address = fmt.tprint(address, str_disp)
  //if !(rm == 0b110 && mod == 0b00) && (mod != 0b11) do address = fmt.tprint(address, str_disp)
  //if (mod == 0b10) || (mod == 0b00 && rm != 0b110) || (mod == 0b01 && disp != 0) do address = fmt.tprint(address, str_disp)
  return address
} 

calc_displacement :: proc(mod : u8, rm : u8, reader : ^strings.Reader) -> u32 {
  disp : u32
  // Depending on the mode find the displacement
  switch mod {
    case 0b11: // Register Mode (no displacement) (R/M is second register)
      disp = u32(0)
    case 0b00: // Memory Mode (no displacement) + if R/M == 110 then 16-bit disp. (R/M indicates effective address calculaction)
      disp = u32(0)
      if rm == 0b110 { // special case, if rm is 110 then we got 2 byte immediate disp
        disp1,_ := strings.reader_read_byte(reader)
        disp2,_ := strings.reader_read_byte(reader)
        disp = (u32(disp2) << 8) | u32(disp1)
      }
    case 0b01: // Memory Mode (8-bit displacement follows) (R/M indicates effective address calculaction)
      disp1,_ := strings.reader_read_byte(reader)
      disp = u32(disp1)
    case 0b10: // Memory Mode (16-bit displacement follows) (R/M indicates effective address calculaction)
      disp1,_ := strings.reader_read_byte(reader)
      disp2,_ := strings.reader_read_byte(reader)
      disp = (u32(disp2) << 8) | u32(disp1)
    case:
  }
  return disp
}

decode_imm_to_reg_mov_op :: proc(byte1 : u8, reader : ^strings.Reader) ->string {
  W := bool((byte1 >> 3) & 0b1) // Wide (TM)
  reg := u8((byte1 >> 0) & 0b111) // Register

  data := calc_data(W,false, reader)

  reg_name := fmt.tprint(calc_reg_name(reg, W))
  return fmt.tprintf("mov %s,%d", reg_name, data)
}

decode_mem_to_accum_mov_op :: proc(byte1 : u8, reader : ^strings.Reader) ->string {
  W := bool((byte1 >> 0) & 0b1) // Wide (TM)
  byte2,_ := strings.reader_read_byte(reader)
  byte3,_ := strings.reader_read_byte(reader)
  addr := u32(u32(byte3) << 8) | u32(byte2)

  reg_name := W ? "ax" : "al"
  return fmt.tprintf("mov %s,[%d]", reg_name, addr)
}

decode_accum_to_mem_mov_op :: proc(byte1 : u8, reader : ^strings.Reader) ->string {
  W := bool((byte1 >> 0) & 0b1) // Wide (TM)
  byte2,_ := strings.reader_read_byte(reader)
  byte3,_ := strings.reader_read_byte(reader)
  addr := u32(u32(byte3) << 8) | u32(byte2)

  reg_name := W ? "ax" : "al"
  return fmt.tprintf("mov [%d],%s", addr, reg_name)
}

decode_imm_to_reg_mem_mov_op :: proc(byte1 : u8, reader : ^strings.Reader) -> string {
  W := bool((byte1 >> 0) & 0b1) // Wide (TM)
  byte2,_ := strings.reader_read_byte(reader)
  mod := u8((byte2 >> 6) & 0b11) // Mode
  rm  := u8((byte2 >> 0) & 0b111) // R/M

  disp := calc_displacement(mod, rm, reader)
  operand1 := calc_effective_address(rm, mod, W, disp)
  if mod != 0b11 do operand1 = fmt.tprintf("[%s]",operand1) // 0b11 is register-to-register so no [] needed


  data := calc_data(W, false, reader)
  operand2 := fmt.tprint(data)

  // TODO: should make this standard, not for this op only!
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
  disp := calc_displacement(mod, rm, reader)
  operand2 := calc_effective_address(rm, mod, W, disp)
  if mod != 0b11 do operand2 = fmt.tprintf("[%s]",operand2) // 0b11 is register-to-register so no [] needed

  // If D is set, swap operands!! (this exists because 2nd op can be extended with 
  // the rest of the op, for example use an equation via Mode or add a displacement via R/M)
  if D do operand1,operand2 = operand2,operand1 

  return fmt.tprintf("mov %s,%s", operand2, operand1)
}

decode_reg_mem_with_reg_to_either_arithmentic_op ::proc(byte1 : u8, reader : ^strings.Reader) ->string {
  W := bool((byte1 >> 0) & 0b1) // Wide (TM)
  D := bool((byte1 >> 1) & 0b1) // Direction
  arop := u8((byte1 >> 3) & 0b111) // Arithmetic Op 
  byte2,_ := strings.reader_read_byte(reader)
  mod := u8((byte2 >> 6) & 0b11) // Mode
  reg := u8((byte2 >> 3) & 0b111) // Register
  rm  := u8((byte2 >> 0) & 0b111) // R/M

  arop_name := calc_arithmetic_op_name(arop)

  operand1 := fmt.tprint(calc_reg_name(reg, W))
  disp := calc_displacement(mod, rm, reader)
  operand2 := calc_effective_address(rm, mod, W, disp)
  if mod != 0b11 do operand2 = fmt.tprintf("[%s]",operand2) // 0b11 is register-to-register so no [] needed

  // If D is set, swap operands!! (this exists because 2nd op can be extended with 
  // the rest of the op, for example use an equation via Mode or add a displacement via R/M)
  if D do operand1,operand2 = operand2,operand1 

  return fmt.tprintf("%s %s,%s", arop_name, operand2, operand1)
}

decode_imm_to_accum_arithmetic_op ::proc(byte1 : u8, reader : ^strings.Reader) ->string {
  W := bool((byte1 >> 0) & 0b1) // Wide (TM)
  arop := u8((byte1 >> 3) & 0b111) // arithmetic op 

  data := calc_data(W, false, reader)
  arop_name := calc_arithmetic_op_name(arop)

  reg_name := W ? "ax" : "al"
  return fmt.tprintf("%s %s,%d", arop_name, reg_name, data)
}

decode_imm_to_reg_mem_arithmetic_op :: proc(byte1 : u8, reader : ^strings.Reader) ->string {
  W := bool((byte1 >> 0) & 0b1) // Wide (TM)
  S := bool((byte1 >> 1) & 0b1) // Signed

  byte2,_ := strings.reader_read_byte(reader)
  mod := u8((byte2 >> 6) & 0b11) // Mode
  arop := u8((byte2 >> 3) & 0b111) // Arithmetic Op
  rm  := u8((byte2 >> 0) & 0b111) // R/M

  disp := calc_displacement(mod, rm, reader)
  operand1 := calc_effective_address(rm, mod, W, disp)
  if mod != 0b11 do operand1 = fmt.tprintf("[%s]",operand1) // 0b11 is register-to-register so no [] needed


  data := calc_data(W, S, reader)
  operand2 := fmt.tprint(data)

  arop_name := calc_arithmetic_op_name(arop)
  return fmt.tprintf("%s %s,%s", arop_name, operand1, operand2)
}

calc_jmp_name :: proc(byte1 : u8) ->string {
  switch byte1 {
  case 0b01110100: return fmt.tprint("je")
  case 0b01111100: return fmt.tprint("jl")
  case 0b01111110: return fmt.tprint("jle")
  case 0b01110010: return fmt.tprint("jb")
  case 0b01110110: return fmt.tprint("jbe")
  case 0b01111010: return fmt.tprint("jp")
  case 0b01110000: return fmt.tprint("jo")
  case 0b01111000: return fmt.tprint("js")
  case 0b01110101: return fmt.tprint("jnz")
  case 0b01111101: return fmt.tprint("jnl")
  case 0b01111111: return fmt.tprint("jnle")
  case 0b01110011: return fmt.tprint("jnb")
  case 0b01110111: return fmt.tprint("jnbe")
  case 0b01111011: return fmt.tprint("jnp")
  case 0b01110001: return fmt.tprint("jno")
  case 0b01111001: return fmt.tprint("jns")
  case 0b11100010: return fmt.tprint("loop")
  case 0b11100001: return fmt.tprint("loopz")
  case 0b11100000: return fmt.tprint("loopnz")
  case 0b11100011: return fmt.tprint("jcxz")
  case: return fmt.tprint("")
  }
}

decode_jmp_op :: proc(byte1 : u8, reader : ^strings.Reader) ->string {
  byte2,_ := strings.reader_read_byte(reader)
  ip_inc8 := calc_twos_complement_u8(byte2)
  return fmt.tprint(calc_jmp_name(byte1), ip_inc8)
}


sim86_decode :: proc(data : string) {
  reader : strings.Reader
  strings.reader_init(&reader, string(data))
  for {
    byte1,err := strings.reader_read_byte(&reader); if err != .None do break
    cmd : string
    if (byte1 >> 2) & 0b111111 == 0b100010 { // 100010dw
      cmd = decode_register_mem_mov_op(byte1, &reader)
    } else if (byte1 >> 4) & 0b1111 == 0b1011 { // 1011wreg
      cmd = decode_imm_to_reg_mov_op(byte1, &reader)
    } else if (byte1 >> 1) & 0b1111111 == 0b1010000 { // 1010000w
      cmd = decode_mem_to_accum_mov_op(byte1, &reader)
    } else if (byte1 >> 1) & 0b1111111 == 0b1010001 { // 1010001w
      cmd = decode_accum_to_mem_mov_op(byte1, &reader)
    } else if (byte1 >> 1) & 0b1111111 == 0b1100011 { // 1100011w
      cmd = decode_imm_to_reg_mem_mov_op(byte1, &reader)
    } else if (byte1 >> 2) & 0b1 == 0b0 && (byte1 >> 6) & 0b11 == 0b00 { // 00xxx0dw
      cmd = decode_reg_mem_with_reg_to_either_arithmentic_op(byte1, &reader)
    } else if (byte1 >> 1) & 0b11 == 0b10 && (byte1 >> 6) & 0b11 == 0b00 { // 00xxx10w
      cmd = decode_imm_to_accum_arithmetic_op(byte1, &reader)
    } else if (byte1 >> 2) & 0b111111 == 0b100000 { // 100000sw
      cmd = decode_imm_to_reg_mem_arithmetic_op(byte1, &reader)
    } else if calc_jmp_name(byte1) != "" {
      cmd = decode_jmp_op(byte1, &reader)
    } else {
      fmt.println("Couldnt parse [", byte1,"]",)
      return
    }
    

    fmt.println(cmd)
  }
}
