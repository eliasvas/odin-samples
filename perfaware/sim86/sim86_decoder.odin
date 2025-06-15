package sim86

import "core:fmt"
import "core:os"
import "core:strings"

Reg_Loc :: enum { a,b,c,d,sp,bp,si,di }
Register_Def :: struct {
  location: Reg_Loc,
  count : u8,
  offset : u8, // e.g ah is offset=1 count=1, ax is offset=0 count=2
}

calc_register_def :: proc(reg : u8, w : bool) ->Register_Def {
  switch reg {
  case 0b000: return (w) ? {.a,2,0} : {.a,1,0} 
  case 0b001: return (w) ? {.c,2,0} : {.c,1,0} 
  case 0b010: return (w) ? {.d,2,0} : {.d,1,0} 
  case 0b011: return (w) ? {.b,2,0} : {.b,1,0} 
  case 0b100: return (w) ? {.sp,1,0} : {.a,1,1} 
  case 0b101: return (w) ? {.bp,1,0} : {.c,1,1} 
  case 0b110: return (w) ? {.si,1,0} : {.d,1,1} 
  case 0b111: return (w) ? {.di,1,0} : {.b,1,1} 
  case: return {.a,2,0} 
  }
}

Effective_Address_Equation :: enum {
  bx_plus_si,
  bx_plus_di,
  bp_plus_si,
  bp_plus_di,
  si,
  di,
  bp,
  bx,
  DIRECT_ACCESS, // this means immediate u16 displacement right?
}

Operand :: union {
  Register_Def,
  Effective_Address_Def,
  i8, // immediate i8
  u16, // immediate u16
}

Instruction_Type :: enum { mov, add, sub, cmp, jmp }

Instruction_Flag :: enum {
  WIDE,
}

Instruction :: struct {
  type : Instruction_Type,
  flags : bit_set[Instruction_Flag],
  fields : bit_set[Instruction_Field],

  operands : [2]Operand,
  operand_count : u32
}

instruction_print :: proc(inst : Instruction) {
  //fmt.println("mov cx, bx")
  fmt.print(inst.type)
  fmt.print(" ")

  if inst.operand_count == 1 {
    operand_print(inst.operands[0])
  } else if inst.operand_count == 2 {
    operand_print(inst.operands[0])
    fmt.print(",")
    operand_print(inst.operands[1])
  }

  fmt.println()
}

// transforms a sequence of bytes to the equivalent twos comlement (signed) number
calc_twos_complement_u16 :: proc(num : u16) -> i16 { return transmute(i16)num }
calc_twos_complement_u8 :: proc(num : u8) -> i16 { return i16(transmute(i8)num) }
calc_twos_complement :: proc{ calc_twos_complement_u16, calc_twos_complement_u8, }


ActualAddress :: struct {
  equation : Effective_Address_Equation,
  eq_disp : i16,
}

Effective_Address_Def :: union {
  ActualAddress,
  u16,
  Register_Def,
}

operand_print :: proc(op : Operand) {
  switch v in op {
  case Register_Def:
    postfix := (v.count == 2) ? "x" : (v.offset == 0) ? "l" : "h"
    fmt.print(v.location)
    if u32(v.location) <= u32(Reg_Loc.d) do fmt.print(postfix)
  case Effective_Address_Def:
    switch addr in v {
    case ActualAddress:
      fmt.print(addr.equation)
      if addr.eq_disp < 0.0 {
        fmt.print("-", abs(addr.eq_disp)) 
      } else {
        fmt.print("+", abs(addr.eq_disp)) 
      }
    case u16:
      fmt.print(addr)
    case Register_Def:
      postfix := (addr.count == 2) ? "x" : (addr.offset == 0) ? "l" : "h"
      fmt.print(addr.location)
      if u32(addr.location) <= u32(Reg_Loc.d) do fmt.print(postfix)
    }
  case i8:
    fmt.print(v)
  case u16:
    fmt.print(v)
  case: 
    fmt.println("Uknown Operand!?")
  }
} 

calc_effective_address :: proc(rm : u8, mod : u8, w : bool, disp : u32) ->Effective_Address_Def {
  def : Effective_Address_Def

  // convert disp to twos complement
  sdisp := (mod == 0b10) ? calc_twos_complement_u16(u16(disp & 0xFFFF)) : calc_twos_complement_u8(u8(disp & 0xFF))
  //str_disp := (sdisp >= 0) ? fmt.tprint("+",abs(sdisp)) : fmt.tprint("-",abs(sdisp))


  // TODO: maybe this could be table driven?! YES
  switch mod {
    case 0b11: def = calc_register_def(rm, w)
    case 0b00: fallthrough
    case 0b01: fallthrough
    case 0b10:
      switch rm {
      case 0b000: def = ActualAddress{.bx_plus_si, sdisp}
      case 0b001: def = ActualAddress{.bx_plus_di,sdisp}
      case 0b010: def = ActualAddress{.bp_plus_si,sdisp}
      case 0b011: def = ActualAddress{.bp_plus_di,sdisp}
      case 0b100: def = ActualAddress{.si,sdisp}
      case 0b101: def = ActualAddress{.di,sdisp}
      case 0b110: def = (mod == 0b00) ? u16(disp) : ActualAddress{.bx,sdisp} // special IMMEDIATE ADDRESS case in 8086 manual
      case 0b111: def = ActualAddress{.bx,sdisp}
      }
    case:
  }

  return def
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

/*
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
  return ""
}

decode_register_mem_mov_op :: proc(byte1 : u8, reader : ^strings.Reader) -> string {
  /*
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
  */
  return ""
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
  return ""
}

decode_imm_to_accum_arithmetic_op ::proc(byte1 : u8, reader : ^strings.Reader) ->string {
  W := bool((byte1 >> 0) & 0b1) // Wide (TM)
  arop := u8((byte1 >> 3) & 0b111) // arithmetic op 

  data := calc_data(W, false, reader)
  arop_name := calc_arithmetic_op_name(arop)

  reg_name := W ? "ax" : "al"
  return fmt.tprintf("%s %s,%d", arop_name, reg_name, data)
  return ""
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
  return ""
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
  return ""
}

decode_jmp_op :: proc(byte1 : u8, reader : ^strings.Reader) ->string {
  byte2,_ := strings.reader_read_byte(reader)
  ip_inc8 := calc_twos_complement_u8(byte2)
  return fmt.tprint(calc_jmp_name(byte1), ip_inc8)
}
*/

Instruction_Field :: enum {
  D_FIELD,
  W_FIELD,
  S_FIELD,
  REG_FIELD,
  MOD_FIELD,
  RM_FIELD,
  DISP_FIELD,
  DATA_FIELD,
}
Instruction_Field_Def :: struct {
  field : Instruction_Field,
  byte_index : u32,
  shifts_required : u32,
}

Instruction_Def :: struct {
  type : Instruction_Type,
  prefix : u8,
  prefix_shifts_required : u8, // we can calc this on-demand right?
  defs : []Instruction_Field_Def,
}

// All handled opcodes are baked in this Array :)
OPCODES := []Instruction_Def{
  Instruction_Def{.mov, 0b100010, 2, []Instruction_Field_Def{Instruction_Field_Def{.D_FIELD,0,1}, Instruction_Field_Def{.W_FIELD,0,0}, Instruction_Field_Def{.MOD_FIELD,1,6},Instruction_Field_Def{.REG_FIELD,1,3},Instruction_Field_Def{.RM_FIELD,1,0},Instruction_Field_Def{.DISP_FIELD,2,0}}},
}

match_opcode :: proc(byte1 : u8) -> (def: Instruction_Def, found: bool) {
  for inst_def in OPCODES {
    if byte1 >> inst_def.prefix_shifts_required == inst_def.prefix do return inst_def,true
  }
  return {},false
}

decode_instruction_at :: proc(mem : []u8) -> (size : u8) {
  // globals
  d,w,s : bool
  reg,rm,mod : u8

  disp,data : u32
  disp_byte_idx,data_byte_idx : u32

  // decode instruction from the definition
  inst_def, found := match_opcode(mem[0])

  inst_byte_count:u32 = 1 // I don't love this.. should be able to calc on demand

  if found {
    inst : Instruction
    // Do the magic!
    inst.type = inst_def.type
    for field_def in inst_def.defs {
      inst_byte_count = max(inst_byte_count, field_def.byte_index+1)
      switch field_def.field {
      case .W_FIELD: inst.fields |= {.W_FIELD}; w = auto_cast ((mem[field_def.byte_index] >> field_def.shifts_required) & 0b1)
      case .D_FIELD: inst.fields |= {.D_FIELD}; d = auto_cast ((mem[field_def.byte_index] >> field_def.shifts_required) & 0b1)
      case .S_FIELD: inst.fields |= {.S_FIELD}; s = auto_cast ((mem[field_def.byte_index] >> field_def.shifts_required) & 0b1)
      case .REG_FIELD: inst.fields |= {.REG_FIELD}; reg = (mem[field_def.byte_index] >> field_def.shifts_required) & 0b111
      case .MOD_FIELD: inst.fields |= {.MOD_FIELD}; mod = (mem[field_def.byte_index] >> field_def.shifts_required) & 0b11
      case .RM_FIELD: inst.fields |= {.RM_FIELD}; rm = (mem[field_def.byte_index] >> field_def.shifts_required) & 0b111
      case .DISP_FIELD: inst.fields  |= {.DISP_FIELD}; disp_byte_idx = field_def.byte_index
      case .DATA_FIELD: inst.fields  |= {.DATA_FIELD}; data_byte_idx = field_def.byte_index
      case:
      }
    }

    // If the instruction contains displacement, fetch it
    if .DISP_FIELD in inst.fields {
      switch mod {
        case 0b11: // Register Mode (no displacement) (R/M is second register)
          disp = u32(0)
        case 0b00: // Memory Mode (no displacement) + if R/M == 110 then 16-bit disp. (R/M indicates effective address calculaction)
          if rm == 0b110 { // special case, if rm is 110 then we got 2 byte immediate disp
            disp = (u32(mem[disp_byte_idx + 1]) << 8) | u32(mem[disp_byte_idx])
            inst_byte_count = max(inst_byte_count, disp_byte_idx + 2)
          }
        case 0b01: // Memory Mode (8-bit displacement follows) (R/M indicates effective address calculaction)
          disp = u32(mem[disp_byte_idx + 1])
        case 0b10: // Memory Mode (16-bit displacement follows) (R/M indicates effective address calculaction)
          disp = (u32(mem[disp_byte_idx + 1]) << 8) | u32(mem[disp_byte_idx])
          inst_byte_count = max(inst_byte_count, disp_byte_idx + 2)
        case:
      }
    }

    // If the instruction contains data, fetch it
    if .DATA_FIELD in inst.fields {
      if w && !s {
        data = auto_cast ((mem[data_byte_idx + 1] << 8) | mem[data_byte_idx])
        inst_byte_count = max(inst_byte_count, data_byte_idx + 2)
      } else {
        data = auto_cast mem[data_byte_idx]
      }
    }

    // Decode operands
    if .DISP_FIELD in inst.fields {
      inst.operands[inst.operand_count] = calc_effective_address(rm, mod, w, disp)
      //if mod != 0b11 do inst.operands[inst.operand_count] = fmt.tprintf("[%s]",operand2) // 0b11 is register-to-register so no [] needed
      inst.operand_count+=1
    }

    if .REG_FIELD in inst.fields {
      inst.operands[inst.operand_count] = calc_register_def(reg, w)
      inst.operand_count+=1
    }

    if .DATA_FIELD in inst.fields {
      inst.operands[inst.operand_count] = u16(data)
      inst.operand_count+=1
    }

    // If D is set, swap operands!! (this exists because 2nd op can be extended with 
    // the rest of the op, for example use an equation via Mode or add a displacement via R/M)
    if .D_FIELD in inst.fields {
      if d do inst.operands[0],inst.operands[1] = inst.operands[1],inst.operands[0]
    }

    //-------------------
    instruction_print(inst)
  } else {
    fmt.printf("Instruction with first byte %b not registered!\n", mem[0])
  }

  return auto_cast inst_byte_count
}

sim86_decode :: proc(data : []u8) {
  itr := 0
  for itr < len(data) {
    itr += cast(int)decode_instruction_at(data[itr:])
  }
}
