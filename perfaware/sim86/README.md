# Sim86
## _An 8086 instruction decoder_

## Description
This is just a decoder for the 8086 instruction set, made for [Computer, Enhance!](https://www.computerenhance.com/) homework

## Building
```sh
git clone --recurse-submodules -j8 git://github.com/eliasvas/odin-samples.git
cd perfaware_hw/sim86
mkdir build
pushd build
odin build .. -out:sim86
./sim86 ../../ce_ref/perfaware/part1/listing_0037_single_register_mov.asm
diff my_disasm ../../perfaware_ref/perfaware/part1/listing_0037_single_register_mov
popd
```
