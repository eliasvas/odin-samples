#!/bin/bash

# Usage: ./test_sim86.sh path/to/file.asm_listing

if [ $# -ne 1 ]; then
    echo "Usage: $0 path/to/file.asm_listing"
    exit 1
fi

LISTING_PATH="$1"
SIM86_EXEC="./sim86"
ASM_OUTPUT="disasm.asm"
BIN_OUTPUT="disasm.bin"

# Derive original binary file path
BASENAME=$(basename "$LISTING_PATH" .asm_listing)
DIRNAME=$(dirname "$LISTING_PATH")
#ORIGINAL_BIN="$DIRNAME/$BASENAME.bin"
ORIGINAL_BIN="$LISTING_PATH"

# Step 1: Run sim86 and capture output
echo "Running sim86 on $LISTING_PATH..."
$SIM86_EXEC "$LISTING_PATH" > "$ASM_OUTPUT"
if [ $? -ne 0 ]; then
    echo "ERROR: sim86 failed"
    exit 1
fi

# Step 2: Assemble with NASM
echo "Assembling $ASM_OUTPUT..."
nasm -f bin "$ASM_OUTPUT" -o "$BIN_OUTPUT"
if [ $? -ne 0 ]; then
    echo "ERROR: NASM failed"
    exit 1
fi

# Step 3: Compare with original binary
echo "Comparing $BIN_OUTPUT with $ORIGINAL_BIN..."
if cmp -s "$BIN_OUTPUT" "$ORIGINAL_BIN"; then
    echo "✅ SUCCESS: Binaries match"
    exit 0
else
    echo "❌ FAILURE: Binaries differ"
    echo "Showing diff (hex):"
    diff <(xxd "$BIN_OUTPUT") <(xxd "$ORIGINAL_BIN") | head -20
    exit 2
fi
