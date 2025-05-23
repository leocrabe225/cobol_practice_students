# Makefile for COBOL project

COBOL = cobc
SRC = students.cbl
BIN = students
INPUT_DIR = data
OUTPUT_DIR = output
OUTPUT_FILE = $(OUTPUT_DIR)/output.dat

# Default target
all: $(BIN)

# Compile the COBOL source
$(BIN): $(SRC)
	$(COBOL) -x $(SRC)

# Run with input.dat
run1: $(BIN)
	cp $(INPUT_DIR)/input-1.dat $(INPUT_DIR)/input.dat
	./$(BIN)
	mv $(OUTPUT_FILE) $(OUTPUT_DIR)/output-1.dat

# Run with input-2.dat
run2: $(BIN)
	cp $(INPUT_DIR)/input-2.dat $(INPUT_DIR)/input.dat
	./$(BIN)
	mv $(OUTPUT_FILE) $(OUTPUT_DIR)/output-2.dat

# Run with input-3.dat
run3: $(BIN)
	cp $(INPUT_DIR)/input-3.dat $(INPUT_DIR)/input.dat
	./$(BIN)
	mv $(OUTPUT_FILE) $(OUTPUT_DIR)/output-3.dat

# Run all tests
all-tests: $(BIN)
	@make run1
	@make run2
	@make run3

# Clean build and output files
clean:
	rm -f $(BIN) $(OUTPUT_DIR)/output*.dat $(INPUT_DIR)/input.dat