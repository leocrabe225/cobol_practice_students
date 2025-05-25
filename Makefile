# Makefile for COBOL project

COBOL = cobc
SRC = students.cbl
BIN = students

INPUT_DIR = data
OUTPUT_DIR = output
TEMP_INPUT = $(INPUT_DIR)/input.dat
OUTPUT_FILE = $(OUTPUT_DIR)/output.dat

# Default target
all: $(BIN)

# Compile COBOL source
$(BIN): $(SRC)
	$(COBOL) -x $(SRC)

# Ensure output directory exists
$(OUTPUT_DIR):
	mkdir -p $(OUTPUT_DIR)

# Run a specific test: make run F=1
run: $(BIN) $(OUTPUT_DIR)
	@if [ -z "$(F)" ]; then \
		echo "Error: Please specify a file number with F=<n>, e.g., make run F=2"; \
		exit 1; \
	fi
	@if [ ! -f $(INPUT_DIR)/input-$(F).dat ]; then \
		echo "Error: File $(INPUT_DIR)/input-$(F).dat does not exist."; \
		exit 1; \
	fi
	@echo "Running with input-$(F).dat..."
	cp $(INPUT_DIR)/input-$(F).dat $(INPUT_DIR)/input.dat
	./$(BIN)
	mv $(OUTPUT_FILE) $(OUTPUT_DIR)/output-$(F).dat
	@echo "Output saved to $(OUTPUT_DIR)/output-$(F).dat"

# Run all available input-*.dat files
run-all: $(BIN) $(OUTPUT_DIR)
	@for input in $(INPUT_DIR)/input-*.dat; do \
		num=$$(echo $$input | sed -E 's/.*input-([0-9]+)\.dat/\1/'); \
		echo "Running with input file $$input (test $$num)..."; \
		cp $$input $(TEMP_INPUT); \
		./$(BIN); \
		mv $(OUTPUT_FILE) $(OUTPUT_DIR)/output-$$num.dat; \
		echo "Output saved to $(OUTPUT_DIR)/output-$$num.dat"; \
	done; \
	echo "All tests completed."

# Clean build and output files
clean:
	rm -f $(BIN) $(TEMP_INPUT)
	rm -rf $(OUTPUT_DIR)