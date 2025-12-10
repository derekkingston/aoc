# Compiler
CXX = g++
# Compiler flags
CXXFLAGS = -Wall -Wextra -std=c++17
# Target executable name
TARGET = day01

# Default rule
all: $(TARGET)

# Build target
$(TARGET): main.o
	$(CXX) $(CXXFLAGS) -o $(TARGET) main.o

# Compile source file into object file
main.o: main.cpp
	$(CXX) $(CXXFLAGS) -c main.cpp

# Clean up build files
clean:
	rm -f $(TARGET) *.o