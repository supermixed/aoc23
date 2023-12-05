// --- Day 3: Gear Ratios ---
// 
// You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.
// 
// It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.
// 
// "Aaah!"
// 
// You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.
// 
// The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.
// 
// The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)
// 
// Here is an example engine schematic:
// 
// 467..114..
// ...*......
// ..35..633.
// ......#...
// 617*......
// .....+.58.
// ..592.....
// ......755.
// ...$.*....
// .664.598..
// 
// In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.
// 
// Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?
//
// --- Part Two ---
// 
// The engineer finds the missing part and installs it in the engine! As the engine springs to life, you jump in the closest gondola, finally ready to ascend to the water source.
// 
// You don't seem to be going very fast, though. Maybe something is still wrong? Fortunately, the gondola has a phone labeled "help", so you pick it up and the engineer answers.
// 
// Before you can explain the situation, she suggests that you look out the window. There stands the engineer, holding a phone in one hand and waving with the other. You're going so slowly that you haven't even left the station. You exit the gondola.
// 
// The missing part wasn't the only issue - one of the gears in the engine is wrong. A gear is any * symbol that is adjacent to exactly two part numbers. Its gear ratio is the result of multiplying those two numbers together.
// 
// This time, you need to find the gear ratio of every gear and add them all up so that the engineer can figure out which gear needs to be replaced.
// 
// Consider the same engine schematic again:
// 
// 467..114..
// ...*......
// ..35..633.
// ......#...
// 617*......
// .....+.58.
// ..592.....
// ......755.
// ...$.*....
// .664.598..
// 
// In this schematic, there are two gears. The first is in the top left; it has part numbers 467 and 35, so its gear ratio is 16345. The second gear is in the lower right; its gear ratio is 451490. (The * adjacent to 617 is not a gear because it is only adjacent to one part number.) Adding up all of the gear ratios produces 467835.
// 
// What is the sum of all of the gear ratios in your engine schematic?



#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>


#define MAX_WIDTH 0x100
#define MAX_HEIGHT 0x100


typedef struct Schematic_t
{
	size_t width_;
	size_t height_;
	char data_[MAX_WIDTH * MAX_HEIGHT];
} Schematic;


Schematic ParseSchematic(FILE* f)
{
	Schematic schematic = {
		.width_ = 0,
		.height_ = 0,
		.data_ = {'\0'}
	};

	char* data = schematic.data_;
	fgets(data, MAX_WIDTH, f);
	schematic.width_ = strlen(data) - 1;
	if (schematic.width_ > 0)
	{
		schematic.height_++;
		data += schematic.width_; // n.b. this nukes the newline and null-terminator
		
		while (fgets(data, schematic.width_ + 2, f))
		{
			schematic.height_++;
			data += schematic.width_;
		}
	}
	else
	{
		puts("Empty first line?");
	}

	return schematic;
}


void PrintSchematic(const Schematic* schematic)
{
	printf("Width: %llu\nHeight: %llu\n", schematic->width_, schematic->height_);
	char buffer[MAX_WIDTH] = {'\0'};

	for (size_t i = 0; i < schematic->height_; ++i)
	{
		memcpy(buffer, schematic->data_ + (i * schematic->width_), schematic->width_);
		puts(buffer);
	}
}


const char* GetChar(const Schematic* schematic, size_t i, size_t j)
{
	if ((i < schematic->width_) && (j < schematic->height_))
	{
		return schematic->data_ + i + (j * schematic->width_);
	}
	else
	{
		static const char star = '*';
		return &star;
	}
}


uint64_t GetFullNumber(const Schematic* schematic, size_t i, size_t j)
{
	while (isdigit(*GetChar(schematic, --i, j))) {}
	++i;
	// The number starts here
	const uint64_t n = strtoull(GetChar(schematic, i, j), NULL, 10);

	return n;
}


bool CheckAndFn(const Schematic* schematic, uint32_t* numAdjacent, uint64_t* total, size_t i, size_t j, uint64_t(*fn)(uint64_t, uint64_t))
{
	if (isdigit(*GetChar(schematic, i, j)))
	{
		*total = fn(*total, GetFullNumber(schematic, i, j));
		++(*numAdjacent);
		return true;
	}
	else
	{
		return false;
	}
}


uint64_t Add(uint64_t a, uint64_t b)
{
	return a + b;
}


uint64_t Multiply(uint64_t a, uint64_t b)
{
	return a * b;
}


uint64_t FnAdjacentNumbers(const Schematic* schematic, uint32_t* numAdjacent, size_t i, size_t j, uint64_t(*fn)(uint64_t, uint64_t), uint64_t initialTotal)
{
	uint64_t total = initialTotal;

	// scenarios -
	//
	// d..
	// .x.
	//
	// .d.
	// .x.
	//
	// ..d
	// .x.
	//
	// d.d
	// .x.
	//
	// dd.
	// .x.
	//
	// .dd
	// .x.
	//
	// ddd
	// .x.
	//
	// We need to check above first, then left and right
	// only if there was no digit in the middle
	
	if (!CheckAndFn(schematic, numAdjacent, &total, i, j - 1, fn))
	{
		CheckAndFn(schematic, numAdjacent, &total, i - 1, j - 1, fn);
		CheckAndFn(schematic, numAdjacent, &total, i + 1, j - 1, fn);
	}

	CheckAndFn(schematic, numAdjacent, &total, i - 1, j, fn);
	CheckAndFn(schematic, numAdjacent, &total, i + 1, j, fn);

	if (!CheckAndFn(schematic, numAdjacent, &total, i, j + 1, fn))
	{
		CheckAndFn(schematic, numAdjacent, &total, i - 1, j + 1, fn);
		CheckAndFn(schematic, numAdjacent, &total, i + 1, j + 1, fn);
	}

	return total;
}


uint64_t Part1(const Schematic* schematic)
{
	uint64_t total = 0;

	// Find characters that are not digits or periods
	for (size_t j = 0; j < schematic->height_; ++j)
	{
		for (size_t i = 0; i < schematic->width_; ++i)
		{
			const char c = *GetChar(schematic, i, j);
			if ((c != '.') && (!isdigit(c)))
			{
				// We have found a symbol, search for adjacent numbers
				uint32_t na = 0;
				total += FnAdjacentNumbers(schematic, &na, i, j, &Add, 0);
			}
		}
	}
	return total;
}


uint64_t SumGearRatios(const Schematic* schematic, size_t i, size_t j)
{
	uint32_t na = 0;
	uint64_t total = FnAdjacentNumbers(schematic, &na, i, j, &Multiply, 1);

	if (na != 2)
	{
		total = 0;
	}

	return total;
}


uint64_t Part2(const Schematic* schematic)
{
	uint64_t total = 0;

	// Find stars
	for (size_t j = 0; j < schematic->height_; ++j)
	{
		for (size_t i = 0; i < schematic->width_; ++i)
		{
			const char c = *GetChar(schematic, i, j);
			if (c == '*')
			{
				total += SumGearRatios(schematic, i, j);
			}
		}
	}

	return total;
}


int main(int argc, char** argv)
{
	if (argc == 2)
	{
		FILE* f = fopen(argv[1], "r");
		if (f)
		{
			Schematic schematic = ParseSchematic(f);
			PrintSchematic(&schematic);
			fclose(f);
			printf("Part 1: %llu\n", Part1(&schematic));
			printf("Part 2: %llu\n", Part2(&schematic));
		}
		else
		{
			perror("Opening input file");
		}
	}
	else
	{
		printf("Usage: %s <filename>\n", argv[0]);
	}

	return 0;
}

