// --- Day 1: Trebuchet?! ---

// Something is wrong with global snow production, and you've been selected to take a look. The Elves have even given you a map; on it, they've used stars to mark the top fifty locations that are likely to be having problems.

// You've been doing this long enough to know that to restore snow operations, you need to check all fifty stars by December 25th.

// Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

// You try to ask why they can't just use a weather machine ("not powerful enough") and where they're even sending you ("the sky") and why your map looks mostly blank ("you sure ask a lot of questions") and hang on did you just say the sky ("of course, where do you think snow comes from") when you realize that the Elves are already loading you into a trebuchet ("please hold still, we need to strap you in").

// As they're making the final adjustments, they discover that their calibration document (your puzzle input) has been amended by a very young Elf who was apparently just excited to show off her art skills. Consequently, the Elves are having trouble reading the values on the document.

// The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover. On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.

// For example:

// 1abc2
// pqr3stu8vwx
// a1b2c3d4e5f
// treb7uchet

// In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.

// Consider your entire calibration document. What is the sum of all of the calibration values?
// --- Part Two ---
// 
// Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".
// 
// Equipped with this new information, you now need to find the real first and last digit on each line. For example:
// 
// two1nine
// eightwothree
// abcone2threexyz
// xtwone3four
// 4nineeightseven2
// zoneight234
// 7pqrstsixteen
// 
// In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.
// 
// What is the sum of all of the calibration values?

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>


uint64_t Part1Line(const char* line)
{
	uint64_t lineTotal = 0;
	char leftDigit = '\0';
	char rightDigit = '\0';

	const size_t bufLen = strlen(line);
	// Find a digit from the left
	for (size_t i = 0; !leftDigit && (i < bufLen); ++i)
	{
		if (('0' <= line[i]) && (line[i] <= '9'))
		{
			leftDigit = line[i];
		}
	}

	// Find a digit from the right
	size_t i = bufLen;
	do
	{
		--i;
		if (('0' <= line[i]) && (line[i] <= '9'))
		{
			rightDigit = line[i];
		}
	} while((i > 0) && !rightDigit);

	if (leftDigit && rightDigit)
	{
		lineTotal += (10 * (leftDigit - '0'));
		lineTotal += rightDigit - '0';
	}

	return lineTotal;
}


uint64_t Part1(FILE* f)
{
	uint64_t total = 0;

	char buffer[0x1000] = {'\0'};
	while (fgets(buffer, sizeof(buffer), f))
	{
		total += Part1Line(buffer);
	}

	return total;
}


#define DIM(x) (sizeof(x) / sizeof(x[0]))


void FindDigits(
	const char* line,
	const char** digitStrings,
	size_t digitStringsLength,
	uint64_t* leftDigit,
	const char** leftDigitPtr,
	uint64_t* rightDigit,
	const char** rightDigitPtr
)
{
	for (size_t i = 0; i < digitStringsLength; ++i)
	{
		const char* digitString = digitStrings[i];
		const size_t digitStrlen = strlen(digitString);
		const char* foundDigit = strstr(line, digitString);
		if (foundDigit)
		{
			if (!*leftDigitPtr || (foundDigit < *leftDigitPtr))
			{
				*leftDigit = i;
				*leftDigitPtr = foundDigit;
			}

			// To get the rightmost digit, we need to repeatedly call strstr
			while (foundDigit)
			{
				if (!*rightDigitPtr || (foundDigit > *rightDigitPtr))
				{
					*rightDigit = i;
					*rightDigitPtr = foundDigit;
				}

				foundDigit = strstr(foundDigit + digitStrlen, digitString);
			}
		}
	}
}


uint64_t Part2Line(char* line)
{
	// Trim the newlines
	const size_t lineLen = strlen(line);
	if (lineLen)
	{
		line[lineLen - 1] = '\0';
	}
	
	static const char* digitStrings[] = {
		"NOZERO?",
		"one",
		"two",
		"three",
		"four",
		"five",
		"six",
		"seven",
		"eight",
		"nine"
	};

	static const char* stringDigits[] = {
		"0",
		"1",
		"2",
		"3",
		"4",
		"5",
		"6",
		"7",
		"8",
		"9"
	};

	uint64_t leftDigit = 0;
	const char* leftDigitPtr = NULL;
	uint64_t rightDigit = 0;
	const char* rightDigitPtr = NULL;

	FindDigits(line, digitStrings, DIM(digitStrings), &leftDigit, &leftDigitPtr, &rightDigit, &rightDigitPtr);
	FindDigits(line, stringDigits, DIM(stringDigits), &leftDigit, &leftDigitPtr, &rightDigit, &rightDigitPtr);

	uint64_t lineTotal = 0;
	if (leftDigitPtr && rightDigitPtr)
	{
		lineTotal = (leftDigit * 10) + rightDigit;
	}

	return lineTotal;
}


uint64_t Part2(FILE* f)
{
	uint64_t total = 0;

	char buffer[0x1000] = {'\0'};
	while (fgets(buffer, sizeof(buffer), f))
	{
		total += Part2Line(buffer);
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
			printf("Part 1: %llu\n", Part1(f));
			rewind(f);
			printf("Part 2: %llu\n", Part2(f));
			fclose(f);
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

