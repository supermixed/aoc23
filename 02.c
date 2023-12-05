// --- Day 2: Cube Conundrum ---
// 
// You're launched high into the atmosphere! The apex of your trajectory just barely reaches the surface of a large island floating in the sky. You gently land in a fluffy pile of leaves. It's quite cold, but you don't see much snow. An Elf runs over to greet you.
// 
// The Elf explains that you've arrived at Snow Island and apologizes for the lack of snow. He'll be happy to explain the situation, but it's a bit of a walk, so you have some time. They don't get many visitors up here; would you like to play a game in the meantime?
// 
// As you walk, the Elf shows you a small bag and some cubes which are either red, green, or blue. Each time you play this game, he will hide a secret number of cubes of each color in the bag, and your goal is to figure out information about the number of cubes.
// 
// To get information, once a bag has been loaded with cubes, the Elf will reach into the bag, grab a handful of random cubes, show them to you, and then put them back in the bag. He'll do this a few times per game.
// 
// You play several games and record the information from each game (your puzzle input). Each game is listed with its ID number (like the 11 in Game 11: ...) followed by a semicolon-separated list of subsets of cubes that were revealed from the bag (like 3 red, 5 green, 4 blue).
// 
// For example, the record of a few games might look like this:
// 
// Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
// Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
// Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
// Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
// Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
// 
// In game 1, three sets of cubes are revealed from the bag (and then put back again). The first set is 3 blue cubes and 4 red cubes; the second set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.
// 
// The Elf would first like to know which games would have been possible if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?
// 
// In the example above, games 1, 2, and 5 would have been possible if the bag had been loaded with that configuration. However, game 3 would have been impossible because at one point the Elf showed you 20 red cubes at once; similarly, game 4 would also have been impossible because the Elf showed you 15 blue cubes at once. If you add up the IDs of the games that would have been possible, you get 8.
// 
// Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs of those games?

//--- Part Two ---
//
//The Elf says they've stopped producing snow because they aren't getting any water! He isn't sure why the water stopped; however, he can show you how to get to the water source to check it out for yourself. It's just up ahead!
//
//As you continue your walk, the Elf poses a second question: in each game you played, what is the fewest number of cubes of each color that could have been in the bag to make the game possible?
//
//Again consider the example games from earlier:
//
//Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
//Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
//Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
//Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
//Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
//
//    In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes. If any color had even one fewer cube, the game would have been impossible.
//    Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
//    Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
//    Game 4 required at least 14 red, 3 green, and 15 blue cubes.
//    Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.
//
//The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied together. The power of the minimum set of cubes in game 1 is 48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up these five powers produces the sum 2286.
//
//For each game, find the minimum set of cubes that must have been present. What is the sum of the power of these sets?



#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void ParseRound(char* roundStr, uint32_t* r, uint32_t* g, uint32_t* b)
{
	// Takes in a string like
	// 1 green, 2 red, 3 blue
	static const char* colourSplit = ", ";
	const char* tok = strtok(roundStr, colourSplit);
	bool number = true;
	uint32_t n = 0;
	while (tok)
	{
		if (*tok == ' ')
		{
			++tok;
		}

		if (number)
		{
			// Parse the number
			char* end = NULL;
			n = strtoul(tok, &end, 10);
			if (end == tok)
			{
				puts("Parse failure");
			}
		}
		else
		{
			// Set the correct colour's count
			if (!strcmp("red", tok))
			{
				*r = n;
			}
			else if (!strcmp("green", tok))
			{
				*g = n;
			}
			else if (!strcmp("blue", tok))
			{
				*b = n;
			}
			n = 0;
		}

		number = !number;

		tok = strtok(NULL, colourSplit);
	}
}


void ParseGame(char* line, uint32_t* r, uint32_t* g, uint32_t* b)
{
	bool pass = false;

	// Trim the newlines
	const size_t lineLen = strlen(line);
	if ((lineLen > 0) && (line[lineLen - 1] == '\n'))
	{
		line[lineLen - 1] = '\0';
	}

	// Get rid of the game count at the start
	static const char* countPrefix = ": ";
	char* countStart = strstr(line, countPrefix);
	if (countStart)
	{
		countStart += 2;

		static const char* roundSplit = ";";
		char* savePtr = NULL;
		char* tok = strtok_r(countStart, roundSplit, &savePtr);
		while (tok)
		{
			if (*tok == ' ')
			{
				++tok;
			}

			char* roundStr = strdup(tok);
			const size_t roundLen = strlen(roundStr);

			uint32_t roundR = 0;
			uint32_t roundG = 0;
			uint32_t roundB = 0;

			ParseRound(roundStr, &roundR, &roundG, &roundB);

			*r = (*r > roundR) ? *r : roundR;
			*g = (*g > roundG) ? *g : roundG;
			*b = (*b > roundB) ? *b : roundB;

			free(roundStr);

			tok = strtok_r(NULL, roundSplit, &savePtr);
		}
	}
	else
	{
		puts("Skipping line");
	}
}


uint64_t Part1(FILE* f)
{
	uint64_t total = 0;

	char buffer[0x1000] = {'\0'};
	uint64_t game = 1;
	while (fgets(buffer, sizeof(buffer), f))
	{
		uint32_t r = 0;
		uint32_t g = 0;
		uint32_t b = 0;
		ParseGame(buffer, &r, &g, &b);

		if ((r <= 12) && (g <= 13) && (b <= 14))
		{
			total += game;
		}

		++game;
	}

	return total;
}


uint64_t Part2(FILE* f)
{
	uint64_t total = 0;

	char buffer[0x1000] = {'\0'};
	uint64_t sum = 0;

	while (fgets(buffer, sizeof(buffer), f))
	{
		uint32_t r = 0;
		uint32_t g = 0;
		uint32_t b = 0;
		ParseGame(buffer, &r, &g, &b);

		sum += (r * g * b);
	}

	return sum;
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
			perror("Opening input file: ");
		}
	}
	else
	{
		printf("Usage: %s <filename>\n", argv[0]);
	}

	return 0;
}

