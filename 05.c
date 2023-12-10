// --- Day 5: If You Give A Seed A Fertilizer ---
// 
// You take the boat and find the gardener right where you were told he would be: managing a giant "garden" that looks more to you like a farm.
// 
// "A water source? Island Island is the water source!" You point out that Snow Island isn't receiving any water.
// 
// "Oh, we had to stop the water because we ran out of sand to filter it with! Can't make snow with dirty water. Don't worry, I'm sure we'll get more sand soon; we only turned off the water a few days... weeks... oh no." His face sinks into a look of horrified realization.
// 
// "I've been so busy making sure everyone here has food that I completely forgot to check why we stopped getting more sand! There's a ferry leaving soon that is headed over in that direction - it's much faster than your boat. Could you please go check it out?"
// 
// You barely have time to agree to this request when he brings up another. "While you wait for the ferry, maybe you can help us with our food production problem. The latest Island Island Almanac just arrived and we're having trouble making sense of it."
// 
// The almanac (your puzzle input) lists all of the seeds that need to be planted. It also lists what type of soil to use with each kind of seed, what type of fertilizer to use with each kind of soil, what type of water to use with each kind of fertilizer, and so on. Every type of seed, soil, fertilizer and so on is identified with a number, but numbers are reused by each category - that is, soil 123 and fertilizer 123 aren't necessarily related to each other.
// 
// For example:
// 
// seeds: 79 14 55 13
// 
// seed-to-soil map:
// 50 98 2
// 52 50 48
// 
// soil-to-fertilizer map:
// 0 15 37
// 37 52 2
// 39 0 15
// 
// fertilizer-to-water map:
// 49 53 8
// 0 11 42
// 42 0 7
// 57 7 4
// 
// water-to-light map:
// 88 18 7
// 18 25 70
// 
// light-to-temperature map:
// 45 77 23
// 81 45 19
// 68 64 13
// 
// temperature-to-humidity map:
// 0 69 1
// 1 0 69
// 
// humidity-to-location map:
// 60 56 37
// 56 93 4
// 
// The almanac starts by listing which seeds need to be planted: seeds 79, 14, 55, and 13.
// 
// The rest of the almanac contains a list of maps which describe how to convert numbers from a source category into numbers in a destination category. That is, the section that starts with seed-to-soil map: describes how to convert a seed number (the source) to a soil number (the destination). This lets the gardener and his team know which soil to use with which seeds, which water to use with which fertilizer, and so on.
// 
// Rather than list every source number and its corresponding destination number one by one, the maps describe entire ranges of numbers that can be converted. Each line within a map contains three numbers: the destination range start, the source range start, and the range length.
// 
// Consider again the example seed-to-soil map:
// 
// 50 98 2
// 52 50 48
// 
// The first line has a destination range start of 50, a source range start of 98, and a range length of 2. This line means that the source range starts at 98 and contains two values: 98 and 99. The destination range is the same length, but it starts at 50, so its two values are 50 and 51. With this information, you know that seed number 98 corresponds to soil number 50 and that seed number 99 corresponds to soil number 51.
// 
// The second line means that the source range starts at 50 and contains 48 values: 50, 51, ..., 96, 97. This corresponds to a destination range starting at 52 and also containing 48 values: 52, 53, ..., 98, 99. So, seed number 53 corresponds to soil number 55.
// 
// Any source numbers that aren't mapped correspond to the same destination number. So, seed number 10 corresponds to soil number 10.
// 
// So, the entire list of seed numbers and their corresponding soil numbers looks like this:
// 
// seed  soil
// 0     0
// 1     1
// ...   ...
// 48    48
// 49    49
// 50    52
// 51    53
// ...   ...
// 96    98
// 97    99
// 98    50
// 99    51
// 
// With this map, you can look up the soil number required for each initial seed number:
// 
//     Seed number 79 corresponds to soil number 81.
//     Seed number 14 corresponds to soil number 14.
//     Seed number 55 corresponds to soil number 57.
//     Seed number 13 corresponds to soil number 13.
// 
// The gardener and his team want to get started as soon as possible, so they'd like to know the closest location that needs a seed. Using these maps, find the lowest location number that corresponds to any of the initial seeds. To do this, you'll need to convert each seed number through other categories until you can find its corresponding location number. In this example, the corresponding types are:
// 
//     Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78, humidity 78, location 82.
//     Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42, humidity 43, location 43.
//     Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82, humidity 82, location 86.
//     Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.
// 
// So, the lowest location number in this example is 35.
// 
// What is the lowest location number that corresponds to any of the initial seed numbers?
// 
// --- Part Two ---
//
// Everyone will starve if you only plant such a small number of seeds. Re-reading the almanac, it looks like the seeds: line actually describes ranges of seed numbers.
// 
// The values on the initial seeds: line come in pairs. Within each pair, the first value is the start of the range and the second value is the length of the range. So, in the first line of the example above:
// 
// seeds: 79 14 55 13
// 
// This line describes two ranges of seed numbers to be planted in the garden. The first range starts with seed number 79 and contains 14 values: 79, 80, ..., 91, 92. The second range starts with seed number 55 and contains 13 values: 55, 56, ..., 66, 67.
// 
// Now, rather than considering four seed numbers, you need to consider a total of 27 seed numbers.
// 
// In the above example, the lowest location number can be obtained from seed number 82, which corresponds to soil 84, fertilizer 84, water 84, light 77, temperature 45, humidity 46, and location 46. So, the lowest location number is 46.
// 
// Consider all of the initial seed numbers listed in the ranges on the first line of the almanac. What is the lowest location number that corresponds to any of the initial seed numbers?



#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>


typedef struct Rule_t
{
	uint64_t destination_;
	uint64_t source_;
	uint64_t length_;
} Rule;


typedef struct Map_t
{
	Rule rule_[0x100];
	size_t numRules_;
} Map;


typedef struct Maps_t
{
	Map map_[0x100];
	size_t numMaps_;
} Maps;


typedef struct Seeds_t
{
	uint64_t seed_[0x100];
	size_t numSeeds_;
} Seeds;


typedef struct Puzzle_t
{
	Seeds seeds_;
	Maps maps_;
} Puzzle;


Seeds ParseSeeds(FILE* f)
{
	Seeds seeds = {
		.numSeeds_ = 0
	};
	char buffer[0x1000] = {'\0'};
	char* bp = buffer;

	if (fgets(buffer, sizeof(buffer), f))
	{
		bp += sizeof("seeds:");

		// Read the seed values out
		const char* tok = strtok(bp, " ");
		while (tok)
		{
			const uint64_t n = strtoull(tok, NULL, 10);
			if (n != 0)
			{
				seeds.seed_[seeds.numSeeds_++] = n;
			}
			tok = strtok(NULL, " ");
		}

		// Read the blank line
		fgets(buffer, sizeof(buffer), f);
	}
	else
	{
		puts("Unknown format");
	}

	return seeds;
}


uint64_t ParseNum(const char* s, bool* numValid)
{
	uint64_t n = 0;
	char* end = NULL;
	n = strtoull(s, &end, 10);
	*numValid = end != s;
	
	return n;
}


Rule ParseRule(FILE* f, bool* ruleValid)
{
	Rule rule = {
		.destination_ = 0,
		.source_ = 0,
		.length_ = 0
	};
	char buffer[0x1000] = {'\0'};
	
	if (fgets(buffer, sizeof(buffer), f))
	{
		// There should be three numbers, separated by spaces
		const char* tok = strtok(buffer, " ");
		if (tok)
		{
			rule.destination_ = ParseNum(tok, ruleValid);
			tok = strtok(NULL, " ");
		}

		if (tok)
		{
			rule.source_ = ParseNum(tok, ruleValid);
			tok = strtok(NULL, " ");
		}

		if (tok)
		{
			rule.length_ = ParseNum(tok, ruleValid);
		}
	}
	else
	{
		// End of file
		*ruleValid = false;
	}

	return rule;
}


Map ParseMap(FILE* f)
{
	Map map = {
		.numRules_ = 0
	};

	// The first line should mention "map:"
	char buffer[0x1000] = {'\0'};
	if (fgets(buffer, sizeof(buffer), f) && strstr(buffer, "map:"))
	{
		bool ruleValid = false;
		Rule rule = ParseRule(f, &ruleValid);
		while (ruleValid)
		{
			map.rule_[map.numRules_++] = rule;
			rule = ParseRule(f, &ruleValid);
		}
		// n.b. we already consumed the blank line
	}

	return map;
}


Maps ParseMaps(FILE* f)
{
	Maps maps = {
		.numMaps_ = 0
	};

	Map map = ParseMap(f);
	while (map.numRules_ > 0)
	{
		maps.map_[maps.numMaps_++] = map;
		map = ParseMap(f);
	}
	
	return maps;
}


Puzzle ParsePuzzle(FILE* f)
{
	Puzzle puzzle = {
		.seeds_ = ParseSeeds(f),
		.maps_ = ParseMaps(f)
	};

	return puzzle;
}


uint64_t ApplyMap(const Map* map, uint64_t n)
{
	for (size_t ruleIdx = 0; ruleIdx < map->numRules_; ++ruleIdx)
	{
		const Rule* rule = map->rule_ + ruleIdx;
		if ((rule->source_ <= n) && (n < rule->source_ + rule->length_))
		{
			n += rule->destination_ - rule->source_;
			break;
		}
	}

	return n;
}


uint64_t SeedLocation(uint64_t seed, const Maps* maps)
{
	for (size_t mapIdx = 0; mapIdx < maps->numMaps_; ++mapIdx)
	{
		const Map* map = maps->map_ + mapIdx;
		seed = ApplyMap(map, seed);
	}

	return seed;
}


uint64_t Part1(const Puzzle* puzzle)
{
	uint64_t lowestLocation = UINT64_MAX;
	const Seeds* seeds = &puzzle->seeds_;
	const Maps* maps = &puzzle->maps_;
	for (size_t seedIdx = 0; seedIdx < seeds->numSeeds_; ++seedIdx)
	{
		const uint64_t location = SeedLocation(seeds->seed_[seedIdx], maps);
		lowestLocation = (location < lowestLocation) ? location : lowestLocation;
	}

	return lowestLocation;
}


uint64_t Part2(const Puzzle* puzzle)
{
	uint64_t lowestLocation = UINT64_MAX;
	const Seeds* seeds = &puzzle->seeds_;
	const Maps* maps = &puzzle->maps_;
	for (size_t seedIdx = 0; seedIdx < seeds->numSeeds_; seedIdx += 2)
	{
		const uint64_t seedStart = seeds->seed_[seedIdx];
		const uint64_t seedEnd = seedStart + seeds->seed_[seedIdx + 1];
		for (uint64_t seed = seedStart; seed < seedEnd; ++seed)
		{
			const uint64_t location = SeedLocation(seed, maps);
			lowestLocation = (location < lowestLocation) ? location : lowestLocation;
		}
		putchar('.');
	}

	putchar('\n');

	return lowestLocation;
}


int main(int argc, char** argv)
{
	if (argc == 2)
	{
		FILE* f = fopen(argv[1], "r");
		if (f)
		{
			Puzzle puzzle = ParsePuzzle(f);
			fclose(f);
			printf("Part 1: %llu\n", Part1(&puzzle));
			printf("Part 2: %llu\n", Part2(&puzzle));
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

