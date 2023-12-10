#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>


typedef enum Rank_e
{
	UNKNOWN = 0,
	HIGH_CARD,
	PAIR,
	TWO_PAIR,
	THREE_OF_A_KIND,
	FULL_HOUSE,
	FOUR_OF_A_KIND,
	FIVE_OF_A_KIND
} Rank;


typedef struct Hand_t {
	char cards_[5];
	uint64_t bid_;
	Rank rank_;
} Hand;


uint8_t HistMax(uint8_t* hist, size_t* maxIdx)
{
	uint8_t max = 0;

	for (size_t i = 0; i < 0x100; ++i)
	{
		if (hist[i] > max)
		{
			max = hist[i];
			*maxIdx = i;
		}
	}

	return max;
}


Rank RankHand(char cards[5], bool enableJoker)
{
	uint8_t hist[0x100] = {0};

	for (size_t i = 0; i < 5; ++i)
	{
		hist[cards[i]]++;
	}

	uint8_t numJokers = 0;
	if (enableJoker)
	{
		numJokers = hist['1'];
		hist['1'] = 0;
	}

	size_t idx = 0;
	switch (HistMax(hist, &idx) + numJokers)
	{
	default:
		puts("Unknown format");
		return UNKNOWN;
	case 5:
		return FIVE_OF_A_KIND;
	case 4:
		return FOUR_OF_A_KIND;
	case 3:
		hist[idx] = 0;
		if (HistMax(hist, &idx) == 2)
		{
			return FULL_HOUSE;
		}
		else
		{
			return THREE_OF_A_KIND;
		}
	case 2:
		hist[idx] = 0;
		if (HistMax(hist, &idx) == 2)
		{
			return TWO_PAIR;
		}
		else
		{
			return PAIR;
		}
	case 1:
		return HIGH_CARD;
	}
}


void RankHands(Hand* hands, size_t numHands, bool enableJoker)
{
	for (size_t i = 0; i < numHands; ++i)
	{
		Hand* hand = hands + i;
		hand->rank_ = RankHand(hand->cards_, enableJoker);
		// printf(
		// 	"%c%c%c%c%c => %d\n",
		// 	hand->cards_[0],
		// 	hand->cards_[1],
		// 	hand->cards_[2],
		// 	hand->cards_[3],
		// 	hand->cards_[4],
		// 	hand->rank_
		// );
	}
}


char ParseCard(char c)
{
	switch (c)
	{
	default:
		return c;
	case 'T':
		return '9' + 1;
	case 'J':
		return '9' + 2;
	case 'Q':
		return '9' + 3;
	case 'K':
		return '9' + 4;
	case 'A':
		return '9' + 5;
	}
}


void ParseHands(FILE* f, Hand* hands, size_t* numHands)
{
	char buffer[0x1000] = {'\0'};

	while (fgets(buffer, sizeof(buffer), f))
	{
		Hand* hand = hands + (*numHands)++;
		const char* tok = strtok(buffer, " ");
		for (size_t i = 0; i < 5; ++i)
		{
			hand->cards_[i] = ParseCard(tok[i]);
		}
		tok = strtok(NULL, " ");
		hand->bid_ = strtoull(tok, NULL, 10);
	}
}


int CompareHands(const void* hv1, const void* hv2)
{
	const Hand* h1 = (const Hand*)hv1;
	const Hand* h2 = (const Hand*)hv2;

	if (h1->rank_ < h2->rank_)
	{
		return -1;
	}
	else if (h1->rank_ > h2->rank_)
	{
		return 1;
	}
	else
	{
		// Card-by-card comparison
		for (size_t i = 0; i < 5; ++i)
		{
			if (h1->cards_[i] < h2->cards_[i])
			{
				return -1;
			}
			else if (h1->cards_[i] > h2->cards_[i])
			{
				return 1;
			}
		}

		// The hands are the same?!
		puts("Same hands??");
		return 0;
	}
}


// Hands are sorted from weakest to strongest
void SortHands(Hand* hands, size_t numHands)
{
	qsort(hands, numHands, sizeof(*hands), CompareHands);
}


uint64_t Part1(const Hand* hands, size_t numHands)
{
	uint64_t total = 0;

	for (size_t i = 0; i < numHands; ++i)
	{
		total += (i + 1) * (hands[i].bid_);
	}

	return total;
}


void FixJokerCardRank(Hand* hands, size_t numHands)
{
	for (size_t i = 0; i < numHands; ++i)
	{
		char* cards = hands[i].cards_;
		for (size_t j = 0; j < 5; ++j)
		{
			if (cards[j] == '9' + 2)
			{
				cards[j] = '1';
			}
		}
	}
}


uint64_t Part2(Hand* hands, size_t numHands)
{
	FixJokerCardRank(hands, numHands);
	RankHands(hands, numHands, true);
	SortHands(hands, numHands);
	return Part1(hands, numHands);;
}


int main(int argc, char** argv)
{
	if (argc == 2)
	{
		FILE* f = fopen(argv[1], "r");
		if (f)
		{
			Hand hands[0x1000];
			size_t numHands = 0;
			ParseHands(f, hands, &numHands);
			fclose(f);
			RankHands(hands, numHands, false);
			SortHands(hands, numHands);
			printf("Part 1: %llu\n", Part1(hands, numHands));
			printf("Part 2: %llu\n", Part2(hands, numHands));
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

