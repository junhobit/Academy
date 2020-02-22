#include <iostream>
#include <string>
#include <math.h>
#include <fstream>
using namespace std;

int hit = 0, access = 0;
typedef struct Cache
{
	unsigned int tag;
	int empty;
} Cache;
void cache_simulator(Cache **cache, unsigned int tag, unsigned int index, int associativity)
{
	//hit
	Cache tmp;
	for (int i = 0; i < associativity; i++)
	{
		if (cache[index][i].tag == tag)
		{
			hit++;
			for (int j = i + 1; j < associativity; j++)
			{
				if (!cache[index][j].empty)
				{
					tmp = cache[index][j - 1];
					cache[index][j - 1] = cache[index][j];
					cache[index][j] = tmp;
				}
				else
					break;
			}
			return;
		}
	}
	//miss and write
	for (int i = 0; i < associativity; i++)
	{
		if (cache[index][i].empty)
		{
			cache[index][i].tag = tag;
			cache[index][i].empty = 0;
			return;
		}
	}
	//replacement
	cache[index][0].tag = tag;
	for (int i = 1; i < associativity; i++)
	{
		tmp = cache[index][i - 1];
		cache[index][i - 1] = cache[index][i];
		cache[index][i] = tmp;
	}
}

int main(int argc, char *argv[])
{
	if (argc != 5)
	{
		cout << "������ ���� �����մϴ�." << endl;
		return 0;
	}
	string filename;
	int cache_byte, cache_block, associativity;
	filename = argv[1];
	cache_byte = atoi(argv[2]);
	cache_block = atoi(argv[3]);
	associativity = atoi(argv[4]);

	unsigned int tag, index;
	ifstream read;
	read.open(filename);
	string op, address_str, count;
	unsigned long address;

	if (read.fail())
	{
		cout << "�ش� ������ �����ϴ�." << endl;
		return 0;
	}
	else if (fmod(log2(cache_block), 2.0) != 0 && fmod(log2(cache_block), 2.0) != 1)
	{
		cout << "Cache block�� ũ�Ⱑ 2�� �������� �ƴմϴ�." << endl;
		return 0;
	}
	else if (associativity != 1 && associativity != 2 && associativity != 4 && associativity != 8)
	{
		cout << "Associativity�� 1, 2, 4, 8 �̿��� ���� �Է� �Ǿ����ϴ�." << endl;
		return 0;
	}
	else if (cache_byte % (cache_block*associativity))
	{
		cout << "Cache ��ü�� ũ�Ⱑ (cache block ũ��)x(associativity)�� ����� �ƴմϴ�." << endl;
		return 0;
	}
	int offset_bit = log2(cache_block);
	int index_bit = log2(cache_byte / cache_block / associativity);

	cout << "tag: " << 32 - index_bit - offset_bit << " bits" << endl;
	cout << "index: " << index_bit << " bits" << endl;
	cout << "offset: " << offset_bit << " bits" << endl;

	int size = cache_byte / cache_block / associativity;
	Cache **cache = new Cache*[size];
	for (int i = 0; i < size; i++)
		cache[i] = new Cache[associativity];

	for (int i = 0; i < size; i++)
		for (int j = 0; j < associativity; j++)
			cache[i][j].empty = 1;

	while (!read.eof())
	{
		read >> op;
		read >> address_str;
		read >> count;
		address = strtoul(address_str.c_str(), NULL, 16);
		tag = address >> (index_bit + offset_bit);
		index = (address - (tag << (index_bit + offset_bit))) >> offset_bit;
		cache_simulator(cache, tag, index, associativity);
		access++;
	}
	cout.precision(2);
	cout << "Result: total access " << access;
	cout << ", hit " << hit;
	cout << ", hit rate " << (double)hit/access << endl;

	for (int i = 0; i < size; i++)
		delete[] cache[i];
	delete[] cache;
	return 0;
}