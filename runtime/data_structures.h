#ifndef DATA_STRUCTURES_H
#define DATA_STRUCTURES_H
	struct List {
		int length;
		int element_size;
		char *list;
	};

	struct String {
		int length;
		char *elements;
	};

	extern "C" void emptyList(struct List *list, int element_size);
	extern "C" void initList(struct List *list, int element_size, int num, char *elements);
	extern "C" void getElement(struct List *list, int index, char *ret);
	extern "C" void addElement(struct List *list, char *element);
	extern "C" void initString(struct String *str, char *elements);
	extern "C" int strlength(struct String *str);
	extern "C" char getChar(struct String *str, int index);
	extern "C" void concat(struct String *l, struct String *r, struct String *n_str);
	extern "C" void prints(struct String *str);
#endif
