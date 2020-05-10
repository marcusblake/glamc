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

	extern "C" void initList(struct List *list, int element_size);
	extern "C" void getElement(struct List *list, void *ret);
	extern "C" void addElement(struct List *list, void *element);
	extern "C" void initString(struct String *str, char *elements);
	extern "C" int length(struct String *str);
	extern "C" char getChar(struct String *str, int index);
	extern "C" void concat(struct String *l, struct String *r, struct String *n_str);
#endif
