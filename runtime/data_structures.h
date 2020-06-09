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
	extern "C" void make(struct List *list, int element_size, int num, char *initializer);
	extern "C" int lenstr(struct String *str);
	extern "C" void getElement(struct List *list, int index, char *ret);
	extern "C" void addElement(struct List *list, char *element);
	extern "C" void setElement(struct List *list, int index, char *element);
	extern "C" void popElement(struct List *list);
	extern "C" int lenlist(struct List *list);
	extern "C" void initString(struct String *str, char *elements);
	extern "C" int strlength(struct String *str);
	extern "C" char getChar(struct String *str, int index);
	extern "C" void concat(struct String *l, struct String *r, struct String *n_str);
	extern "C" void prints(struct String *str);
	extern "C" void split(struct String *str, char delimeter, struct List *list);
	extern "C" void join(struct List *list, char delimeter,  struct String *str);
#endif
