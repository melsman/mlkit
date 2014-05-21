
// Hash tables with strings as keys (keys are copied during insert)

typedef struct objectListHashTable {
  char *key;
  char *value;                         /* entry */
  struct objectListHashTable *next;    /* next hashed element */
} ObjectListHashTable;

struct hashTable {
  int size;                            /* Number of elements in the hash table */
  int arraySize;                       /* Size of array */
  ObjectListHashTable *array[0];
};

typedef struct hashTable* HashTable;

HashTable emptyHashTable(int arraySize);

char* lookupHashTable(HashTable h, char* key);
// returns NULL if the entry does not exist

void insertHashTable(HashTable, char* key, char* value);

void freeHashTable(HashTable h);
