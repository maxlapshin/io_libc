#define _GNU_SOURCE
#include <string.h>
#include <stdio.h>
#include "erl_nif.h"

#define ARG_NONE   0
#define ARG_INT    1
#define ARG_LONG   2
#define ARG_UINT   3
#define ARG_ULONG  4
#define ARG_DOUBLE 6
#define ARG_STRING 7

// Fetchers for every argument type
int             fetch_int(ErlNifEnv*, ERL_NIF_TERM*);
long            fetch_long(ErlNifEnv*, ERL_NIF_TERM*);
unsigned int    fetch_uint(ErlNifEnv*, ERL_NIF_TERM*);
unsigned long   fetch_ulong(ErlNifEnv*, ERL_NIF_TERM*);
double          fetch_double(ErlNifEnv*, ERL_NIF_TERM*);
char*           fetch_string(ErlNifEnv*, ERL_NIF_TERM*);

// Iterable function which gets first convertion spec from input and appends formatted data to given binary
int format_first(ErlNifEnv*, ErlNifBinary*, char**, ERL_NIF_TERM*);

// fwrite implementation
static ERL_NIF_TERM
io_libc_fwrite(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  char fmt[1024];
  int len = 0;

  if((len = enif_get_string(env, argv[0], fmt, sizeof(fmt), ERL_NIF_LATIN1)) <= 0) return enif_make_badarg(env);
  if(!enif_is_list(env, argv[1])) return enif_make_badarg(env);
  
  ERL_NIF_TERM items = argv[1];

  // At first, allocate result as empty binary
  ErlNifBinary result;
  enif_alloc_binary(0, &result);

  // Get escape sequences one-by-one from fmt with according values from items
  char *cur_fmt = fmt;
  int format_result;
  while ((format_result = format_first(env, &result, &cur_fmt, &items)) > 0) {
    // Nothing to do here, everything is ok
  }

  // good: return resulting binary
  if (format_result >= 0) return enif_make_binary(env, &result);

  // bad: make badarg. TODO: return something pointing to erroneous place
  return enif_make_badarg(env);
};


// Helper marcos for formatting numeric types
#define fetch_asprintf(fetcher) \
  switch (consumed) {                 \
    case 1:                                                           \
      asprintf(&formatted, fmt_start, fetcher(env, items));           \
      break;                                                          \
    case 2:                                                           \
      e1 = fetch_int(env, items);                                     \
      asprintf(&formatted, fmt_start, e1, fetcher(env, items));       \
      break;                                                          \
    case 3:                                                           \
      e1 = fetch_int(env, items);                                     \
      e2 = fetch_int(env, items);                                     \
      asprintf(&formatted, fmt_start, e1, e2, fetcher(env, items));   \
      break;                                                          \
  };                                                                  \
  break;

int format_first(ErlNifEnv* env, ErlNifBinary* result, char** fmt_start_ptr, ERL_NIF_TERM* items) {
  char* fmt_start = *fmt_start_ptr;
  char* fmt_iter = fmt_start;
  int consumed = 0;
  int fmt_long = 0;
  int arg_type = ARG_NONE;

  // started at the end of line
  if (*fmt_iter == 0) return 0;

  // find first covnertion spec character or string end
  while (*fmt_iter != 0) {
    if (*fmt_iter == '%') {
      // Ignore "%%" as it does not take any arguments
      if (*(fmt_iter+1) == '%')
        fmt_iter += 2;
      else {
        arg_type = -1;
        break;
      };
    }
    else fmt_iter += 1;
  }

  // find convertion specifier
  while (*fmt_iter != 0){
    if (index("dic", *fmt_iter) != NULL) {
      arg_type = fmt_long?ARG_LONG:ARG_INT;
      consumed++;
      fmt_iter++;
      break;
    };
    if (index("ouxX", *fmt_iter) != NULL) {
      arg_type = fmt_long?ARG_ULONG:ARG_UINT;
      consumed++;
      fmt_iter++;
      break;
    };
    if (index("eEfFgGaA", *fmt_iter) != NULL) {
      arg_type = ARG_DOUBLE;
      consumed++;
      fmt_iter++;
      break;
    };
    if (*fmt_iter == 's') {
      arg_type = ARG_STRING;
      consumed++;
      fmt_iter++;
      break;
    };

    // Extra argument is consumed from input
    if (*fmt_iter == '*') consumed++;

    // We don't support references to argument positions -> return error
    if (*fmt_iter == '$') return -1;

    // Support long
    if (*fmt_iter == 'l') fmt_long = 1;

    // At last, go to next character
    fmt_iter++;
  };

  // convertion spec opened but not closed
  if (arg_type < 0) return -1;
  // Too much stars in spec
  if (consumed > 3) return -1;

  // To not copy part of format, just set zero where scan has ended, then return original value back
  char kept_fmt_byte = *fmt_iter;
  *fmt_iter = 0;

  // We don't really know how much memory will be used, so we call asprintf to let it allocate as much as it needs
  // Do not forget to call free() in the end!
  char* formatted = NULL;
  char* stringbuf;

  int e1, e2;

  switch (arg_type) {
    case ARG_NONE:
      asprintf(&formatted, fmt_start);
      break;
    // Numeric types: see macros above
    case ARG_INT:    fetch_asprintf(fetch_int);
    case ARG_LONG:   fetch_asprintf(fetch_long);
    case ARG_UINT:   fetch_asprintf(fetch_uint);
    case ARG_ULONG:  fetch_asprintf(fetch_ulong);
    case ARG_DOUBLE: fetch_asprintf(fetch_double);

    case ARG_STRING: // This cannot be handled by that simple macros because of allocation inside fetch_string
      switch (consumed) {
        case 1:
          asprintf(&formatted, fmt_start, stringbuf = fetch_string(env, items));
          free(stringbuf);
          break;
        case 2:
          e1 = fetch_int(env, items);
          asprintf(&formatted, fmt_start, e1, stringbuf = fetch_string(env, items));
          free(stringbuf);
          break;
        case 3:
          e1 = fetch_int(env, items);
          e2 = fetch_int(env, items);
          asprintf(&formatted, fmt_start, e1, e2, stringbuf = fetch_string(env, items));
          free(stringbuf);
          break;
      };
      break;
  };

  // Put back saved byte and move format pointer
  *fmt_iter = kept_fmt_byte;
  *fmt_start_ptr = fmt_iter;

  // asprintf did not run or returned error
  if (formatted == NULL || formatted < 0) return -2;

  // remember where we start copying formatted part
  int oldsize = result->size;

  // Here we are sure formatted contains zero-terminated string, so strlen is safe
  int part_length = strlen(formatted);
  enif_realloc_binary(result, oldsize + part_length);

  // Do copy from formatted to result buffer
  char* copy_dest = (char*)result->data + oldsize;
  memcpy(copy_dest, formatted, part_length);

  // free memory allocated by asprintf
  free(formatted);

  // Return number of elements read from items
  return consumed;
};


int
fetch_int(ErlNifEnv* env, ERL_NIF_TERM* items) {
  ERL_NIF_TERM head;
  // Fetch head if possible
  if (! enif_get_list_cell(env, *items, &head, items)) return 0;

  int i;
  double d;

  if(enif_get_int(env, head, &i)) return i;
  if(enif_get_double(env, head, &d)) return (int) d;

  return 0;
};

unsigned int
fetch_uint(ErlNifEnv* env, ERL_NIF_TERM* items) {
  ERL_NIF_TERM head;
  // Fetch head if possible
  if (! enif_get_list_cell(env, *items, &head, items)) return 0;

  unsigned int i;
  double d;

  if(enif_get_uint(env, head, &i)) return i;
  if(enif_get_double(env, head, &d)) return (unsigned int) abs(d);

  return 0;
};

long
fetch_long(ErlNifEnv* env, ERL_NIF_TERM* items) {
  ERL_NIF_TERM head;
  // Fetch head if possible
  if (! enif_get_list_cell(env, *items, &head, items)) return 0;

  long i;
  double d;

  if(enif_get_long(env, head, &i)) return i;
  if(enif_get_double(env, head, &d)) return (long) d;

  return 0;
};

unsigned long
fetch_ulong(ErlNifEnv* env, ERL_NIF_TERM* items) {
  ERL_NIF_TERM head;
  // Fetch head if possible
  if (! enif_get_list_cell(env, *items, &head, items)) return 0;

  unsigned long i;
  double d;

  if(enif_get_ulong(env, head, &i)) return i;
  if(enif_get_double(env, head, &d)) return (unsigned long) abs(d);

  return 0;
};

double
fetch_double(ErlNifEnv* env, ERL_NIF_TERM* items) {
  ERL_NIF_TERM head;
  // Fetch head if possible
  if (! enif_get_list_cell(env, *items, &head, items)) return 0.0;

  long i;
  double d;

  if(enif_get_double(env, head, &d)) return d;
  if(enif_get_long(env, head, &i)) return (double) i;

  return 0.0;
};

char*
fetch_string(ErlNifEnv* env, ERL_NIF_TERM* items) {
  ERL_NIF_TERM head;
  char* buffer;

  // Fetch head if possible
  if (! enif_get_list_cell(env, *items, &head, items)) {
    buffer = (char*) malloc(1);
    buffer[0] = 0;
    return buffer;
  }

  unsigned int bodylen = 0;

  // Try to fetch atom
  if (enif_get_atom_length(env, head, &bodylen, ERL_NIF_LATIN1)) {
    buffer = (char*) malloc(bodylen + 1);
    buffer[0] = 0;
    enif_get_atom(env, head, buffer, bodylen + 1, ERL_NIF_LATIN1);
    return buffer;
  }

  // If it is not atom, try to fetch iolist. Strings, deep lists, binaries should fit well here
  ErlNifBinary bin;
  if (enif_inspect_iolist_as_binary(env, head, &bin)) {
    bodylen = bin.size;
    buffer = (char*) malloc(bodylen + 1);
    memcpy(buffer, bin.data, bodylen);
    buffer[bodylen] = 0;
    return buffer;
  }

  // If we get here there is no string-like term in the head, so just allocate and return "\0"
  buffer = (char*) malloc(1);
  buffer[0] = 0;
  return buffer;
};


// dummy upgrade
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
  return 0;
};


// Erlang to C function mapping
static ErlNifFunc io_libc_funcs[] = {
  {"fwrite", 2, io_libc_fwrite}
};

ERL_NIF_INIT(io_libc, io_libc_funcs, NULL, NULL, upgrade, NULL)
