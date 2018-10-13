## vim: filetype=makocpp

#ifndef QUEX_INTERFACE_H
#define QUEX_INTERFACE_H

#include <stdint.h>

#include "${ada_lib_name.lower()}_lexer.h"


struct token {
    /* Kind for this token (identifier, keyword, ...). */
    uint16_t id;

    /* For token kinds that don't keep text, NULL.  Pointer to an
       UTF-32-encoded string otherwise (native endianity). */
    const uint32_t *text;

    /* Size (in 32-bit words) for text. */
    size_t text_length;

    /* Code point offset from the beginning of the source buffer for this
       token.  */
    uint32_t offset;
};

typedef struct Lexer Lexer;

uint16_t
${capi.get_name('lexer_prev_token')}(QUEX_TYPE_ANALYZER *quex_lexer);

Lexer*
${capi.get_name('lexer_from_buffer')}(uint32_t *buffer, size_t length);

void
${capi.get_name('free_lexer')}(Lexer* lexer);

int
${capi.get_name('next_token')}(Lexer* lexer, struct token* tok);

#endif
