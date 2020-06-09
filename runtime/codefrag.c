/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cambium, INRIA Paris                  */
/*                                                                        */
/*   Copyright 2020 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* A table of all code fragments (main program and dynlinked modules) */

#include <string.h>
#include <stddef.h>
#include "caml/codefrag.h"
#include "caml/misc.h"
#include "caml/md5.h"
#include "caml/memory.h"
#include "caml/skiplist.h"

static struct skiplist code_fragments_by_pc = SKIPLIST_STATIC_INITIALIZER;

static struct skiplist code_fragments_by_num = SKIPLIST_STATIC_INITIALIZER;

static int code_fragments_counter = 0;

static struct skiplist code_fragments_by_digest = SKIPLIST_STATIC_INITIALIZER;

static struct code_fragment * code_fragments_recently_added = NULL;

int caml_register_code_fragment(char * start, char * end,
                                enum digest_status digest_kind,
                                unsigned char * opt_digest)
{
  struct code_fragment * cf = caml_stat_alloc(sizeof(struct code_fragment));

  cf->code_start = start;
  cf->code_end = end;
  switch (digest_kind) {
  case DIGEST_LATER:
    break;
  case DIGEST_NOW:
    caml_md5_block(cf->digest, cf->code_start, cf->code_end - cf->code_start);
    digest_kind = DIGEST_PROVIDED;
    break;
  case DIGEST_PROVIDED:
    memcpy(cf->digest, opt_digest, 16);
    break;
  case DIGEST_IGNORE:
    break;
  }
  cf->digest_status = digest_kind;
  cf->fragnum = code_fragments_counter++;
  caml_skiplist_insert(&code_fragments_by_pc,
                       (uintnat) start, (uintnat) cf);
  caml_skiplist_insert(&code_fragments_by_num,
                       cf->fragnum, (uintnat) cf);
  if (digest_kind != DIGEST_IGNORE) {
    /* Queue the fragment for insertion in code_fragments_by_digest
       the next time caml_find_code_fragment_by_digest is called. */
    cf->next = code_fragments_recently_added;
    code_fragments_recently_added = cf;
  }
  return cf->fragnum;
}

void caml_remove_code_fragment(struct code_fragment * cf)
{
  caml_skiplist_remove(&code_fragments_by_pc, (uintnat) cf->code_start);
  caml_skiplist_remove(&code_fragments_by_num, cf->fragnum);
  caml_stat_free(cf);
}

struct code_fragment * caml_find_code_fragment_by_pc(char *pc)
{
  struct code_fragment * cf;
  uintnat key, data;

  if (caml_skiplist_find_below(&code_fragments_by_pc,
                               (uintnat) pc, &key, &data)) {
    cf = (struct code_fragment *) data;
    if (cf->code_start <= pc && pc < cf->code_end) return cf;
  }
  return NULL;
}

struct code_fragment * caml_find_code_fragment_by_num(int fragnum)
{
  uintnat data;
  if (caml_skiplist_find(&code_fragments_by_num, fragnum, &data)) {
    return (struct code_fragment *) data;
  } else {
    return NULL;
  }
}

unsigned char * caml_digest_of_code_fragment(struct code_fragment * cf)
{
  if (cf->digest_status == DIGEST_IGNORE)
    return NULL;
  if (cf->digest_status == DIGEST_LATER) {
    caml_md5_block(cf->digest, cf->code_start, cf->code_end - cf->code_start);
    cf->digest_status = DIGEST_PROVIDED;
  }
  return cf->digest;
}

Caml_inline uintnat key_of_digest(unsigned char * digest)
{
  uintnat key;
  memcpy(&key, digest, sizeof(uintnat));
  return key;
}

struct code_fragment *
   caml_find_code_fragment_by_digest(unsigned char digest[16])
{
  struct code_fragment * cf;
  uintnat key, data;
  unsigned char * d;

  /* Insert recently-added fragments into the code_fragments_by_digest
     table, indexed by the top bits of their digest. */
  while (code_fragments_recently_added != NULL) {
    cf = code_fragments_recently_added;
    code_fragments_recently_added = cf->next;
    d = caml_digest_of_code_fragment(cf);
    CAMLassert (d != NULL);
    key = key_of_digest(d);
    if (caml_skiplist_find(&code_fragments_by_digest, key, &data)) {
      /* Collision!  Chain via the "next" field. */
      cf->next = (struct code_fragment *) data;
    } else {
      cf->next = NULL;
    }
    caml_skiplist_insert(&code_fragments_by_digest, key, (uintnat) cf);
    /* Stop early if we find the digest we're looking for. */
    if (memcmp(digest, d, 16) == 0) return cf;
  }
  /* Look the digest up in code_fragments_by_digest */
  key = key_of_digest(digest);
  if (! caml_skiplist_find(&code_fragments_by_digest, key, &data))
    return NULL;
  for (cf = (struct code_fragment *) data; cf != NULL; cf = cf->next) {
    CAMLassert(cf->digest_status == DIGEST_NOW ||
               cf->digest_status == DIGEST_PROVIDED);
    if (memcmp(digest, cf->digest, 16) == 0) return cf;
  }
  return NULL;
}
