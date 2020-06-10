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

/* Code fragments, indexed by start PC address */
static struct skiplist code_fragments_by_pc = SKIPLIST_STATIC_INITIALIZER;

/* Code fragments, indexed by fragment number */
static struct skiplist code_fragments_by_num = SKIPLIST_STATIC_INITIALIZER;

static int code_fragments_counter = 0;

/* Code fragments, indexed by the top bits of the digest */
static struct skiplist code_fragments_by_digest = SKIPLIST_STATIC_INITIALIZER;

/* Code fragments whose digest is to be computed lazily (DIGEST_LATER) */
static struct code_fragment * code_fragments_to_digest = NULL;

Caml_inline uintnat key_of_digest(unsigned char * digest)
{
  uintnat key;
  memcpy(&key, digest, sizeof(uintnat));
  return key;
}

static void insert_by_digest(struct code_fragment * cf)
{
  uintnat key, data;
  
  CAMLassert (cf->digest_status == DIGEST_PROVIDED);
  key = key_of_digest(cf->digest);
  if (caml_skiplist_find(&code_fragments_by_digest, key, &data)) {
    /* Collision!  Chain via the "next" field. */
    cf->next = (struct code_fragment *) data;
  } else {
    cf->next = NULL;
  }
  caml_skiplist_insert(&code_fragments_by_digest, key, (uintnat) cf);
}

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
  switch (digest_kind) {
  case DIGEST_NOW: case DIGEST_PROVIDED:
    /* Insert in code_fragments_by_digest immediately */
    insert_by_digest(cf);
    break;
  case DIGEST_LATER:
    /* Queue the fragment for insertion in code_fragments_by_digest
       the next time caml_find_code_fragment_by_digest is called. */
    cf->next = code_fragments_to_digest;
    code_fragments_to_digest = cf;
  case DIGEST_IGNORE:
    /* Don't insert in code_fragments_by_digest ever */
    break;
  }
  return cf->fragnum;
}

static void remove_from_list(struct code_fragment ** list,
                             struct code_fragment * cf)
{
  while (*list != cf) {
    CAMLassert(*list != NULL);
    list = &((*list)->next);
  }
  *list = cf->next;
  cf->next = NULL;
}

void caml_remove_code_fragment(struct code_fragment * cf)
{
  CAMLunused_start int ret CAMLunused_end;
  uintnat key, data;
  struct code_fragment * list;

  caml_skiplist_remove(&code_fragments_by_pc, (uintnat) cf->code_start);
  caml_skiplist_remove(&code_fragments_by_num, cf->fragnum);
  switch (cf->digest_status) {
  case DIGEST_NOW: case DIGEST_PROVIDED:
    key = key_of_digest(cf->digest);
    ret = caml_skiplist_find(&code_fragments_by_digest, key, &data);
    CAMLassert(ret);
    list = (struct code_fragment *) data;
    /* Remove the code fragment from the list of collisions */
    remove_from_list(&list, cf);
    /* Remove from skiplist or update skiplist entry */
    if (list == NULL) {
      caml_skiplist_remove(&code_fragments_by_digest, key);
    } else {
      caml_skiplist_insert(&code_fragments_by_digest, key, (uintnat) list);
    }
    break;
  case DIGEST_LATER:
    /* Remove the code fragment from the "to digest" list */
    remove_from_list(&code_fragments_to_digest, cf);
    break;
  case DIGEST_IGNORE:
    break;
  }
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

struct code_fragment *
   caml_find_code_fragment_by_digest(unsigned char digest[16])
{
  struct code_fragment * cf;
  uintnat key, data;
  unsigned char * d;

  /* Look the digest up in code_fragments_by_digest */
  key = key_of_digest(digest);
  if (caml_skiplist_find(&code_fragments_by_digest, key, &data)) {
    for (cf = (struct code_fragment *) data; cf != NULL; cf = cf->next) {
      CAMLassert(cf->digest_status == DIGEST_NOW ||
                 cf->digest_status == DIGEST_PROVIDED);
      if (memcmp(digest, cf->digest, 16) == 0) return cf;
    }
  }
  /* Compute the digests of DIGEST_LATER fragments and insert them into
     the code_fragments_by_digest table.  In parallel, search for
     the required digest. */
  while (code_fragments_to_digest != NULL) {
    cf = code_fragments_to_digest;
    code_fragments_to_digest = cf->next;
    d = caml_digest_of_code_fragment(cf);
    CAMLassert (d != NULL);
    insert_by_digest(cf);
    /* Stop now if we find the digest we're looking for. */
    if (memcmp(digest, d, 16) == 0) return cf;
  }
  return NULL;
}
