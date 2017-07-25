/* crc32_stubs.c -- compute the CRC-32 of a data stream
   Copyright (C) 1995-2013 Jean-loup Gailly and Mark Adler

   This software is provided 'as-is', without any express or implied
   warranty.  In no event will the authors be held liable for any damages
   arising from the use of this software.

   Permission is granted to anyone to use this software for any purpose,
   including commercial applications, and to alter it and redistribute it
   freely, subject to the following restrictions:

   1. The origin of this software must not be misrepresented; you must not
      claim that you wrote the original software. If you use this software
      in a product, an acknowledgment in the product documentation would be
      appreciated but is not required.
   2. Altered source versions must be plainly marked as such, and must not be
      misrepresented as being the original software.
   3. This notice may not be removed or altered from any source distribution.

   Jean-loup Gailly        Mark Adler
   jloup@gzip.org          madler@alumni.caltech.edu


   The data format used by the zlib library is described by RFCs (Request for
   Comments) 1950 to 1952 in the files http://tools.ietf.org/html/rfc1950
   (zlib format), rfc1951 (deflate format) and rfc1952 (gzip format).

   - Rodney Brown <rbrown64@csc.com.ay>: faster CRC methods
   - Romain Calascibetta <romain.calascibetta@gmail.com>: binding with OCaml
*/

#include <limits.h>
#include <stddef.h>

#if (UINT_MAX == 0xFFFFFFFFUL)
#  define U4 unsigned
#elif (ULONG_MAX == 0xFFFFFFFFUL)
#  define U4 unsigned long
#elif (USHRT_MAX == 0xFFFFFFFFUL)
#  define U4 unsigned short
#endif

typedef U4 crc_t;

#define SWAP32(q) ((((q) >> 24) & 0xff) + (((q) >> 8) & 0xff00) + \
                   (((q) & 0xff00) << 8) + (((q) & 0xff) << 24))

#if !defined(NOBYFOUR) && defined(U4)
#  define BYFOUR
#endif
#ifdef BYFOUR
   static unsigned long crc32_le(unsigned long, const unsigned char *, size_t);
   static unsigned long crc32_be(unsigned long, const unsigned char *, size_t);

#  define TBLS 8
#else
#  define TBLS 1
#endif

#include "crc32_stubs.h"

#define DO1 crc = crc_table[0][((int)crc ^ (*buf++)) & 0xFF] ^ (crc >> 8)
#define DO8 DO1; DO1; DO1; DO1; DO1; DO1; DO1; DO1

long
crc32(crc, buf, len)
     unsigned long crc;
     const unsigned char *buf;
     size_t len;
{
  if (buf == NULL)
    return 0UL;

#ifdef BYFOUR
  if (sizeof(void *) == sizeof(long))
    {
      crc_t endian;

      endian = 1;

      if (*(unsigned char *) (&endian))
        return crc32_le(crc, buf, len);
      else
        return crc32_be(crc, buf, len);
    }
#endif

  crc = crc ^ 0xFFFFFFFFUL;

  while (len >= 8) {
    DO8;
    len -= 8;
  }

  if (len)
    do {
      DO1;
    } while (--len);

  return crc ^ 0xFFFFFFFFUL;
}

#ifdef BYFOUR

#define DOLIT4 c ^= *buf4++; \
  c = crc_table[3][c & 0xFF] ^ crc_table[2][(c >> 8) & 0xFF] ^ \
    crc_table[1][(c >> 16) & 0xFF] ^ crc_table[0][c >> 24]
#define DOLIT32 DOLIT4; DOLIT4; DOLIT4; DOLIT4; DOLIT4; DOLIT4; DOLIT4; DOLIT4

static unsigned long crc32_le(crc, buf, len)
     unsigned long crc;
     const unsigned char *buf;
     size_t len;
{
  register crc_t c;
  register const crc_t *buf4;

  c = (crc_t) crc;
  c = ~c;

  while (len && ((long) buf & 3)) {
    c = crc_table[0][(c ^ *buf++) & 0xFF] ^ (c >> 8);
    len--;
  }

  buf4 = (const crc_t *)(const void *) buf;

  while (len >= 32) {
    DOLIT32;
    len -= 32;
  }

  while (len >= 4) {
    DOLIT4;
    len -= 4;
  }

  buf = (const unsigned char *) buf4;

  if (len)
    do {
      c = crc_table[0][(c ^ *buf++) & 0xFF] ^ (c >> 8);
    } while (--len);

  c = ~c;

  return (unsigned long) c;
}

#define DOBIG4 c ^= *++buf4; \
  c = crc_table[4][c & 0xFF] ^ crc_table[5][(c >> 8) & 0xFF] ^ \
    crc_table[6][(c >> 16) & 0xFF] ^ crc_table[7][c >> 24]
#define DOBIG32 DOBIG4; DOBIG4; DOBIG4; DOBIG4; DOBIG4; DOBIG4; DOBIG4; DOBIG4

static unsigned long crc32_be(crc, buf, len)
     unsigned long crc;
     const unsigned char *buf;
     size_t len;
{
  register crc_t c;
  register const crc_t *buf4;

  c = SWAP32((crc_t) crc);
  c = ~c;

  while (len && ((long) buf & 3)) {
    c = crc_table[4][(c >> 24) ^ *buf++] ^ (c << 8);
    len--;
  }

  buf4 = (const crc_t *)(const void *) buf;
  buf4--;

  while (len >= 32) {
    DOBIG32;
    len -= 32;
  }
  while (len >= 4) {
    DOBIG4;
    len -= 4;
  }
  buf4++;
  buf = (const unsigned char *) buf4;

  if (len)
    do {
      c = crc_table[4][(c >> 24) ^ *buf++] ^ (c << 8);
    } while (--len);

  c = ~c;

  return (unsigned long)(SWAP32(c));
}

#endif /* BYFOUR */

#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/alloc.h>

CAMLprim value
caml_st_crc32(value val_crc32, value val_buf, value val_off, value val_len)
{
  unsigned long res;

  res = crc32(Int32_val (val_crc32), String_val (val_buf) + Long_val (val_off), Long_val (val_len));

  return caml_copy_int32(res);
}

CAMLprim value
caml_ba_crc32(value val_crc32, value val_buf, value val_off, value val_len)
{
  unsigned long res;

  res = crc32(Int32_val (val_crc32), Caml_ba_data_val (val_buf) + Long_val (val_off), Long_val (val_len));

  return caml_copy_int32(res);
}
