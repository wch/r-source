/* Define to 1 if crc32 integrity check is enabled. */
#define HAVE_CHECK_CRC32 1

/* Define to 1 if crc64 integrity check is enabled. */
#define HAVE_CHECK_CRC64 1

/* Define to 1 if sha256 integrity check is enabled. */
#define HAVE_CHECK_SHA256 1

/* Define to 1 if decoder components are enabled. */
#define HAVE_DECODER 1

/* Define to 1 if arm decoder is enabled. */
#define HAVE_DECODER_ARM 1

/* Define to 1 if armthumb decoder is enabled. */
#define HAVE_DECODER_ARMTHUMB 1

/* Define to 1 if delta decoder is enabled. */
#define HAVE_DECODER_DELTA 1

/* Define to 1 if ia64 decoder is enabled. */
#define HAVE_DECODER_IA64 1

/* Define to 1 if lzma1 decoder is enabled. */
#define HAVE_DECODER_LZMA1 1

/* Define to 1 if lzma2 decoder is enabled. */
#define HAVE_DECODER_LZMA2 1

/* Define to 1 if powerpc decoder is enabled. */
#define HAVE_DECODER_POWERPC 1

/* Define to 1 if sparc decoder is enabled. */
#define HAVE_DECODER_SPARC 1

/* Define to 1 if subblock decoder is enabled. */
#define HAVE_DECODER_SUBBLOCK 1

/* Define to 1 if x86 decoder is enabled. */
#define HAVE_DECODER_X86 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if encoder components are enabled. */
#define HAVE_ENCODER 1

/* Define to 1 if arm encoder is enabled. */
#define HAVE_ENCODER_ARM 1

/* Define to 1 if armthumb encoder is enabled. */
#define HAVE_ENCODER_ARMTHUMB 1

/* Define to 1 if delta encoder is enabled. */
#define HAVE_ENCODER_DELTA 1

/* Define to 1 if ia64 encoder is enabled. */
#define HAVE_ENCODER_IA64 1

/* Define to 1 if lzma1 encoder is enabled. */
#define HAVE_ENCODER_LZMA1 1

/* Define to 1 if lzma2 encoder is enabled. */
#define HAVE_ENCODER_LZMA2 1

/* Define to 1 if powerpc encoder is enabled. */
#define HAVE_ENCODER_POWERPC 1

/* Define to 1 if sparc encoder is enabled. */
#define HAVE_ENCODER_SPARC 1

/* Define to 1 if subblock encoder is enabled. */
/* #undef HAVE_ENCODER_SUBBLOCK */

/* Define to 1 if x86 encoder is enabled. */
#define HAVE_ENCODER_X86 1

/* Define to 1 to enable bt2 match finder. */
#define HAVE_MF_BT2 1

/* Define to 1 to enable bt3 match finder. */
#define HAVE_MF_BT3 1

/* Define to 1 to enable bt4 match finder. */
#define HAVE_MF_BT4 1

/* Define to 1 to enable hc3 match finder. */
#define HAVE_MF_HC3 1

/* Define to 1 to enable hc4 match finder. */
#define HAVE_MF_HC4 1

/* Things that might be defined on a per-platform basis: */

/* Define to 1 if using x86 assembler optimizations. */
/* #define HAVE_ASM_X86 1 */

/* Define to 1 if using x86_64 assembler optimizations. */
/* #undef HAVE_ASM_X86_64 */

/* Define to 1 if bswap_16 is available. */
/* #undef HAVE_BSWAP_16 */

/* Define to 1 if bswap_32 is available. */
/* #undef HAVE_BSWAP_32 */

/* Define to 1 if bswap_64 is available. */
/* #undef HAVE_BSWAP_64 */

/* Define to 1 if you have the <byteswap.h> header file. */
/* #undef HAVE_BYTESWAP_H */

/* Define to 1 if the system supports fast unaligned memory access. */
/* #define HAVE_FAST_UNALIGNED_ACCESS 1 */

/* Define to 1 if the system has the type `_Bool'. */
/* #define HAVE__BOOL 1 */

#ifdef WIN32
#define HAVE_ASM_X86 1
#define HAVE_FAST_UNALIGNED_ACCESS 1
#endif
