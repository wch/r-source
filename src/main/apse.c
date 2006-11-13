/*

Copyright (C) Jarkko Hietaniemi, 1998,1999,2000,2001. All Rights Reserved.

This program is free software; you can redistribute it and/or modify
it under the terms of either:

a) the GNU Library General Public License as published by the Free
   Software Foundation; either version 2, or (at your option) any
   later version, or

b) the "Artistic License" which comes with Perl source code.

Other free software licensing schemes are negotiable.

Furthermore:

(1) This software is provided as-is, without warranties or
    obligations of any kind.

(2) You shall include this copyright notice intact in all copies
    and derived materials.

*/

/* <UTF8-FIXME> byte-level ops. Not at all clear that this is fixable. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_hidden
#endif



#include "apse.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#define APSE_BITS_IN_BITVEC	(8*sizeof(apse_vec_t))

#define APSE_CHAR_MAX		256

#ifdef APSE_DEBUGGING
#define APSE_DEBUG(x) x
#else
#define APSE_DEBUG(x)
#endif

#define APSE_BIT(i)		((apse_vec_t)1 << ((i)%APSE_BITS_IN_BITVEC))
#define APSE_IDX(p, q, i)	((p)*(q)+(i)/APSE_BITS_IN_BITVEC)
#define APSE_BIT_SET(bv, p, q, i) (bv[APSE_IDX(p, q, i)] |=  APSE_BIT(i))
#define APSE_BIT_CLR(bv, p, q, i) (bv[APSE_IDX(p, q, i)] &= ~APSE_BIT(i))
#define APSE_BIT_TST(bv, p, q, i) (bv[APSE_IDX(p, q, i)] &   APSE_BIT(i))

#define APSE_MATCH_STATE_BOT		0
#define APSE_MATCH_STATE_SEARCH		1
#define APSE_MATCH_STATE_BEGIN		2
#define APSE_MATCH_STATE_FAIL		3
#define APSE_MATCH_STATE_GREEDY		4
#define APSE_MATCH_STATE_END		5
#define APSE_MATCH_STATE_EOT		6

#define APSE_TEST_HIGH_BIT(i)	\
	(((i) & ((apse_vec_t)1 << (APSE_BITS_IN_BITVEC - 1))) ? 1 : 0)

/* In case you are reading the TR 91-11 of University of Arizona, page 6:
 * j+1	state
 * j	prev_state
 * d	i
 * d-1	prev_i
 */

#define APSE_NEXT_EXACT(state, prev_state, text, i, carry)		\
	(state[i] = ((prev_state[i] << 1 | carry) & text))

#define APSE_NEXT_APPROX(state, prev_state, text, i, prev_i, carry)	\
	(state[i]  = (((prev_state[i] << 1) & text)		  |	\
			prev_state[prev_i] 			  |	\
		      ((state[prev_i] | prev_state[prev_i]) << 1) |	\
			carry))

#define APSE_NEXT_COMMON(state, prev_state, text, i)			\
	(state[i]  = (prev_state[i] << 1) & text)

#define APSE_NEXT_INSERT(state, prev_state, i, prev_i)			\
	(state[i] |= prev_state[prev_i])

#define APSE_NEXT_DELETE(state, i, prev_i)				\
	(state[i] |= (state[prev_i] << 1))

#define APSE_NEXT_SUBSTI(state, prev_state, i, prev_i)			\
	(state[i] |= (prev_state[prev_i] << 1))

#define APSE_NEXT_CARRY(state, i, carry)				\
	(state[i] |= carry)

#define APSE_EXACT_MATCH_BEGIN(ap)	(ap->state[0] & 1)

#define APSE_APPROX_MATCH_BEGIN(ap)	\
	(ap->state[ap->largest_distance + ap->match_begin_bitvector] > \
	 ap->match_begin_prefix && \
	 ap->state[ap->largest_distance + ap->match_begin_bitvector] & \
	 ap->match_begin_prefix)

#define APSE_PREFIX_DELETE_MASK(ap)	\
	    do { if (ap->edit_deletions < ap->edit_distance &&	\
		     ap->text_position  < ap->edit_distance)	\
 	             ap->state[h] &= ap->match_begin_bitmask; } while (0)

#define APSE_DEBUG_SINGLE(ap, i)		\
	APSE_DEBUG(printf("%c %2ld %2ld %s\n",				\
			  isprint(ap->text[ap->text_position])?	\
			  ap->text[ap->text_position]:'.',		\
			  ap->text_position, i, 			\
			  _apse_fbin(ap->state[i],			\
				     ap->pattern_size, 1)))


#define APSE_DEBUG_MULTIPLE_FIRST(ap, i)	\
	APSE_DEBUG(printf("%c %2ld %2ld",				\
			  isprint(ap->text[ap->text_position])?	\
			  ap->text[ap->text_position]:'.',		\
			  ap->text_position, i))

#define APSE_DEBUG_MULTIPLE_REST(ap, i, j)	\
	APSE_DEBUG(printf(" %s",					\
			  _apse_fbin(ap->state[j],			\
				     ap->pattern_size, 			\
				     i == ap->bitvectors_in_state-1)))


#ifdef APSE_DEBUGGING
static char *_apse_fbin(apse_vec_t v, apse_size_t n, apse_bool_t last);

static char *_apse_fbin(apse_vec_t v, apse_size_t n, apse_bool_t last) {
    static char s[APSE_BITS_IN_BITVEC + 1] = { 0 }; /* non-reentrant */

    if (v) {
	static const char *b =
	    "0000100001001100001010100110111000011001010111010011101101111111";
	apse_size_t i;

	for (i = 0; i < APSE_BITS_IN_BITVEC && i < n && v; i += 4) {
	    (void)memcpy(s + i, b + ((v & 0x0f) << 2), (size_t)4);
	    v >>= 4;
	}

	if (i < APSE_BITS_IN_BITVEC)
	    memset(s + i, '0', APSE_BITS_IN_BITVEC - i);
    } else
	memset(s, '0', APSE_BITS_IN_BITVEC);

    if (last)
	s[n % APSE_BITS_IN_BITVEC] = 0;

    return s;
}
#endif

/* The code begins. */

static
apse_bool_t apse_set_pattern(apse_t*		ap,
			     unsigned char*	pattern,
			     apse_size_t	pattern_size) {
    apse_size_t	i;

    if (ap->case_mask)
	free(ap->case_mask);
    if (ap->fold_mask)
	free(ap->fold_mask);

    ap->pattern_mask = 0;
    ap->fold_mask    = 0;
    ap->case_mask    = 0;

    ap->is_greedy    = 0;

    ap->prev_equal   = 0;
    ap->prev_active  = 0;

    ap->pattern_size = pattern_size;
    ap->bitvectors_in_state = (pattern_size - 1)/APSE_BITS_IN_BITVEC + 1;

    if (ap->edit_distance)
	ap->largest_distance = ap->edit_distance * ap->bitvectors_in_state;

    ap->bytes_in_state = ap->bitvectors_in_state * sizeof(apse_vec_t);

    ap->case_mask = (apse_vec_t *) calloc((apse_size_t)APSE_CHAR_MAX, ap->bytes_in_state);
    if (!ap->case_mask)
	goto out;

    for (i = 0; i < pattern_size; i++)
	APSE_BIT_SET(ap->case_mask,
		     (unsigned)pattern[i],
		     ap->bitvectors_in_state, i);

    ap->pattern_mask = ap->case_mask;

    ap->match_end_bitmask =
	(apse_vec_t)1 << ((pattern_size - 1) % APSE_BITS_IN_BITVEC);

out:
    if (ap && ap->case_mask)
	return 1;
    else {
	if (ap->case_mask)
	    free(ap->case_mask);
	if (ap)
	    free(ap);
	return 0;
    }
}

#if 0
void apse_set_greedy(apse_t *ap, apse_bool_t greedy) {
    ap->is_greedy = greedy;
}

apse_bool_t apse_get_greedy(apse_t *ap) {
    return ap->is_greedy;
}

void apse_set_match_bot_callback(apse_t *ap,
				 void* (*match_bot_callback)(apse_t* ap)) {
    ap->match_bot_callback = match_bot_callback;
}

void apse_set_match_begin_callback(apse_t *ap,
				   void* (*match_begin_callback)(apse_t* ap)) {
    ap->match_begin_callback = match_begin_callback;
}

void apse_set_match_fail_callback(apse_t *ap,
				  void* (*match_fail_callback)(apse_t* ap)) {
    ap->match_fail_callback = match_fail_callback;
}

void apse_set_match_end_callback(apse_t *ap,
				 void* (*match_end_callback)(apse_t* ap)) {
    ap->match_end_callback = match_end_callback;
}

void apse_set_match_eot_callback(apse_t *ap,
				 void* (*match_eot_callback)(apse_t* ap)) {
    ap->match_eot_callback = match_eot_callback;
}

void* (*apse_get_match_bot_callback(apse_t * ap))(apse_t *ap) {
    return ap->match_bot_callback;
}

void* (*apse_get_match_begin_callback(apse_t * ap))(apse_t *ap) {
    return ap->match_begin_callback;
}

void* (*apse_get_match_fail_callback(apse_t * ap))(apse_t *ap) {
    return ap->match_fail_callback;
}

void* (*apse_get_match_end_callback(apse_t * ap))(apse_t *ap) {
    return ap->match_end_callback;
}

void* (*apse_get_match_eot_callback(apse_t * ap))(apse_t *ap) {
    return ap->match_eot_callback;
}
#endif

static int _apse_wrap_slice(apse_t*		ap,
			    apse_ssize_t	begin_in,
			    apse_ssize_t	size_in,
			    apse_ssize_t*	begin_out,
			    apse_ssize_t*	size_out) {
    if (begin_in < 0) {
	if (-begin_in > ap->pattern_size)
	    return 0;
	begin_in = ap->pattern_size + begin_in;
    }

    if (size_in < 0) {
	if (-size_in > begin_in)
	    return 0;
	size_in   = -size_in;
	begin_in -=  size_in;
    }

    if (begin_in >= ap->pattern_size)
	return 0;
	
    if (begin_in + size_in > ap->pattern_size)
	size_in = ap->pattern_size - begin_in;

    if (begin_out)
	*begin_out = begin_in;

    if (size_out)
	*size_out = size_in;

    return 1;
}

#if 0
apse_bool_t apse_set_anychar(apse_t *ap, apse_ssize_t pattern_index) {
    apse_size_t	bitvectors_in_state = ap->bitvectors_in_state;
    apse_ssize_t	true_index, i;
    apse_bool_t	okay = 0;

    if (!_apse_wrap_slice(ap, pattern_index, (apse_ssize_t)1,
			      &true_index, 0))
	goto out;

    for (i = 0; i < APSE_CHAR_MAX; i++)
	APSE_BIT_SET(ap->case_mask,
		      i, bitvectors_in_state, pattern_index);
    if (ap->fold_mask)
	for (i = 0; i < APSE_CHAR_MAX; i++)
	    APSE_BIT_SET(ap->fold_mask,
			  i, bitvectors_in_state, pattern_index);

    okay = 1;

  out:
    
    return okay;
}

apse_bool_t apse_set_charset(apse_t*		ap,
			     apse_ssize_t	pattern_index,
			     unsigned char*	set,
			     apse_size_t	set_size,
			     apse_bool_t	complement) {
    apse_size_t	bitvectors_in_state = ap->bitvectors_in_state;
    apse_ssize_t	true_index, i;
    apse_bool_t	okay = 0;

    if (!_apse_wrap_slice(ap, pattern_index, (apse_ssize_t)1,
			      &true_index, 0))
	goto out;
    
    if (complement) {
	for (i = 0; i < set_size; i++)
	    APSE_BIT_CLR(ap->case_mask,
			  (unsigned)set[i],
			  bitvectors_in_state, true_index);
    } else {
	for (i = 0; i < set_size; i++)
	    APSE_BIT_SET(ap->case_mask,
			  (unsigned)set[i],
			  bitvectors_in_state, true_index);
    }
    if (ap->fold_mask)
	apse_set_caseignore_slice(ap, pattern_index,
				   (apse_ssize_t)1, (apse_bool_t)1);

    okay = 1;

 out:

    return okay;
}
#endif

static void _apse_reset_state(apse_t* ap) {
    apse_size_t	i, j;

    (void)memset(ap->state,      0, ap->bytes_in_all_states);
    (void)memset(ap->prev_state, 0, ap->bytes_in_all_states);

    ap->prev_equal  = 0;
    ap->prev_active = 0;

    for (i = 1; i <= ap->edit_distance; i++) {
	for (j = 0; j < i; j++)
	    APSE_BIT_SET(ap->prev_state, i, ap->bitvectors_in_state, j);
    }
}

#if 0
apse_bool_t apse_set_text_position(apse_t *ap,
					 apse_size_t text_position) {
    ap->text_position = text_position;

    return 1;
}

apse_size_t apse_get_text_position(apse_t *ap) {
    return ap->text_position;
}

apse_bool_t apse_set_text_initial_position(apse_t *ap,
					   apse_size_t text_initial_position) {
    ap->text_initial_position = text_initial_position;

    return 1;
}

apse_size_t apse_get_text_initial_position(apse_t *ap) {
    return ap->text_initial_position;
}

apse_bool_t apse_set_text_final_position(apse_t *ap,
					 apse_size_t text_final_position) {
    ap->text_final_position = text_final_position;

    return 1;
}

apse_size_t apse_get_text_final_position(apse_t *ap) {
    return ap->text_final_position;
}

apse_bool_t apse_set_text_position_range(apse_t *ap,
					 apse_size_t text_position_range) {
    ap->text_position_range = text_position_range;

    return 1;
}

apse_size_t apse_get_text_position_range(apse_t *ap) {
    return ap->text_position_range;
}
#endif

static
void apse_reset(apse_t *ap) {
    _apse_reset_state(ap);

    ap->text_position = ap->text_initial_position;
    ap->text_position_range = APSE_MATCH_BAD;

    ap->match_state = APSE_MATCH_STATE_BOT;
    ap->match_begin = APSE_MATCH_BAD;
    ap->match_end   = APSE_MATCH_BAD;
}

static
apse_bool_t apse_set_edit_distance(apse_t *ap, apse_size_t edit_distance) {
    /* TODO: waste not--reuse if possible */

    if (ap->state)
	free(ap->state);
    if (ap->prev_state)
	free(ap->prev_state);

    ap->edit_distance = edit_distance;

    ap->bytes_in_all_states = (edit_distance + 1) * ap->bytes_in_state;

    ap->state = ap->prev_state = 0;

    ap->state = (apse_vec_t *) calloc(edit_distance + 1, ap->bytes_in_state);
    if (!ap->state)
	goto out;

    ap->prev_state = (apse_vec_t *) calloc(edit_distance + 1, ap->bytes_in_state);
    if (!ap->prev_state)
	goto out;

    apse_reset(ap);

    if (!ap->has_different_distances) {
	ap->edit_insertions		= edit_distance;
	ap->edit_deletions		= edit_distance;
	ap->edit_substitutions		= edit_distance;
    }

    if (ap->edit_distance && ap->bitvectors_in_state)
	ap->largest_distance = ap->edit_distance * ap->bitvectors_in_state;

    ap->match_begin_bitvector	=
	(edit_distance + 1) / APSE_BITS_IN_BITVEC;
    ap->match_begin_prefix = ((apse_vec_t)1 << edit_distance) - 1;
    ap->match_begin_bitmask	=
	((apse_vec_t)1 << edit_distance) - 1;

    ap->match_end_bitvector =
	(ap->pattern_size - 1) / APSE_BITS_IN_BITVEC;

#ifdef APSE_DEBUGGING
    if (ap->has_different_distances) {
	printf("(edit distances: ");
	printf("insertions = %ld, deletions = %ld, substitutions = %ld)\n",
	       ap->edit_insertions,
	       ap->edit_deletions,
	       ap->edit_substitutions);
    } else
	printf("(edit_distance = %ld)\n", ap->edit_distance);
#endif

out:
    return ap->state && ap->prev_state;
}

#if 0
apse_size_t apse_get_edit_distance(apse_t *ap) {
    return ap->edit_distance;
}

apse_bool_t apse_set_minimal_distance(apse_t* ap, apse_bool_t minimal) {
    ap->use_minimal_distance = minimal;
    return 1;
}

apse_bool_t apse_get_minimal_distance(apse_t *ap) {
    return ap->use_minimal_distance;
}

apse_bool_t apse_set_exact_slice(apse_t*	ap,
				 apse_ssize_t	exact_begin,
				 apse_ssize_t	exact_size,
				 apse_bool_t	exact) {
    apse_ssize_t	i, j, true_begin, true_size;
    apse_bool_t	okay = 0;

    if (!ap->exact_mask) {

	ap->exact_mask = calloc((size_t)1, ap->bytes_in_state);
	if (!ap->exact_mask)
	    goto out;

	ap->exact_positions = 0;
    }

    if (!_apse_wrap_slice(ap, exact_begin, exact_size,
			      &true_begin, &true_size))
	goto out;

    if (exact) {
	for (i = true_begin, j = true_begin +  true_size;
	     i < j && i < ap->pattern_size; i++) {
	    if (!APSE_BIT_TST(ap->exact_mask, 0, 0, i))
		ap->exact_positions++;
	    APSE_BIT_SET(ap->exact_mask, 0, 0, i);
	}
    } else {
	for (i = true_begin, j = true_begin +  true_size;
	     i < j && i < ap->pattern_size; i++) {
	    if (APSE_BIT_TST(ap->exact_mask, 0, 0, i))
		ap->exact_positions--;
	    APSE_BIT_CLR(ap->exact_mask, 0, 0, i);
	}
    }

    okay = 1;

out:
    return okay;
}
#endif

attribute_hidden
apse_bool_t apse_set_caseignore_slice(apse_t*		ap,
				      apse_ssize_t	caseignore_begin,
				      apse_ssize_t	caseignore_size,
				      apse_bool_t	caseignore) {
    apse_size_t		i, j;
    int			k;
    apse_ssize_t	true_begin, true_size;
    apse_bool_t		okay = 0;

    if (!ap->fold_mask) {

	ap->fold_mask = (apse_vec_t *) calloc((apse_size_t)APSE_CHAR_MAX,
					      ap->bytes_in_state);
	if (!ap->fold_mask)
	    goto out;

	memcpy(ap->fold_mask,
	       ap->case_mask,
	       APSE_CHAR_MAX * ap->bytes_in_state);

	ap->pattern_mask = ap->fold_mask;
    }

    if (!_apse_wrap_slice(ap, caseignore_begin, caseignore_size,
			      &true_begin, &true_size))
	goto out;

    if (caseignore) {
	for (i = true_begin, j = true_begin +  true_size;
	     i < j && i < ap->pattern_size; i++) {
	    for (k = 0; k < APSE_CHAR_MAX; k++) {
		if (APSE_BIT_TST(ap->case_mask,
				 k, ap->bitvectors_in_state, i)) {
		    if (isupper(k))
			APSE_BIT_SET(ap->fold_mask,
				     tolower(k),
				     ap->bitvectors_in_state, i);
		    else if (islower(k))
			APSE_BIT_SET(ap->fold_mask,
				     toupper(k),
				     ap->bitvectors_in_state, i);
		}
	    }
	}
    } else {
	for (i = true_begin, j = true_begin +  true_size;
	     i < j && i < ap->pattern_size; i++) {
	    for (k = 0; k < APSE_CHAR_MAX; k++) {
		if (APSE_BIT_TST(ap->case_mask,
				 k, ap->bitvectors_in_state, i)) {
		    if (isupper(k))
			APSE_BIT_CLR(ap->fold_mask,
				     tolower(k),
				     ap->bitvectors_in_state, i);
		    else
			if (islower(k))
			    APSE_BIT_CLR(ap->fold_mask,
					 toupper(k),
					 ap->bitvectors_in_state, i);
		}
	    }
	}
    }

    okay = 1;

out:
    return okay;
}

attribute_hidden
void apse_destroy(apse_t *ap) {
    if (ap->case_mask)		free(ap->case_mask);
    if (ap->fold_mask)		free(ap->fold_mask);
    if (ap->state)		free(ap->state);
    if (ap->prev_state)		free(ap->prev_state);
    if (ap->exact_mask)	 	free(ap->exact_mask);
    free(ap);
}

attribute_hidden
apse_t *apse_create(unsigned char*	pattern,
		    apse_size_t		pattern_size,
		    apse_size_t		edit_distance) {
    apse_t		*ap;
    apse_bool_t	okay = 0;

    APSE_DEBUG(printf("(apse version %u.%u)\n",
		      APSE_MAJOR_VERSION, APSE_MINOR_VERSION));

    APSE_DEBUG(
	printf("(pattern = \"%s\", pattern_size = %ld)\n",
	       pattern, pattern_size));

    ap = (apse_t *) calloc((size_t)1, sizeof(*ap));
    if (!ap)
	return 0;

    ap->pattern_size		= 0;
    ap->pattern_mask		= 0;

    ap->edit_distance		= 0;
    ap->has_different_distances = 0;
    ap->edit_insertions		= 0;
    ap->edit_deletions		= 0;
    ap->edit_substitutions	= 0;
    ap->use_minimal_distance	= 0;

    ap->bitvectors_in_state	= 0;
    ap->bytes_in_state		= 0;
    ap->bytes_in_all_states	= 0;
    ap->largest_distance	= 0;

    ap->text			= 0;
    ap->text_size		= 0;
    ap->text_position		= 0;
    ap->text_initial_position	= 0;
    ap->text_final_position	= APSE_MATCH_BAD;
    ap->text_position_range	= APSE_MATCH_BAD;

    ap->state			= 0;
    ap->prev_state		= 0;
    ap->match_begin_bitmask	= 0;
    ap->match_begin_prefix	= 0;
    ap->match_end_bitvector	= 0;
    ap->match_end_bitmask	= 0;
    ap->match_state		= APSE_MATCH_STATE_BOT;
    ap->match_begin		= APSE_MATCH_BAD;
    ap->match_end		= APSE_MATCH_BAD;

    ap->match_bot_callback	= 0;
    ap->match_begin_callback	= 0;
    ap->match_fail_callback	= 0;
    ap->match_end_callback	= 0;
    ap->match_eot_callback	= 0;

    ap->exact_positions		= 0;
    ap->exact_mask		= 0;

    ap->is_greedy		= 0;

    ap->custom_data		= 0;
    ap->custom_data_size	= 0;

    if (!apse_set_pattern(ap, (unsigned char *)pattern, pattern_size))
	goto out;

    if (!apse_set_edit_distance(ap, edit_distance))
	goto out;

    ap->edit_insertions = ap->edit_deletions =
	ap->edit_substitutions = ap->edit_distance;

    ap->largest_distance = edit_distance * ap->bitvectors_in_state;

#ifdef APSE_DEBUGGING
    printf("(size of bitvector = %ld, bitvectors_in_state = %ld)\n",
	   (long)sizeof(apse_vec_t), ap->bitvectors_in_state);
    printf("(bytes_in_state = %ld, states = %ld, bytes_in_all_states = %ld)\n",
	   ap->bytes_in_state, ap->edit_distance + 1, ap->bytes_in_all_states);
    printf("(match_begin_bitvector = %ld, match_begin_bitmask = %s)\n",
	   ap->match_begin_bitvector,
	   _apse_fbin(ap->match_begin_bitmask, ap->pattern_size, 1));
    printf("(match_end_bitvector = %ld, match_end_bitmask = %s)\n",
	   ap->match_end_bitvector,
	   _apse_fbin(ap->match_end_bitmask, ap->pattern_size, 1));
    printf("(largest_distance = %ld, match_begin_prefix = %s)\n",
	   ap->largest_distance,
	   _apse_fbin(ap->match_begin_prefix, ap->pattern_size, 1));
    if (ap->bitvectors_in_state == 1)
	printf("(single bitvector");
    else
	printf("(multiple bitvectors");
    printf(")\n");
#endif
    okay = 1;

 out:
    if (!okay) {
	apse_destroy(ap);
	ap = 0;
    }

    return ap;
}

attribute_hidden
apse_bool_t apse_set_insertions(apse_t *ap, apse_size_t insertions) {
    apse_bool_t	okay = 0;

    if (insertions > ap->edit_distance)
	insertions = ap->edit_distance;
    ap->edit_insertions = insertions;
    ap->has_different_distances = 1;

    APSE_DEBUG((printf("(edit distances: insertions = %ld, deletions = %ld, substitutions = %ld)\n",
		       ap->edit_insertions,
		       ap->edit_deletions,
		       ap->edit_substitutions)));

    okay = 1;

    return okay;
}

#if 0
apse_size_t apse_get_insertions(apse_t *ap) {
    if (ap->has_different_distances)
	return ap->edit_insertions;
    else
	return ap->edit_distance;
}
#endif

attribute_hidden
apse_bool_t apse_set_deletions(apse_t *ap, apse_size_t deletions) {
    apse_bool_t	okay = 0;

    if (deletions > ap->edit_distance)
	deletions = ap->edit_distance;
    ap->edit_deletions = deletions;
    ap->has_different_distances = 1;

    APSE_DEBUG((printf("(edit distances: insertions = %ld, deletions = %ld, substitutions = %ld)\n",
		       ap->edit_insertions,
		       ap->edit_deletions,
		       ap->edit_substitutions)));

    okay = 1;

    return okay;
}

#if 0
apse_size_t apse_get_deletions(apse_t *ap) {
    if (ap->has_different_distances)
	return ap->edit_deletions;
    else
	return ap->edit_distance;
}
#endif

attribute_hidden
apse_bool_t apse_set_substitutions(apse_t *ap, apse_size_t substitutions) {
    apse_bool_t	okay = 0;

    if (substitutions > ap->edit_distance)
	substitutions = ap->edit_distance;
    ap->edit_substitutions = substitutions;
    ap->has_different_distances = 1;

    APSE_DEBUG((printf("(edit distances: insertions = %ld, deletions = %ld, substitutions = %ld)\n",
		       ap->edit_insertions,
		       ap->edit_deletions,
		       ap->edit_substitutions)));

    okay = 1;

    return okay;
}

#if 0
apse_size_t apse_get_substitutions(apse_t *ap) {
    if (ap->has_different_distances)
	return ap->edit_substitutions;
    else
	return ap->edit_distance;
}
#endif

#ifdef APSE_DEBUGGING
static const char* apse_match_state_name(apse_t* ap) {
    switch (ap->match_state) {
    case APSE_MATCH_STATE_BOT:		return "BOT";
    case APSE_MATCH_STATE_SEARCH:	return "SEARCH";
    case APSE_MATCH_STATE_BEGIN:	return "BEGIN";
    case APSE_MATCH_STATE_FAIL:		return "FAIL";
    case APSE_MATCH_STATE_GREEDY:	return "GREEDY";
    case APSE_MATCH_STATE_END:		return "END";
    case APSE_MATCH_STATE_EOT:		return "EOT";
    default:				return "***UNKNOWN***";
    }
}
#endif

static void _apse_match_bot(apse_t *ap) {
    APSE_DEBUG(printf("(text = \"%*.*s\", text_size = %ld)\n",
		       (int)ap->text_size, (int)ap->text_size, ap->text,
		       ap->text_size));
    apse_reset(ap);
    APSE_DEBUG(printf("(match state %s)\n", apse_match_state_name(ap)));
    APSE_DEBUG(printf("(text begin %ld)\n", ap->text_position));
    if (ap->match_bot_callback)
	ap->match_bot_callback(ap);
}

static void _apse_match_begin(apse_t *ap) {
    ap->match_state = APSE_MATCH_STATE_BEGIN;
    APSE_DEBUG(printf("(match state %s)\n", apse_match_state_name(ap)));
    APSE_DEBUG(printf("(match begin %ld)\n", ap->text_position));
    ap->match_begin = ap->text_position;
    if (ap->match_begin_callback)
	ap->match_begin_callback(ap);
}

static void _apse_match_fail(apse_t *ap) {
    ap->match_state = APSE_MATCH_STATE_FAIL;
    APSE_DEBUG(printf("(match state %s)\n", apse_match_state_name(ap)));
    ap->match_begin = APSE_MATCH_BAD;
    APSE_DEBUG(printf("(match fail %ld)\n", ap->text_position));
    if (ap->match_fail_callback)
	ap->match_fail_callback(ap);
    ap->match_state = APSE_MATCH_STATE_SEARCH;
    APSE_DEBUG(printf("(match state %s)\n", apse_match_state_name(ap)));
}

static void _apse_match_end(apse_t *ap) {
    ap->match_state = APSE_MATCH_STATE_END;
    APSE_DEBUG(printf("(match state %s)\n", apse_match_state_name(ap)));
#ifdef APSE_DEBUGGING
    printf("(match end %ld)\n", ap->match_end);
    printf("(match string \"%.*s\")\n",
	   (int)(ap->match_end - ap->match_begin + 1),
	   ap->text + ap->match_begin);
    printf("(match length %ld)\n",
	   ap->match_end - ap->match_begin + 1);
#endif
    if (ap->match_end_callback)
	ap->match_end_callback(ap);
    ap->match_state = APSE_MATCH_STATE_SEARCH;
    APSE_DEBUG(printf("(match state %s)\n", apse_match_state_name(ap)));
}

static void _apse_match_eot(apse_t *ap) {
    ap->match_state = APSE_MATCH_STATE_EOT;
    APSE_DEBUG(printf("(match state %s)\n", apse_match_state_name(ap)));
    ap->text_position = ap->text_size;
    APSE_DEBUG(printf("(text end %ld)\n", ap->text_position));
    if (ap->match_eot_callback)
	ap->match_eot_callback(ap);
}

static apse_bool_t _apse_match_next_state(apse_t *ap) {
    apse_size_t	h, i, j, k;
    apse_vec_t	match;

    k = ap->edit_distance * ap->bitvectors_in_state;

    switch (ap->match_state) {
    case APSE_MATCH_STATE_SEARCH:
	if (APSE_EXACT_MATCH_BEGIN(ap) || APSE_APPROX_MATCH_BEGIN(ap))
	    _apse_match_begin(ap);
	break;
    case APSE_MATCH_STATE_BEGIN:
	{
	    apse_size_t		equal	= 0;
	    apse_size_t		active	= 0;

	    for (h = 0;
		 h <= k;
		 h += ap->bitvectors_in_state) {
		for (i = h, j = i + ap->bitvectors_in_state - 1; i < j; j--)
		    if (ap->state[j] != ap->prev_state[j])
			break;
		if (ap->prev_state[j] == ap->state[j])
		    equal++;
		if (ap->state[j])
		    active++;
	    }
#ifdef APSE_DEBUGGING
	    printf("(equal = %d, active = %d)\n", equal, active);
#endif
	    if ((equal == ap->edit_distance + 1 && ap->is_greedy == 0) ||
		(equal < ap->prev_equal && ap->prev_active &&
		 active > ap->prev_active &&
		 !APSE_BIT_TST(ap->state,
			       ap->edit_distance,
			       ap->bitvectors_in_state,
			       ap->text_position - ap->match_begin))) {
		ap->match_begin = ap->text_position;
#ifdef APSE_DEBUGGING
		printf("(slide begin %d)\n", ap->match_begin);
#endif
	    }
	    else if (active == 0)
		_apse_match_fail(ap);
	    ap->prev_equal  = equal;
	    ap->prev_active = active;
	}
	break;
    default:
	break;
    }

    for (match = 0, h = 0;
	 h <= k;
	 h += ap->bitvectors_in_state)
	match |= ap->state[h + ap->match_end_bitvector];

    if (match & ap->match_end_bitmask) {
	if (ap->match_state == APSE_MATCH_STATE_BEGIN) {
	    if (ap->is_greedy) {
		ap->match_state = APSE_MATCH_STATE_GREEDY;
		APSE_DEBUG(printf("(match state %s)\n",
				   apse_match_state_name(ap)));
		APSE_DEBUG(
		    printf("(greedy match continue %ld)\n",
			   ap->text_position));
	    } else {
		ap->match_state = APSE_MATCH_STATE_END;
		ap->match_end   = ap->text_position;
	    }
	}
    } else if (ap->match_state == APSE_MATCH_STATE_GREEDY) {
	ap->match_state = APSE_MATCH_STATE_END;
	APSE_DEBUG(printf("(match state %s)\n", apse_match_state_name(ap)));
	ap->match_end	= ap->text_position - 1;
	APSE_DEBUG(printf("(match end %ld)\n",  ap->match_end));
    }

    return ap->match_state;
}

static void _apse_exact_multiple(apse_t* ap) {
    apse_size_t	h;
    apse_size_t	g = ap->edit_distance * ap->bitvectors_in_state;

    for (h = 0; h < ap->bitvectors_in_state; h++)
	ap->state[g + h] &= ~ap->exact_mask[h];
} 

static apse_bool_t _apse_match_single_simple(apse_t *ap) {
    /* single apse_vec_t, edit_distance */

    APSE_DEBUG(printf("(match single simple)\n"));
    for ( ; ap->text_position < ap->text_size; ap->text_position++) {
	apse_vec_t	t =
      	    ap->pattern_mask[(unsigned)ap->text[ap->text_position] *
			    ap->bitvectors_in_state];
	apse_size_t	h, g;
	APSE_NEXT_EXACT(ap->state, ap->prev_state, t, (apse_size_t)0, 1);
	APSE_DEBUG_SINGLE(ap, (apse_size_t)0);

	for (g = 0, h = 1; h <= ap->edit_distance; g = h, h++) {
	    APSE_NEXT_APPROX(ap->state, ap->prev_state, t, h, g, 1);
	    APSE_DEBUG_SINGLE(ap, h);
	}

	if (ap->exact_positions)
	    ap->state[ap->edit_distance] &= ~ap->exact_mask[0];

	if (_apse_match_next_state(ap) == APSE_MATCH_STATE_END)
	    return 1;

	(void)memcpy(ap->prev_state, ap->state, ap->bytes_in_all_states);
    }

    return 0;
}

static apse_bool_t _apse_match_multiple_simple(apse_t *ap) {
    /* multiple apse_vec_t:s, has_different_distances */
    apse_size_t	h, i;

    APSE_DEBUG(printf("(match multiple simple)\n"));
    for ( ; ap->text_position < ap->text_size; ap->text_position++) {
	apse_vec_t	*t =
	    ap->pattern_mask +
	    (unsigned)ap->text[ap->text_position] * ap->bitvectors_in_state;
	apse_vec_t	c, d;

	APSE_DEBUG_MULTIPLE_FIRST(ap, (apse_size_t)0);
	for (c = 1, i = 0; i < ap->bitvectors_in_state; i++, c = d) {
	    d = APSE_TEST_HIGH_BIT(ap->state[i]);
	    APSE_NEXT_EXACT(ap->state, ap->prev_state, t[i], i, c);
	    APSE_DEBUG_MULTIPLE_REST(ap, i, i);
	}
	APSE_DEBUG(printf("\n"));

	for (h = 1; h <= ap->edit_distance; h++) {
	    apse_size_t	kj = h * ap->bitvectors_in_state,
			jj = kj - ap->bitvectors_in_state;

	    APSE_DEBUG_MULTIPLE_FIRST(ap, h);
	    for (c = 1, i = 0;
		 i < ap->bitvectors_in_state;
		 i++, kj++, jj++, c = d) {
		d = APSE_TEST_HIGH_BIT(ap->state[kj]);
		APSE_NEXT_APPROX(ap->state, ap->prev_state,
				  t[i], kj, jj, c);
		APSE_DEBUG_MULTIPLE_REST(ap, i, kj);
	    }
	    APSE_DEBUG(printf("\n"));
	}

	if (ap->exact_positions)
	    _apse_exact_multiple(ap);

	if (_apse_match_next_state(ap) == APSE_MATCH_STATE_END)
	    return 1;

	(void)memcpy(ap->prev_state, ap->state,
		     ap->bytes_in_all_states);
    }

    return 0;
}

static apse_bool_t _apse_match_single_complex(apse_t *ap) {
    /* single apse_vec_t, has_different_distances */
    APSE_DEBUG(printf("(match single complex)\n"));
    for ( ; ap->text_position < ap->text_size; ap->text_position++) {
	unsigned char	o = ap->text[ap->text_position];
	apse_vec_t	t =
	    ap->pattern_mask[(unsigned int)o * ap->bitvectors_in_state];
	apse_size_t	h, g;

	APSE_NEXT_EXACT(ap->state, ap->prev_state, t, (apse_size_t)0, 1);
	APSE_DEBUG_SINGLE(ap, (apse_size_t)0);

	for (g = 0, h = 1; h <= ap->edit_distance; g = h, h++) {
	    apse_bool_t has_insertions    = h <= ap->edit_insertions;
	    apse_bool_t has_deletions     = h <= ap->edit_deletions;
	    apse_bool_t has_substitutions = h <= ap->edit_substitutions;

	    APSE_NEXT_COMMON(ap->state, ap->prev_state, t, h);
	    if (has_insertions)
		APSE_NEXT_INSERT(ap->state, ap->prev_state, h, g);
	    if (has_deletions)
		APSE_NEXT_DELETE(ap->state, h, g);
	    if (has_substitutions)
		APSE_NEXT_SUBSTI(ap->state, ap->prev_state, h, g);
	    APSE_NEXT_CARRY(ap->state, h,
			    has_deletions || has_substitutions ? 1 : 0);
	    APSE_PREFIX_DELETE_MASK(ap);
	    APSE_DEBUG_SINGLE(ap, h);
	}

	if (ap->exact_positions)
	    ap->state[ap->edit_distance] &= ~ap->exact_mask[0];

	if (_apse_match_next_state(ap) == APSE_MATCH_STATE_END)
	    return 1;

	(void)memcpy(ap->prev_state, ap->state,
		     ap->bytes_in_all_states);

    }

    return 0;
}

static apse_bool_t _apse_match_multiple_complex(apse_t *ap) {
    /* multiple apse_vec_t:s, has_different_distances */
    apse_size_t	h, i;

    APSE_DEBUG(printf("(match multiple complex)\n"));
    for ( ; ap->text_position < ap->text_size; ap->text_position++) {
	unsigned char	o = ap->text[ap->text_position];
	apse_vec_t	*t =
	    ap->pattern_mask + (unsigned)o * ap->bitvectors_in_state;
	apse_vec_t	c, d;

	APSE_DEBUG_MULTIPLE_FIRST(ap, (apse_size_t)0);
	for (c = 1, i = 0; i < ap->bitvectors_in_state; i++, c = d) {
	    d = APSE_TEST_HIGH_BIT(ap->state[i]);
	    APSE_NEXT_EXACT(ap->state, ap->prev_state, t[i], i, c);
	    APSE_DEBUG_MULTIPLE_REST(ap, i, i);
	}
	APSE_DEBUG(printf("\n"));

	for (h = 1; h <= ap->edit_distance; h++) {
	    apse_size_t
		kj = h * ap->bitvectors_in_state,
		jj = kj - ap->bitvectors_in_state;

	    apse_bool_t	has_insertions    = h <= ap->edit_insertions;
	    apse_bool_t	has_deletions     = h <= ap->edit_deletions;
	    apse_bool_t	has_substitutions = h <= ap->edit_substitutions;

	    APSE_DEBUG_MULTIPLE_FIRST(ap, h);
	    /* Is there such a thing as too much manual optimization? */
	    if (has_insertions) {
		if (has_deletions && has_substitutions) {
		    for (c = 1, i = 0;
			 i < ap->bitvectors_in_state;
			 i++, kj++, jj++, c = d) {
			d = APSE_TEST_HIGH_BIT(ap->state[kj]);
			APSE_NEXT_COMMON(ap->state, ap->prev_state, t[i], kj);
			APSE_NEXT_INSERT(ap->state, ap->prev_state, kj, jj);
			APSE_NEXT_DELETE(ap->state, kj, jj);
			APSE_NEXT_SUBSTI(ap->state, ap->prev_state, kj, jj);
			APSE_NEXT_CARRY(ap->state, kj, c);
			APSE_PREFIX_DELETE_MASK(ap);
			APSE_DEBUG_MULTIPLE_REST(ap, i, kj);
		    }
		} else if (has_deletions) {
		    for (c = 1, i = 0;
			 i < ap->bitvectors_in_state;
			 i++, kj++, jj++, c = d) {
			d = APSE_TEST_HIGH_BIT(ap->state[kj]);
			APSE_NEXT_COMMON(ap->state, ap->prev_state, t[i], kj);
			APSE_NEXT_INSERT(ap->state, ap->prev_state, kj, jj);
			APSE_NEXT_DELETE(ap->state, kj, jj);
			APSE_NEXT_CARRY(ap->state, kj, c);
			APSE_PREFIX_DELETE_MASK(ap);
			APSE_DEBUG_MULTIPLE_REST(ap, i, kj);
		    }
		} else if (has_substitutions) {
		    for (c = 1, i = 0;
			 i < ap->bitvectors_in_state;
			 i++, kj++, jj++, c = d) {
			d = APSE_TEST_HIGH_BIT(ap->state[kj]);
			APSE_NEXT_COMMON(ap->state, ap->prev_state, t[i], kj);
			APSE_NEXT_INSERT(ap->state, ap->prev_state, kj, jj);
			APSE_NEXT_SUBSTI(ap->state, ap->prev_state, kj, jj);
			APSE_NEXT_CARRY(ap->state, kj, c);
			APSE_PREFIX_DELETE_MASK(ap);
			APSE_DEBUG_MULTIPLE_REST(ap, i, kj);
		    }
		} else {
		    for (c = 1, i = 0;
			 i < ap->bitvectors_in_state;
			 i++, kj++, jj++, c = d) {
			d = APSE_TEST_HIGH_BIT(ap->state[kj]);
			APSE_NEXT_COMMON(ap->state, ap->prev_state, t[i], kj);
			APSE_NEXT_INSERT(ap->state, ap->prev_state, kj, jj);
			APSE_NEXT_CARRY(ap->state, kj, c);
			APSE_PREFIX_DELETE_MASK(ap);
			APSE_DEBUG_MULTIPLE_REST(ap, i, kj);
		    }
		}
	    } else {
		if (has_deletions && has_substitutions) {
		    for (c = 1, i = 0;
			 i < ap->bitvectors_in_state;
			 i++, kj++, jj++, c = d) {
			d = APSE_TEST_HIGH_BIT(ap->state[kj]);
			APSE_NEXT_COMMON(ap->state, ap->prev_state, t[i], kj);
			APSE_NEXT_DELETE(ap->state, kj, jj);
			APSE_NEXT_SUBSTI(ap->state, ap->prev_state, kj, jj);
			APSE_NEXT_CARRY(ap->state, kj, c);
			APSE_PREFIX_DELETE_MASK(ap);
			APSE_DEBUG_MULTIPLE_REST(ap, i, kj);
		    }
		} else if (has_deletions) {
		    for (c = 1, i = 0;
			 i < ap->bitvectors_in_state;
			 i++, kj++, jj++, c = d) {
			d = APSE_TEST_HIGH_BIT(ap->state[kj]);
			APSE_NEXT_COMMON(ap->state, ap->prev_state, t[i], kj);
			APSE_NEXT_DELETE(ap->state, kj, jj);
			APSE_NEXT_CARRY(ap->state, kj, c);
			APSE_PREFIX_DELETE_MASK(ap);
			APSE_DEBUG_MULTIPLE_REST(ap, i, kj);
		    }
		} else if (has_substitutions) {
		    for (c = 1, i = 0;
			 i < ap->bitvectors_in_state;
			 i++, kj++, jj++, c = d) {
			d = APSE_TEST_HIGH_BIT(ap->state[kj]);
			APSE_NEXT_COMMON(ap->state, ap->prev_state, t[i], kj);
			APSE_NEXT_SUBSTI(ap->state, ap->prev_state, kj, jj);
			APSE_NEXT_CARRY(ap->state, kj, c);
			APSE_PREFIX_DELETE_MASK(ap);
			APSE_DEBUG_MULTIPLE_REST(ap, i, kj);
		    }
		}
	    }
	    APSE_DEBUG(printf("\n"));


	    if (ap->exact_positions)
		_apse_exact_multiple(ap);
		
	    if (_apse_match_next_state(ap) == APSE_MATCH_STATE_END)
		return 1;

	    (void)memcpy(ap->prev_state, ap->state,
			 ap->bytes_in_all_states);
	}
    }

    return 0;
}

static apse_bool_t __apse_match(apse_t		*ap,
				unsigned char	*text,
				apse_size_t	text_size) {
    apse_bool_t	did_match = 0;

    APSE_DEBUG(printf("(match enter)\n"));

    if (ap->match_state == APSE_MATCH_STATE_BOT) {
	ap->text      = text;
	if (ap->text_final_position == APSE_MATCH_BAD)
	    ap->text_size = text_size;
	else
	    ap->text_size =
		ap->text_final_position > text_size ?
		    text_size : ap->text_final_position + 1;
	_apse_match_bot(ap);
    } else if (ap->match_state == APSE_MATCH_STATE_EOT)
	goto leave;

    if (ap->edit_deletions     >= ap->pattern_size ||
	ap->edit_substitutions >= ap->pattern_size) {
	ap->match_state   = APSE_MATCH_STATE_END;
	ap->match_begin   = ap->text_initial_position;
	ap->match_end     = ap->text_size - 1;
	ap->text_position = ap->text_size;
	goto out;
    }

    if (ap->pattern_size - ap->edit_deletions >
	ap->text_size - ap->text_initial_position) {
	ap->match_state   = APSE_MATCH_STATE_EOT;
	ap->text_position = ap->text_size;
	goto out;
    }

    if (text_size + ap->edit_distance < ap->pattern_size + ap->text_position) {
	ap->text_position = ap->text_size;
	goto eot;
    }

    if (ap->match_state == APSE_MATCH_STATE_SEARCH) {
	ap->text_position++;
	_apse_reset_state(ap);
    }

    if (ap->text_position_range != APSE_MATCH_BAD &&
	ap->text_position - ap->text_initial_position >
	ap->text_position_range) {
	ap->match_state   = APSE_MATCH_STATE_END;
	goto eot;
    }

    ap->match_state = APSE_MATCH_STATE_SEARCH;
    APSE_DEBUG(printf("(match state %s)\n", apse_match_state_name(ap)));
    APSE_DEBUG(printf("(match search %ld)\n", ap->text_position));

    if (ap->has_different_distances) {
	if (ap->bitvectors_in_state == 1) {
	    if (_apse_match_single_complex(ap))
		goto out;
	} else {
	    if (_apse_match_multiple_complex(ap))
		goto out;
	}
    } else {
	if (ap->bitvectors_in_state == 1) {
	    if (_apse_match_single_simple(ap))
		goto out;
	} else {
	    if (_apse_match_multiple_simple(ap))
		goto out;
	}
    }

 out:

    if (ap->match_state == APSE_MATCH_STATE_GREEDY) {
	ap->match_state = APSE_MATCH_STATE_END;
	ap->match_end   = ap->text_position - 1;
	APSE_DEBUG(printf("(greedy match end %ld)\n", ap->match_end));
    }

    if (ap->match_state == APSE_MATCH_STATE_END) {
	_apse_match_end(ap);
	did_match = 1;
    }

  eot:

    if (ap->text_position == ap->text_size)
	_apse_match_eot(ap);

  leave:

    APSE_DEBUG(printf("(match leave)\n"));

    return did_match;
}

static apse_bool_t _apse_match(apse_t 		*ap,
			       unsigned char	*text,
			       apse_size_t	text_size) {
    if (ap->use_minimal_distance) {
	apse_set_edit_distance(ap, 0);
	if (__apse_match(ap, text, text_size))
	    return 1;
	else {
	    apse_size_t minimal_edit_distance;
	    apse_size_t previous_edit_distance = 0;
	    apse_size_t next_edit_distance;
	    
	    for (next_edit_distance = 1;
		 next_edit_distance <= ap->pattern_size;
		 next_edit_distance *= 2) {
		apse_set_edit_distance(ap, next_edit_distance);
		if (__apse_match(ap, text, text_size))
		    break;
		previous_edit_distance = next_edit_distance;
	    } 
	    minimal_edit_distance = next_edit_distance;
	    if (next_edit_distance > 1) {
		do {
		    minimal_edit_distance =
			(previous_edit_distance + next_edit_distance) / 2;
		    if (minimal_edit_distance == previous_edit_distance)
			break;
		    apse_set_edit_distance(ap, minimal_edit_distance);
		    if (__apse_match(ap, text, text_size))
			next_edit_distance     = minimal_edit_distance;
		    else
			previous_edit_distance = minimal_edit_distance;
		} while (previous_edit_distance <= next_edit_distance);
		if (!__apse_match(ap, text, text_size))
		    minimal_edit_distance++;
	    }
	    apse_set_edit_distance(ap, minimal_edit_distance);
	    __apse_match(ap, text, text_size);
	    
	    return 1;
	}
    } else
	return __apse_match(ap, text, text_size);
}

attribute_hidden
apse_bool_t apse_match(apse_t *ap,
		       unsigned char *text, apse_size_t text_size) {
    apse_bool_t did_match = _apse_match(ap, text, text_size);

    _apse_match_eot(ap);
    apse_reset(ap);

    return did_match;
}

#if 0
apse_bool_t apse_match_next(apse_t *ap,
			    unsigned char *text, apse_size_t text_size) {
    apse_bool_t did_match = _apse_match(ap, text, text_size);

    if (!did_match)
	ap->match_state = APSE_MATCH_STATE_BOT;

    return did_match;
}

apse_ssize_t apse_index(apse_t *ap,
			 unsigned char *text, apse_size_t text_size) {
    apse_size_t did_match = _apse_match(ap, text, text_size);
    _apse_match_eot(ap);
    ap->match_state = APSE_MATCH_STATE_BOT;

    return did_match ? ap->match_begin : APSE_MATCH_BAD;
}

apse_ssize_t apse_index_next(apse_t *ap,
			     unsigned char *text, apse_size_t text_size) {
    apse_bool_t did_match = _apse_match(ap, text, text_size);

    if (!did_match)
	ap->match_state = APSE_MATCH_STATE_BOT;

    return did_match ? ap->match_begin : APSE_MATCH_BAD;
}

static apse_bool_t _apse_slice(apse_t *ap,
			       unsigned char *text,
			       apse_size_t text_size,
			       apse_size_t *match_begin,
			       apse_size_t *match_size) {
    apse_bool_t did_match = _apse_match(ap, text, text_size);

    if (did_match) {
	if (match_begin)
	    *match_begin = ap->match_begin;
	if (match_size)
	    *match_size	 = ap->match_end - ap->match_begin + 1;
    } else {
	if (match_begin)
	    *match_begin = APSE_MATCH_BAD;
	if (match_size)
	    *match_size	 = APSE_MATCH_BAD;
    }

    return did_match;
}

apse_bool_t apse_slice(apse_t *ap,
		       unsigned char *text,
		       apse_size_t text_size,
		       apse_size_t *match_begin,
		       apse_size_t *match_size) {
    apse_bool_t did_match =
        _apse_slice(ap, text, text_size, match_begin, match_size);

    _apse_match_eot(ap);
    ap->match_state = APSE_MATCH_STATE_BOT;

    return did_match;
}

apse_bool_t apse_slice_next(apse_t*		ap,
			    unsigned char*	text,
			    apse_size_t		text_size,
			    apse_size_t*	match_begin,
			    apse_size_t*	match_size) {
    apse_bool_t did_match =
        _apse_slice(ap, text, text_size, match_begin, match_size);

    if (!did_match)
	ap->match_state = APSE_MATCH_STATE_BOT;

    return did_match;
}

void apse_set_custom_data(apse_t*	ap,
			  void*		custom_data,
			  apse_size_t	custom_data_size) {
    ap->custom_data      = custom_data;
    ap->custom_data_size = custom_data_size;
}

void* apse_get_custom_data(apse_t*	ap,
			   apse_size_t*	custom_data_size) {
    if (custom_data_size)
	*custom_data_size = ap->custom_data_size;
    return ap->custom_data;
}
#endif
