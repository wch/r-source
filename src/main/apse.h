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

/*

  $Id: apse.h,v 1.3 2003/04/25 16:37:46 ripley Exp $

*/

#ifndef APSE_H
#define APSE_H

#define APSE_MAJOR_VERSION	0
#define APSE_MINOR_VERSION	15

#include <sys/types.h>

#ifdef APSE_VEC_T
typedef	APSE_VEC_T	apse_vec_t;
#else
typedef	unsigned long	apse_vec_t;
#endif

#ifdef APSE_SIZE_T
typedef	APSE_SIZE_T	apse_size_t;
#else
typedef	unsigned long	apse_size_t;
#endif

#ifdef APSE_SSIZE_T
typedef	APSE_SSIZE_T	apse_ssize_t;
#else
typedef	long		apse_ssize_t;
#endif

#ifdef APSE_BOOL_T
typedef	APSE_BOOL_T	apse_bool_t;
#else
typedef	int		apse_bool_t;
#endif

typedef struct apse_s {
    apse_size_t		pattern_size;
    apse_vec_t*		pattern_mask;

    apse_vec_t*		case_mask;
    apse_vec_t*		fold_mask;

    apse_size_t		edit_distance;
    apse_bool_t		has_different_distances;
    apse_size_t		different_distances_max;
    apse_size_t		edit_insertions;
    apse_size_t		edit_deletions;
    apse_size_t		edit_substitutions;
    apse_bool_t		use_minimal_distance;

    apse_size_t		bitvectors_in_state;
    apse_size_t		bytes_in_state;
    apse_size_t		bytes_in_all_states;
    apse_size_t		largest_distance;

    unsigned char*	text;
    apse_size_t		text_size;
    apse_size_t		text_position;
    apse_size_t		text_initial_position;
    apse_size_t		text_final_position;
    apse_size_t		text_position_range;

    apse_vec_t*		state;
    apse_vec_t*		prev_state;
    apse_size_t		prev_equal;
    apse_size_t		prev_active;
    apse_size_t		match_begin_bitvector;
    apse_vec_t		match_begin_bitmask;
    apse_vec_t		match_begin_prefix;
    apse_size_t		match_end_bitvector;
    apse_vec_t		match_end_bitmask;
    apse_bool_t		match_state;
    apse_size_t		match_begin;
    apse_size_t		match_end;

    void*		(*match_bot_callback)  (struct apse_s *ap);
    void*		(*match_begin_callback)(struct apse_s *ap);
    void*		(*match_fail_callback) (struct apse_s *ap);
    void*		(*match_end_callback)  (struct apse_s *ap);
    void*		(*match_eot_callback)  (struct apse_s *ap);

    apse_size_t	exact_positions;
    apse_vec_t*	exact_mask;

    apse_bool_t	is_greedy;

    void*	custom_data;
    apse_size_t	custom_data_size;
} apse_t;

apse_t *apse_create(unsigned char*	pattern,
		    apse_size_t		pattern_size,
		    apse_size_t		edit_distance);

apse_bool_t apse_match(apse_t*		ap,
		       unsigned char*	text,
		       apse_size_t	text_size);
#if 0
apse_bool_t apse_match_next(apse_t*		ap,
			    unsigned char*	text,
			    apse_size_t		text_size);
apse_ssize_t apse_index(apse_t*		ap,
			unsigned char*	text,
			apse_size_t 	text_size);
apse_ssize_t apse_index_next(apse_t*		ap,
			     unsigned char*	text,
			     apse_size_t 	text_size);
apse_bool_t apse_slice(apse_t*		ap,
		       unsigned char*	text,
		       apse_size_t	text_size,
		       apse_size_t*	match_begin,
		       apse_size_t*	match_size);
apse_bool_t apse_slice_next(apse_t*		ap,
			    unsigned char*	text,
			    apse_size_t		text_size,
			    apse_size_t*	match_begin,
			    apse_size_t*	match_size);

void apse_reset(apse_t *ap);

apse_bool_t apse_set_pattern(apse_t*		ap,
			     unsigned char*	pattern,
			     apse_size_t	pattern_size);
apse_bool_t apse_set_edit_distance(apse_t *ap, apse_size_t edit_distance);
apse_size_t apse_get_edit_distance(apse_t *ap);
apse_bool_t apse_set_minimal_distance(apse_t* ap, apse_bool_t minimal);
apse_bool_t apse_get_minimal_distance(apse_t* ap);
apse_bool_t apse_set_text_position(apse_t *ap,
				   apse_size_t text_position);
apse_size_t apse_get_text_position(apse_t *ap);
apse_bool_t apse_set_text_initial_position(apse_t *ap,
					   apse_size_t text_initial_position);
apse_size_t apse_get_text_initial_position(apse_t *ap);
apse_bool_t apse_set_text_final_position(apse_t *ap,
					   apse_size_t text_final_position);
apse_size_t apse_get_text_final_position(apse_t *ap);
apse_bool_t apse_set_text_position_range(apse_t *ap,
					   apse_size_t text_position_range);
apse_size_t apse_get_text_position_range(apse_t *ap);
#endif

apse_bool_t apse_set_insertions(apse_t *ap, apse_size_t insertions);
apse_bool_t apse_set_deletions(apse_t *ap, apse_size_t deletions);
apse_bool_t apse_set_substitutions(apse_t *ap, apse_size_t substitutions);

#if 0
apse_size_t apse_get_insertions(apse_t *ap);
apse_size_t apse_get_deletions(apse_t *ap);
apse_size_t apse_get_substitutions(apse_t *ap);
#endif

apse_bool_t apse_set_caseignore_slice(apse_t*		ap,
				      apse_ssize_t	caseignore_begin,
				      apse_ssize_t	caseignore_size,
				      apse_bool_t	caseignore);

#if 0
void apse_set_greedy(apse_t *ap, apse_bool_t greedy);
apse_bool_t apse_get_greedy(apse_t *ap);

void apse_set_match_bot_callback(apse_t *ap,
				 void* (*match_bot_callback)(apse_t* ap));
void apse_set_match_begin_callback(apse_t *ap,
				   void* (*match_begin_callback)(apse_t* ap));
void apse_set_match_fail_callback(apse_t *ap,
				  void* (*match_fail_callback)(apse_t* ap));
void apse_set_match_end_callback(apse_t *ap,
				 void* (*match_end_callback)(apse_t* ap));
void apse_set_match_eot_callback(apse_t *ap,
				 void* (*match_eot_callback)(apse_t* ap));
void* (*apse_get_match_bot_callback(apse_t *ap))(apse_t *ap);
void* (*apse_get_match_begin_callback(apse_t *ap))(apse_t *ap);
void* (*apse_get_match_fail_callback(apse_t *ap))(apse_t *ap);
void* (*apse_get_match_end_callback(apse_t *ap))(apse_t *ap);
void* (*apse_get_match_eot_callback(apse_t *ap))(apse_t *ap);

apse_bool_t apse_set_anychar(apse_t *ap, apse_ssize_t pattern_index);

apse_bool_t apse_set_charset(apse_t*		ap,
			     apse_ssize_t	pattern_index,
			     unsigned char*	set,
			     apse_size_t	set_size,
			     apse_bool_t	complement);

apse_bool_t apse_set_exact_slice(apse_t*	ap,
				 apse_ssize_t	exact_begin,
				 apse_ssize_t	exact_size,
				 apse_bool_t	exact);

void apse_set_custom_data(apse_t*	ap,
			  void*		custom_data,
			  apse_size_t	custom_data_size);
void* apse_get_custom_data(apse_t*	ap,
			   apse_size_t*	custom_data_size);
#endif

void apse_destroy(apse_t *ap);

#define APSE_MATCH_BAD			((apse_size_t)  -1)

#endif /* #ifndef APSE_H */
