/*
  tre-compile.c - TRE regex compiler

  This software is released under a BSD-style license.
  See the file LICENSE for details and copyright.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "tre-internal.h"
#include "tre-mem.h"
#include "tre-stack.h"
#include "tre-ast.h"
#include "tre-parse.h"
#include "tre-compile.h"
#include "xmalloc.h"

/*
  Algorithms to setup tags so that submatch addressing can be done.
*/


/* Inserts a catenation node to the root of the tree given in `node'.
   As the left child a new tag with number `tag_id' to `node' is added,
   and the right child is the old root. */
static reg_errcode_t
tre_add_tag_left(tre_mem_t mem, tre_ast_node_t *node, int tag_id)
{
  tre_catenation_t *c;

  DPRINT(("add_tag_left: tag %d\n", tag_id));

  c = tre_mem_alloc(mem, sizeof(*c));
  if (c == NULL)
    return REG_ESPACE;
  c->left = tre_ast_new_literal(mem, TAG, tag_id);
  if (c->left == NULL)
    return REG_ESPACE;
  c->right = tre_mem_alloc(mem, sizeof(tre_ast_node_t));
  if (c->right == NULL)
    return REG_ESPACE;

  c->right->obj = node->obj;
  c->right->type = node->type;
  c->right->nullable = -1;
  c->right->submatch_id = -1;
  c->right->firstpos = NULL;
  c->right->lastpos = NULL;
  c->right->num_tags = 0;
  node->obj = c;
  node->type = CATENATION;
  return REG_OK;
}

/* Inserts a catenation node to the root of the tree given in `node'.
   As the right child a new tag with number `tag_id' to `node' is added,
   and the left child is the old root. */
static reg_errcode_t
tre_add_tag_right(tre_mem_t mem, tre_ast_node_t *node, int tag_id)
{
  tre_catenation_t *c;

  DPRINT(("tre_add_tag_right: tag %d\n", tag_id));

  c = tre_mem_alloc(mem, sizeof(*c));
  if (c == NULL)
    return REG_ESPACE;
  c->right = tre_ast_new_literal(mem, TAG, tag_id);
  if (c->right == NULL)
    return REG_ESPACE;
  c->left = tre_mem_alloc(mem, sizeof(tre_ast_node_t));
  if (c->left == NULL)
    return REG_ESPACE;

  c->left->obj = node->obj;
  c->left->type = node->type;
  c->left->nullable = -1;
  c->left->submatch_id = -1;
  c->left->firstpos = NULL;
  c->left->lastpos = NULL;
  c->left->num_tags = 0;
  node->obj = c;
  node->type = CATENATION;
  return REG_OK;
}

typedef enum {
  ADDTAGS_RECURSE,
  ADDTAGS_AFTER_ITERATION,
  ADDTAGS_AFTER_UNION_LEFT,
  ADDTAGS_AFTER_UNION_RIGHT,
  ADDTAGS_AFTER_CAT_LEFT,
  ADDTAGS_AFTER_CAT_RIGHT,
  ADDTAGS_SET_SUBMATCH_END
} tre_addtags_symbol_t;


typedef struct {
  int tag;
  int next_tag;
} tre_tag_states_t;


/* Go through `regset' and set submatch data for submatches that are
   using this tag. */
static void
tre_purge_regset(int *regset, tre_tnfa_t *tnfa, int tag)
{
  int i;

  for (i = 0; regset[i] >= 0; i++)
    {
      int id = regset[i] / 2;
      int start = !(regset[i] % 2);
      DPRINT(("  Using tag %d for %s offset of "
	      "submatch %d\n", tag,
	      start ? "start" : "end", id));
      if (start)
	tnfa->submatch_data[id].so_tag = tag;
      else
	tnfa->submatch_data[id].eo_tag = tag;
    }
  regset[0] = -1;
}


/* Adds tags to appropriate locations in the parse tree in `tree', so that
   subexpressions marked for submatch addressing can be traced. */
static reg_errcode_t
tre_add_tags(tre_mem_t mem, tre_stack_t *stack, tre_ast_node_t *tree,
	     tre_tnfa_t *tnfa)
{
  reg_errcode_t status = REG_OK;
  tre_addtags_symbol_t symbol;
  tre_ast_node_t *node = tree; /* Tree node we are currently looking at. */
  size_t bottom = tre_stack_num_items(stack);
  /* True for first pass (counting number of needed tags) */
  int first_pass = (mem == NULL || tnfa == NULL);
  int *regset, *orig_regset;
  unsigned int num_tags = 0; /* Total number of tags. */
  unsigned int num_minimals = 0;	 /* Number of special minimal tags. */
  unsigned int tag = 0;	    /* The tag that is to be added next. */
  unsigned int next_tag = 1; /* Next tag to use after this one. */
  int *parents;	    /* Stack of submatches the current submatch is
		       contained in. */
  int minimal_tag = -1; /* Tag that marks the beginning of a minimal match. */
  tre_tag_states_t *saved_states;

  tre_tag_direction_t direction = TRE_TAG_MINIMIZE;
  if (!first_pass)
    {
      tnfa->end_tag = 0;
      tnfa->minimal_tags[0] = -1;
    }

  regset = xmalloc(sizeof(*regset) * ((tnfa->num_submatches + 1) * 2));
  if (regset == NULL)
    return REG_ESPACE;
  regset[0] = -1;
  orig_regset = regset;

  parents = xmalloc(sizeof(*parents) * (tnfa->num_submatches + 1));
  if (parents == NULL)
    {
      xfree(regset);
      return REG_ESPACE;
    }
  parents[0] = -1;

  saved_states = xmalloc(sizeof(*saved_states) * (tnfa->num_submatches + 1));
  if (saved_states == NULL)
    {
      xfree(regset);
      xfree(parents);
      return REG_ESPACE;
    }
  else
    {
      unsigned int i;
      for (i = 0; i <= tnfa->num_submatches; i++)
	saved_states[i].tag = -1;
    }

  STACK_PUSH(stack, voidptr, node);
  STACK_PUSH(stack, int, ADDTAGS_RECURSE);

  while (status == REG_OK && tre_stack_num_items(stack) > bottom)
    {
      symbol = (tre_addtags_symbol_t)tre_stack_pop_int(stack);
      switch (symbol)
	{

	case ADDTAGS_SET_SUBMATCH_END:
	  {
	    int id = tre_stack_pop_int(stack);
	    int i;

	    /* Add end of this submatch to regset. */
	    for (i = 0; regset[i] >= 0; i++);
	    regset[i] = id * 2 + 1;
	    regset[i + 1] = -1;

	    /* Pop this submatch from the parents stack. */
	    for (i = 0; parents[i] >= 0; i++);
	    parents[i - 1] = -1;
	    break;
	  }

	case ADDTAGS_RECURSE:
	  node = tre_stack_pop_voidptr(stack);

	  if (node->submatch_id >= 0)
	    {
	      int id = node->submatch_id;
	      int i;


	      /* Add start of this submatch to regset. */
	      for (i = 0; regset[i] >= 0; i++);
	      regset[i] = id * 2;
	      regset[i + 1] = -1;

	      if (!first_pass)
		{
		  for (i = 0; parents[i] >= 0; i++);
		  tnfa->submatch_data[id].parents = NULL;
		  if (i > 0)
		    {
		      int *p = xmalloc(sizeof(*p) * (i + 1));
		      if (p == NULL)
			{
			  status = REG_ESPACE;
			  break;
			}
		      assert(tnfa->submatch_data[id].parents == NULL);
		      tnfa->submatch_data[id].parents = p;
		      for (i = 0; parents[i] >= 0; i++)
			p[i] = parents[i];
		      p[i] = -1;
		    }
		}

	      /* Add end of this submatch to regset after processing this
		 node. */
	      STACK_PUSHX(stack, int, node->submatch_id);
	      STACK_PUSHX(stack, int, ADDTAGS_SET_SUBMATCH_END);
	    }

	  switch (node->type)
	    {
	    case LITERAL:
	      {
		tre_literal_t *lit = node->obj;

		if (!IS_SPECIAL(lit) || IS_BACKREF(lit))
		  {
		    int i;
		    DPRINT(("Literal %d-%d\n",
			    (int)lit->code_min, (int)lit->code_max));
		    if (regset[0] >= 0)
		      {
			/* Regset is not empty, so add a tag before the
			   literal or backref. */
			if (!first_pass)
			  {
			    status = tre_add_tag_left(mem, node, tag);
			    tnfa->tag_directions[tag] = direction;
			    if (minimal_tag >= 0)
			      {
				DPRINT(("Minimal %d, %d\n", minimal_tag, tag));
				for (i = 0; tnfa->minimal_tags[i] >= 0; i++);
				tnfa->minimal_tags[i] = tag;
				tnfa->minimal_tags[i + 1] = minimal_tag;
				tnfa->minimal_tags[i + 2] = -1;
				minimal_tag = -1;
				num_minimals++;
			      }
			    tre_purge_regset(regset, tnfa, tag);
			  }
			else
			  {
			    DPRINT(("  num_tags = 1\n"));
			    node->num_tags = 1;
			  }

			DPRINT(("  num_tags++\n"));
			regset[0] = -1;
			tag = next_tag;
			num_tags++;
			next_tag++;
		      }
		  }
		else
		  {
		    assert(!IS_TAG(lit));
		  }
		break;
	      }
	    case CATENATION:
	      {
		tre_catenation_t *cat = node->obj;
		tre_ast_node_t *left = cat->left;
		tre_ast_node_t *right = cat->right;
		int reserved_tag = -1;
		DPRINT(("Catenation, next_tag = %d\n", next_tag));


		/* After processing right child. */
		STACK_PUSHX(stack, voidptr, node);
		STACK_PUSHX(stack, int, ADDTAGS_AFTER_CAT_RIGHT);

		/* Process right child. */
		STACK_PUSHX(stack, voidptr, right);
		STACK_PUSHX(stack, int, ADDTAGS_RECURSE);

		/* After processing left child. */
		STACK_PUSHX(stack, int, next_tag + left->num_tags);
		DPRINT(("  Pushing %d for after left\n",
			next_tag + left->num_tags));
		if (left->num_tags > 0 && right->num_tags > 0)
		  {
		    /* Reserve the next tag to the right child. */
		    DPRINT(("  Reserving next_tag %d to right child\n",
			    next_tag));
		    reserved_tag = next_tag;
		    next_tag++;
		  }
		STACK_PUSHX(stack, int, reserved_tag);
		STACK_PUSHX(stack, int, ADDTAGS_AFTER_CAT_LEFT);

		/* Process left child. */
		STACK_PUSHX(stack, voidptr, left);
		STACK_PUSHX(stack, int, ADDTAGS_RECURSE);

		}
	      break;
	    case ITERATION:
	      {
		tre_iteration_t *iter = node->obj;
		DPRINT(("Iteration\n"));

		if (first_pass)
		  {
		    STACK_PUSHX(stack, int, regset[0] >= 0 || iter->minimal);
		  }
		else
		  {
		    STACK_PUSHX(stack, int, tag);
		    STACK_PUSHX(stack, int, iter->minimal);
		  }
		STACK_PUSHX(stack, voidptr, node);
		STACK_PUSHX(stack, int, ADDTAGS_AFTER_ITERATION);

		STACK_PUSHX(stack, voidptr, iter->arg);
		STACK_PUSHX(stack, int, ADDTAGS_RECURSE);

		/* Regset is not empty, so add a tag here. */
		if (regset[0] >= 0 || iter->minimal)
		  {
		    if (!first_pass)
		      {
			int i;
			status = tre_add_tag_left(mem, node, tag);
			if (iter->minimal)
			  tnfa->tag_directions[tag] = TRE_TAG_MAXIMIZE;
			else
			  tnfa->tag_directions[tag] = direction;
			if (minimal_tag >= 0)
			  {
			    DPRINT(("Minimal %d, %d\n", minimal_tag, tag));
			    for (i = 0; tnfa->minimal_tags[i] >= 0; i++);
			    tnfa->minimal_tags[i] = tag;
			    tnfa->minimal_tags[i + 1] = minimal_tag;
			    tnfa->minimal_tags[i + 2] = -1;
			    minimal_tag = -1;
			    num_minimals++;
			  }
			tre_purge_regset(regset, tnfa, tag);
		      }

		    DPRINT(("  num_tags++\n"));
		    regset[0] = -1;
		    tag = next_tag;
		    num_tags++;
		    next_tag++;
		  }
		direction = TRE_TAG_MINIMIZE;
	      }
	      break;
	    case UNION:
	      {
		tre_union_t *uni = node->obj;
		tre_ast_node_t *left = uni->left;
		tre_ast_node_t *right = uni->right;
		int left_tag;
		int right_tag;

		if (regset[0] >= 0)
		  {
		    left_tag = next_tag;
		    right_tag = next_tag + 1;
		  }
		else
		  {
		    left_tag = tag;
		    right_tag = next_tag;
		  }

		DPRINT(("Union\n"));

		/* After processing right child. */
		STACK_PUSHX(stack, int, right_tag);
		STACK_PUSHX(stack, int, left_tag);
		STACK_PUSHX(stack, voidptr, regset);
		STACK_PUSHX(stack, int, regset[0] >= 0);
		STACK_PUSHX(stack, voidptr, node);
		STACK_PUSHX(stack, voidptr, right);
		STACK_PUSHX(stack, voidptr, left);
		STACK_PUSHX(stack, int, ADDTAGS_AFTER_UNION_RIGHT);

		/* Process right child. */
		STACK_PUSHX(stack, voidptr, right);
		STACK_PUSHX(stack, int, ADDTAGS_RECURSE);

		/* After processing left child. */
		STACK_PUSHX(stack, int, ADDTAGS_AFTER_UNION_LEFT);

		/* Process left child. */
		STACK_PUSHX(stack, voidptr, left);
		STACK_PUSHX(stack, int, ADDTAGS_RECURSE);

		/* Regset is not empty, so add a tag here. */
		if (regset[0] >= 0)
		  {
		    if (!first_pass)
		      {
			int i;
			status = tre_add_tag_left(mem, node, tag);
			tnfa->tag_directions[tag] = direction;
			if (minimal_tag >= 0)
			  {
			    DPRINT(("Minimal %d, %d\n", minimal_tag, tag));
			    for (i = 0; tnfa->minimal_tags[i] >= 0; i++);
			    tnfa->minimal_tags[i] = tag;
			    tnfa->minimal_tags[i + 1] = minimal_tag;
			    tnfa->minimal_tags[i + 2] = -1;
			    minimal_tag = -1;
			    num_minimals++;
			  }
			tre_purge_regset(regset, tnfa, tag);
		      }

		    DPRINT(("  num_tags++\n"));
		    regset[0] = -1;
		    tag = next_tag;
		    num_tags++;
		    next_tag++;
		  }

		if (node->num_submatches > 0)
		  {
		    /* The next two tags are reserved for markers. */
		    next_tag++;
		    tag = next_tag;
		    next_tag++;
		  }

		break;
	      }
	    }

	  if (node->submatch_id >= 0)
	    {
	      int i;
	      /* Push this submatch on the parents stack. */
	      for (i = 0; parents[i] >= 0; i++);
	      parents[i] = node->submatch_id;
	      parents[i + 1] = -1;
	    }

	  break; /* end case: ADDTAGS_RECURSE */

	case ADDTAGS_AFTER_ITERATION:
	  {
	    int minimal = 0;
	    int enter_tag;
	    node = tre_stack_pop_voidptr(stack);
	    if (first_pass)
	      {
		node->num_tags = ((tre_iteration_t *)node->obj)->arg->num_tags
		  + tre_stack_pop_int(stack);
		minimal_tag = -1;
	      }
	    else
	      {
		minimal = tre_stack_pop_int(stack);
		enter_tag = tre_stack_pop_int(stack);
		if (minimal)
		  minimal_tag = enter_tag;
	      }

	    DPRINT(("After iteration\n"));
	    if (!first_pass)
	      {
		DPRINT(("  Setting direction to %s\n",
			minimal ? "minimize" : "maximize"));
		if (minimal)
		  direction = TRE_TAG_MINIMIZE;
		else
		  direction = TRE_TAG_MAXIMIZE;
	      }
	    break;
	  }

	case ADDTAGS_AFTER_CAT_LEFT:
	  {
	    int new_tag = tre_stack_pop_int(stack);
	    next_tag = tre_stack_pop_int(stack);
	    DPRINT(("After cat left, tag = %d, next_tag = %d\n",
		    tag, next_tag));
	    if (new_tag >= 0)
	      {
		DPRINT(("  Setting tag to %d\n", new_tag));
		tag = new_tag;
	      }
	    break;
	  }

	case ADDTAGS_AFTER_CAT_RIGHT:
	  DPRINT(("After cat right\n"));
	  node = tre_stack_pop_voidptr(stack);
	  if (first_pass)
	    node->num_tags = ((tre_catenation_t *)node->obj)->left->num_tags
	      + ((tre_catenation_t *)node->obj)->right->num_tags;
	  break;

	case ADDTAGS_AFTER_UNION_LEFT:
	  DPRINT(("After union left\n"));
	  /* Lift the bottom of the `regset' array so that when processing
	     the right operand the items currently in the array are
	     invisible.	 The original bottom was saved at ADDTAGS_UNION and
	     will be restored at ADDTAGS_AFTER_UNION_RIGHT below. */
	  while (*regset >= 0)
	    regset++;
	  break;

	case ADDTAGS_AFTER_UNION_RIGHT:
	  {
	    int added_tags, tag_left, tag_right;
	    tre_ast_node_t *left = tre_stack_pop_voidptr(stack);
	    tre_ast_node_t *right = tre_stack_pop_voidptr(stack);
	    DPRINT(("After union right\n"));
	    node = tre_stack_pop_voidptr(stack);
	    added_tags = tre_stack_pop_int(stack);
	    if (first_pass)
	      {
		node->num_tags = ((tre_union_t *)node->obj)->left->num_tags
		  + ((tre_union_t *)node->obj)->right->num_tags + added_tags
		  + ((node->num_submatches > 0) ? 2 : 0);
	      }
	    regset = tre_stack_pop_voidptr(stack);
	    tag_left = tre_stack_pop_int(stack);
	    tag_right = tre_stack_pop_int(stack);

	    /* Add tags after both children, the left child gets a smaller
	       tag than the right child.  This guarantees that we prefer
	       the left child over the right child. */
	    /* XXX - This is not always necessary (if the children have
	       tags which must be seen for every match of that child). */
	    /* XXX - Check if this is the only place where tre_add_tag_right
	       is used.	 If so, use tre_add_tag_left (putting the tag before
	       the child as opposed after the child) and throw away
	       tre_add_tag_right. */
	    if (node->num_submatches > 0)
	      {
		if (!first_pass)
		  {
		    status = tre_add_tag_right(mem, left, tag_left);
		    tnfa->tag_directions[tag_left] = TRE_TAG_MAXIMIZE;
		    status = tre_add_tag_right(mem, right, tag_right);
		    tnfa->tag_directions[tag_right] = TRE_TAG_MAXIMIZE;
		  }
		DPRINT(("  num_tags += 2\n"));
		num_tags += 2;
	      }
	    direction = TRE_TAG_MAXIMIZE;
	    break;
	  }

	default:
	  assert(0);
	  break;

	} /* end switch(symbol) */
    } /* end while(tre_stack_num_items(stack) > bottom) */

  if (!first_pass)
    tre_purge_regset(regset, tnfa, tag);

  if (!first_pass && minimal_tag >= 0)
    {
      int i;
      DPRINT(("Minimal %d, %d\n", minimal_tag, tag));
      for (i = 0; tnfa->minimal_tags[i] >= 0; i++);
      tnfa->minimal_tags[i] = tag;
      tnfa->minimal_tags[i + 1] = minimal_tag;
      tnfa->minimal_tags[i + 2] = -1;
      minimal_tag = -1;
      num_minimals++;
    }

  DPRINT(("tre_add_tags: %s complete.  Number of tags %d.\n",
	  first_pass? "First pass" : "Second pass", num_tags));

  assert(tree->num_tags == num_tags);
  tnfa->end_tag = num_tags;
  tnfa->num_tags = num_tags;
  tnfa->num_minimals = num_minimals;
  xfree(orig_regset);
  xfree(parents);
  xfree(saved_states);
  return status;
}



/*
  AST to TNFA compilation routines.
*/

typedef enum {
  COPY_RECURSE,
  COPY_SET_RESULT_PTR
} tre_copyast_symbol_t;

/* Flags for tre_copy_ast(). */
#define COPY_REMOVE_TAGS	 1
#define COPY_MAXIMIZE_FIRST_TAG	 2

static reg_errcode_t
tre_copy_ast(tre_mem_t mem, tre_stack_t *stack, tre_ast_node_t *ast,
	     int flags, int *pos_add, tre_tag_direction_t *tag_directions,
	     tre_ast_node_t **copy, int *max_pos)
{
  reg_errcode_t status = REG_OK;
  size_t bottom = tre_stack_num_items(stack);
  int num_copied = 0;
  int first_tag = 1;
  tre_ast_node_t **result = copy;
  tre_copyast_symbol_t symbol;

  STACK_PUSH(stack, voidptr, ast);
  STACK_PUSH(stack, int, COPY_RECURSE);

  while (status == REG_OK && tre_stack_num_items(stack) > bottom)
    {
      tre_ast_node_t *node;
      if (status != REG_OK)
	break;

      symbol = (tre_copyast_symbol_t)tre_stack_pop_int(stack);
      switch (symbol)
	{
	case COPY_SET_RESULT_PTR:
	  result = tre_stack_pop_voidptr(stack);
	  break;
	case COPY_RECURSE:
	  node = tre_stack_pop_voidptr(stack);
	  switch (node->type)
	    {
	    case LITERAL:
	      {
		tre_literal_t *lit = node->obj;
		int pos = lit->position;
		long min = lit->code_min;
		long max = lit->code_max;
		if (!IS_SPECIAL(lit) || IS_BACKREF(lit))
		  {
		    /* XXX - e.g. [ab] has only one position but two
		       nodes, so we are creating holes in the state space
		       here.  Not fatal, just wastes memory. */
		    pos += *pos_add;
		    num_copied++;
		  }
		else if (IS_TAG(lit) && (flags & COPY_REMOVE_TAGS))
		  {
		    /* Change this tag to empty. */
		    min = EMPTY;
		    max = pos = -1;
		  }
		else if (IS_TAG(lit) && (flags & COPY_MAXIMIZE_FIRST_TAG)
			 && first_tag)
		  {
		    /* Maximize the first tag. */
		    tag_directions[max] = TRE_TAG_MAXIMIZE;
		    first_tag = 0;
		  }
		*result = tre_ast_new_literal(mem, min, max);
		if (*result == NULL) {
		  status = REG_ESPACE;
		  break;
		}
		if (!IS_SPECIAL(lit)) {
		  ((tre_literal_t *)(*result)->obj)->u.class = lit->u.class;
		  ((tre_literal_t *)(*result)->obj)->neg_classes = lit->neg_classes;
		} else if (IS_PARAMETER(lit)) {
		  ((tre_literal_t *)(*result)->obj)->u.params = lit->u.params;
		}

		if (pos > *max_pos)
		  *max_pos = pos;
		break;
	      }
	    case UNION:
	      {
		tre_union_t *uni = node->obj;
		tre_union_t *tmp;
		*result = tre_ast_new_union(mem, uni->left, uni->right);
		if (*result == NULL)
		  {
		    status = REG_ESPACE;
		    break;
		  }
		tmp = (*result)->obj;
		result = &tmp->left;
		STACK_PUSHX(stack, voidptr, uni->right);
		STACK_PUSHX(stack, int, COPY_RECURSE);
		STACK_PUSHX(stack, voidptr, &tmp->right);
		STACK_PUSHX(stack, int, COPY_SET_RESULT_PTR);
		STACK_PUSHX(stack, voidptr, uni->left);
		STACK_PUSHX(stack, int, COPY_RECURSE);
		break;
	      }
	    case CATENATION:
	      {
		tre_catenation_t *cat = node->obj;
		tre_catenation_t *tmp;
		*result = tre_ast_new_catenation(mem, cat->left, cat->right);
		if (*result == NULL)
		  {
		    status = REG_ESPACE;
		    break;
		  }
		tmp = (*result)->obj;
		tmp->left = NULL;
		tmp->right = NULL;
		result = &tmp->left;

		STACK_PUSHX(stack, voidptr, cat->right);
		STACK_PUSHX(stack, int, COPY_RECURSE);
		STACK_PUSHX(stack, voidptr, &tmp->right);
		STACK_PUSHX(stack, int, COPY_SET_RESULT_PTR);
		STACK_PUSHX(stack, voidptr, cat->left);
		STACK_PUSHX(stack, int, COPY_RECURSE);
		break;
	      }
	    case ITERATION:
	      {
		tre_iteration_t *iter = node->obj;
		STACK_PUSHX(stack, voidptr, iter->arg);
		STACK_PUSHX(stack, int, COPY_RECURSE);
		*result = tre_ast_new_iter(mem, iter->arg, iter->min,
					   iter->max, iter->minimal);
		if (*result == NULL)
		  {
		    status = REG_ESPACE;
		    break;
		  }
		iter = (*result)->obj;
		result = &iter->arg;
		break;
	      }
	    default:
	      assert(0);
	      break;
	    }
	  break;
	}
    }
  *pos_add += num_copied;
  return status;
}

typedef enum {
  EXPAND_RECURSE,
  EXPAND_AFTER_ITER
} tre_expand_ast_symbol_t;

/* Expands each iteration node that has a finite nonzero minimum or maximum
   iteration count to a catenated sequence of copies of the node.

   Nested bounds multiply, so the expansion can be far larger than the
   pattern: ((a{255}){255}){255} has 16 million positions.  Enforce
   TRE_MAX_POS as the copies are made, rather than leaving it to
   tre_compute_npfl() after all of them have been allocated. */
static reg_errcode_t
tre_expand_ast(tre_mem_t mem, tre_stack_t *stack, tre_ast_node_t *ast,
	       tre_tag_direction_t *tag_directions, int *max_depth)
{
  reg_errcode_t status = REG_OK;
  size_t bottom = tre_stack_num_items(stack);
  int pos_add = 0;
  int pos_add_total = 0;
  int max_pos = 0;
  /* Current approximate matching parameters. */
  int params[TRE_PARAM_LAST];
  /* Approximate parameter nesting level. */
  int params_depth = 0;
  int iter_depth = 0;
  int i;

  for (i = 0; i < TRE_PARAM_LAST; i++)
    params[i] = TRE_PARAM_DEFAULT;

  STACK_PUSHR(stack, voidptr, ast);
  STACK_PUSHR(stack, int, EXPAND_RECURSE);
  while (status == REG_OK && tre_stack_num_items(stack) > bottom)
    {
      tre_ast_node_t *node;
      tre_expand_ast_symbol_t symbol;

      if (status != REG_OK)
	break;

      DPRINT(("pos_add %d\n", pos_add));

      symbol = (tre_expand_ast_symbol_t)tre_stack_pop_int(stack);
      node = tre_stack_pop_voidptr(stack);
      switch (symbol)
	{
	case EXPAND_RECURSE:
	  switch (node->type)
	    {
	    case LITERAL:
	      {
		tre_literal_t *lit= node->obj;
		if (!IS_SPECIAL(lit) || IS_BACKREF(lit))
		  {
		    lit->position += pos_add;
		    if (lit->position > max_pos)
		      max_pos = lit->position;
		  }
		break;
	      }
	    case UNION:
	      {
		tre_union_t *uni = node->obj;
		STACK_PUSHX(stack, voidptr, uni->right);
		STACK_PUSHX(stack, int, EXPAND_RECURSE);
		STACK_PUSHX(stack, voidptr, uni->left);
		STACK_PUSHX(stack, int, EXPAND_RECURSE);
		break;
	      }
	    case CATENATION:
	      {
		tre_catenation_t *cat = node->obj;
		STACK_PUSHX(stack, voidptr, cat->right);
		STACK_PUSHX(stack, int, EXPAND_RECURSE);
		STACK_PUSHX(stack, voidptr, cat->left);
		STACK_PUSHX(stack, int, EXPAND_RECURSE);
		break;
	      }
	    case ITERATION:
	      {
		tre_iteration_t *iter = node->obj;
		STACK_PUSHX(stack, int, pos_add);
		STACK_PUSHX(stack, voidptr, node);
		STACK_PUSHX(stack, int, EXPAND_AFTER_ITER);
		STACK_PUSHX(stack, voidptr, iter->arg);
		STACK_PUSHX(stack, int, EXPAND_RECURSE);
		/* If we are going to expand this node at EXPAND_AFTER_ITER
		   then don't increase the `pos' fields of the nodes now, it
		   will get done when expanding. */
		if (iter->min > 1 || iter->max > 1)
		  pos_add = 0;
		iter_depth++;
		DPRINT(("iter\n"));
		break;
	      }
	    default:
	      assert(0);
	      break;
	    }
	  break;
	case EXPAND_AFTER_ITER:
	  {
	    tre_iteration_t *iter = node->obj;
	    int pos_add_last;
	    pos_add = tre_stack_pop_int(stack);
	    pos_add_last = pos_add;
	    if (iter->min > 1 || iter->max > 1)
	      {
		tre_ast_node_t *seq1 = NULL, *seq2 = NULL;
		int j;
		int pos_add_save = pos_add;

		/* Create a catenated sequence of copies of the node. */
		for (j = 0; j < iter->min; j++)
		  {
		    tre_ast_node_t *copy;
		    /* Remove tags from all but the last copy. */
		    int flags = ((j + 1 < iter->min)
				 ? COPY_REMOVE_TAGS
				 : COPY_MAXIMIZE_FIRST_TAG);
		    DPRINT(("  pos_add %d\n", pos_add));
		    pos_add_save = pos_add;
		    status = tre_copy_ast(mem, stack, iter->arg, flags,
					  &pos_add, tag_directions, &copy,
					  &max_pos);
		    if (status != REG_OK)
		      return status;
		    if (max_pos >= TRE_MAX_POS)
		      return REG_ESPACE;
		    if (seq1 != NULL)
		      seq1 = tre_ast_new_catenation(mem, seq1, copy);
		    else
		      seq1 = copy;
		    if (seq1 == NULL)
		      return REG_ESPACE;
		  }

		if (iter->max == -1)
		  {
		    /* No upper limit. */
		    pos_add_save = pos_add;
		    status = tre_copy_ast(mem, stack, iter->arg, 0,
					  &pos_add, NULL, &seq2, &max_pos);
		    if (status != REG_OK)
		      return status;
		    if (max_pos >= TRE_MAX_POS)
		      return REG_ESPACE;
		    seq2 = tre_ast_new_iter(mem, seq2, 0, -1, 0);
		    if (seq2 == NULL)
		      return REG_ESPACE;
		  }
		else
		  {
		    for (j = iter->min; j < iter->max; j++)
		      {
			tre_ast_node_t *copy;
			pos_add_save = pos_add;
			status = tre_copy_ast(mem, stack, iter->arg, 0,
					      &pos_add, NULL, &copy, &max_pos);
			if (status != REG_OK)
			  return status;
			if (max_pos >= TRE_MAX_POS)
			  return REG_ESPACE;
			if (seq2 != NULL)
			  seq2 = tre_ast_new_catenation(mem, copy, seq2);
			else
			  seq2 = copy;
			if (seq2 == NULL)
			  return REG_ESPACE;
			seq2 = tre_ast_new_iter(mem, seq2, 0, 1, 0);
			if (seq2 == NULL)
			  return REG_ESPACE;
		      }
		  }

		pos_add = pos_add_save;
		if (seq1 == NULL)
		  seq1 = seq2;
		else if (seq2 != NULL)
		  seq1 = tre_ast_new_catenation(mem, seq1, seq2);
		if (seq1 == NULL)
		  return REG_ESPACE;
		node->obj = seq1->obj;
		node->type = seq1->type;
	      }

	    iter_depth--;
	    pos_add_total += pos_add - pos_add_last;
	    if (iter_depth == 0)
	      pos_add = pos_add_total;

	    /* If approximate parameters are specified, surround the result
	       with two parameter setting nodes.  The one on the left sets
	       the specified parameters, and the one on the right restores
	       the old parameters. */
	    if (iter->params)
	      {
		tre_ast_node_t *tmp_l, *tmp_r, *tmp_node, *node_copy;
		int *old_params;

		tmp_l = tre_ast_new_literal(mem, PARAMETER, 0);
		if (!tmp_l)
		  return REG_ESPACE;
		((tre_literal_t *)tmp_l->obj)->u.params = iter->params;
		iter->params[TRE_PARAM_DEPTH] = params_depth + 1;
		tmp_r = tre_ast_new_literal(mem, PARAMETER, 0);
		if (!tmp_r)
		  return REG_ESPACE;
		old_params = tre_mem_alloc(mem, sizeof(*old_params)
					   * TRE_PARAM_LAST);
		if (!old_params)
		  return REG_ESPACE;
		for (i = 0; i < TRE_PARAM_LAST; i++)
		  old_params[i] = params[i];
		((tre_literal_t *)tmp_r->obj)->u.params = old_params;
		old_params[TRE_PARAM_DEPTH] = params_depth;
		/* XXX - this is the only place where ast_new_node is
		   needed -- should be moved inside AST module. */
		node_copy = tre_ast_new_node(mem, ITERATION,
					     sizeof(tre_iteration_t));
		if (!node_copy)
		  return REG_ESPACE;
		node_copy->obj = node->obj;
		tmp_node = tre_ast_new_catenation(mem, tmp_l, node_copy);
		if (!tmp_node)
		  return REG_ESPACE;
		tmp_node = tre_ast_new_catenation(mem, tmp_node, tmp_r);
		if (!tmp_node)
		  return REG_ESPACE;
		/* Replace the contents of `node' with `tmp_node'. */
		memcpy(node, tmp_node, sizeof(*node));
		node->obj = tmp_node->obj;
		node->type = tmp_node->type;
		params_depth++;
		if (params_depth > *max_depth)
		  *max_depth = params_depth;
	      }
	    break;
	  }
	default:
	  assert(0);
	  break;
	}
    }

#ifdef TRE_DEBUG
  DPRINT(("Expanded AST:\n"));
  tre_ast_print(ast);
#endif

  return status;
}

static tre_pos_and_tags_t *
tre_set_empty(tre_mem_t mem)
{
  tre_pos_and_tags_t *new_set;

  new_set = tre_mem_calloc(mem, sizeof(*new_set));
  if (new_set == NULL)
    return NULL;

  new_set[0].position = -1;
  new_set[0].code_min = -1;
  new_set[0].code_max = -1;

  return new_set;
}

static tre_pos_and_tags_t *
tre_set_one(tre_mem_t mem, int position, long code_min, long code_max,
	    tre_ctype_t class, tre_ctype_t *neg_classes, int backref)
{
  tre_pos_and_tags_t *new_set;

  new_set = tre_mem_calloc(mem, sizeof(*new_set) * 2);
  if (new_set == NULL)
    return NULL;

  new_set[0].position = position;
  new_set[0].code_min = code_min;
  new_set[0].code_max = code_max;
  new_set[0].class = class;
  new_set[0].neg_classes = neg_classes;
  new_set[0].backref = backref;
  new_set[1].position = -1;
  new_set[1].code_min = -1;
  new_set[1].code_max = -1;

  return new_set;
}

/* One way of matching the empty string through a nullable subtree: the
   tags on the way, the assertions that must hold there, and the matching
   parameters that are set on the way. */
typedef struct tre_empty_path {
  int *tags;		/* -1 terminated array of tags, or NULL. */
  int assertions;	/* Bitmask of ASSERT_* flags. */
  int *params;		/* TRE_PARAM_LAST parameters, or NULL. */
  struct tre_empty_path *next;
} tre_empty_path_t;

/* A list of empty paths in order of POSIX preference. */
typedef struct {
  tre_empty_path_t *head;
  tre_empty_path_t *tail;
} tre_empty_path_list_t;

/* Sets `*result' to the -1 terminated union of the tag arrays `t1' and
   `t2' (either may be NULL), or to NULL if both are empty.  Tag arrays are
   never changed once made, so the result may be one of them. */
static reg_errcode_t
tre_merge_tags(tre_mem_t mem, int *t1, int *t2, int **result)
{
  int n1, n2, n, i, j;
  int *tags;

  for (n1 = 0; t1 != NULL && t1[n1] >= 0; n1++);
  for (n2 = 0; t2 != NULL && t2[n2] >= 0; n2++);
  if (n1 == 0 || n2 == 0)
    {
      *result = n1 > 0 ? t1 : n2 > 0 ? t2 : NULL;
      return REG_OK;
    }

  tags = tre_mem_alloc(mem, sizeof(*tags) * (n1 + n2 + 1));
  if (tags == NULL)
    return REG_ESPACE;
  for (n = 0; n < n1; n++)
    tags[n] = t1[n];
  for (j = 0; j < n2; j++)
    {
      /* Don't add duplicates. */
      for (i = 0; i < n && tags[i] != t2[j]; i++);
      if (i == n)
	tags[n++] = t2[j];
    }
  tags[n] = -1;
  *result = tags;
  return REG_OK;
}

/* Sets `*result' to the parameters `p1' overridden by the values set in
   `p2'.  Either may be NULL. */
static reg_errcode_t
tre_merge_params(tre_mem_t mem, int *p1, int *p2, int **result)
{
  int *params;
  int i;

  if (p1 == NULL || p2 == NULL)
    {
      *result = p1 != NULL ? p1 : p2;
      return REG_OK;
    }

  params = tre_mem_alloc(mem, sizeof(*params) * TRE_PARAM_LAST);
  if (params == NULL)
    return REG_ESPACE;
  for (i = 0; i < TRE_PARAM_LAST; i++)
    params[i] = p2[i] != TRE_PARAM_UNSET ? p2[i] : p1[i];
  *result = params;
  return REG_OK;
}

/* Returns 1 if the parameter arrays `p1' and `p2' (either may be NULL) are
   the same. */
static int
tre_params_equal(const int *p1, const int *p2)
{
  int i;

  if (p1 == NULL || p2 == NULL)
    return p1 == p2;
  for (i = 0; i < TRE_PARAM_LAST; i++)
    if (p1[i] != p2[i])
      return 0;
  return 1;
}

/* Pairs of assertions where one holds exactly when the other does not. */
static const int tre_assertion_pairs[][2] = {
  { ASSERT_AT_BOL, ASSERT_NOT_BOL },
  { ASSERT_AT_EOL, ASSERT_NOT_EOL },
  { ASSERT_AT_BOW, ASSERT_NOT_BOW },
  { ASSERT_AT_EOW, ASSERT_NOT_EOW },
  { ASSERT_AT_WB, ASSERT_AT_WB_NEG }
};

/* Returns 1 if the assertions in the bitmask `assertions' can never hold
   together. */
static int
tre_assertions_contradict(int assertions)
{
  size_t i;

  for (i = 0; i < elementsof(tre_assertion_pairs); i++)
    if ((assertions & tre_assertion_pairs[i][0])
	&& (assertions & tre_assertion_pairs[i][1]))
      return 1;
  return 0;
}

/* Returns the assertion that holds exactly when `assertion' does not. */
static int
tre_assertion_negate(int assertion)
{
  size_t i;

  for (i = 0; i < elementsof(tre_assertion_pairs); i++)
    {
      if (assertion == tre_assertion_pairs[i][0])
	return tre_assertion_pairs[i][1];
      if (assertion == tre_assertion_pairs[i][1])
	return tre_assertion_pairs[i][0];
    }
  assert(0);
  return 0;
}

static tre_empty_path_t *
tre_empty_path_new(tre_mem_t mem, int *tags, int assertions, int *params)
{
  tre_empty_path_t *path = tre_mem_calloc(mem, sizeof(*path));
  if (path == NULL)
    return NULL;
  path->tags = tags;
  path->assertions = assertions;
  path->params = params;
  return path;
}

/* The path that adds nothing, for the plain union of two sets. */
static const tre_empty_path_t tre_no_path = { NULL, 0, NULL, NULL };

/* Appends to `list' the parts of the path that sets `tags', needs
   `assertions' and sets `params' that apply where no earlier path of the
   list does.  POSIX prefers the earlier paths, so the new one may only be
   taken where each of them fails, that is, where one of its assertions
   a1, ..., an does not hold.  For each of them, the path is split into
   disjoint parts that also need not a1, or a1 and not a2, ..., or a1, ...,
   an-1 and not an.  Parts that need contradicting assertions are left out.
   So the paths in a list never apply together: the preferred one is taken
   whenever it can be, and the next one only where it cannot. */
static reg_errcode_t
tre_empty_path_list_add(tre_mem_t mem, tre_empty_path_list_t *list,
			int *tags, int assertions, int *params)
{
  tre_empty_path_t *earlier, *part, *next, *parts, *kept, **tail;
  int needed, missing, bit;

  if (tre_assertions_contradict(assertions))
    return REG_OK;
  parts = tre_empty_path_new(mem, tags, assertions, params);
  if (parts == NULL)
    return REG_ESPACE;
  for (earlier = list->head; earlier != NULL && parts != NULL;
       earlier = earlier->next)
    {
      kept = NULL;
      tail = &kept;
      for (part = parts; part != NULL; part = next)
	{
	  next = part->next;
	  needed = part->assertions;
	  if (tre_assertions_contradict(needed | earlier->assertions))
	    {
	      /* The earlier path never applies together with this part. */
	      *tail = part;
	      tail = &part->next;
	      continue;
	    }
	  /* If nothing is missing, the earlier path applies wherever this
	     part does, and the part is left out. */
	  missing = earlier->assertions & ~needed;
	  for (bit = 1; missing != 0; bit <<= 1)
	    if (missing & bit)
	      {
		*tail = tre_empty_path_new(mem, part->tags,
					   needed | tre_assertion_negate(bit),
					   part->params);
		if (*tail == NULL)
		  return REG_ESPACE;
		tail = &(*tail)->next;
		needed |= bit;
		missing &= ~bit;
	      }
	}
      *tail = NULL;
      parts = kept;
    }

  if (parts == NULL)
    return REG_OK;
  if (list->tail != NULL)
    list->tail->next = parts;
  else
    list->head = parts;
  for (list->tail = parts; list->tail->next != NULL;
       list->tail = list->tail->next);
  return REG_OK;
}

/* Appends the paths `paths' to `list'. */
static reg_errcode_t
tre_empty_path_list_append(tre_mem_t mem, tre_empty_path_list_t *list,
			   const tre_empty_path_t *paths)
{
  reg_errcode_t status;

  for (; paths != NULL; paths = paths->next)
    {
      status = tre_empty_path_list_add(mem, list, paths->tags,
				       paths->assertions, paths->params);
      if (status != REG_OK)
	return status;
    }
  return REG_OK;
}

/* Appends the empty path through the literal `lit' to `list'. */
static reg_errcode_t
tre_empty_path_literal(tre_mem_t mem, tre_literal_t *lit,
		       tre_empty_path_list_t *list)
{
  int *tags = NULL;
  int assertions = 0;
  int *params = NULL;

  switch (lit->code_min)
    {
    case TAG:
      if (lit->code_max >= 0)
	{
	  tags = tre_mem_alloc(mem, sizeof(*tags) * 2);
	  if (tags == NULL)
	    return REG_ESPACE;
	  tags[0] = lit->code_max;
	  tags[1] = -1;
	}
      break;
    case ASSERTION:
      assert(lit->code_max >= 1 && lit->code_max <= ASSERT_LAST);
      assertions = lit->code_max;
      break;
    case PARAMETER:
      params = lit->u.params;
      break;
    case EMPTY:
      break;
    default:
      assert(0);
      break;
    }
  return tre_empty_path_list_add(mem, list, tags, assertions, params);
}

/* Appends to `list' each path of `left' followed by each path of
   `right'. */
static reg_errcode_t
tre_empty_path_catenate(tre_mem_t mem, tre_empty_path_list_t *list,
			const tre_empty_path_t *left,
			const tre_empty_path_t *right)
{
  const tre_empty_path_t *l, *r;
  int *tags, *params;
  reg_errcode_t status;

  for (l = left; l != NULL; l = l->next)
    for (r = right; r != NULL; r = r->next)
      {
	status = tre_merge_tags(mem, l->tags, r->tags, &tags);
	if (status != REG_OK)
	  return status;
	status = tre_merge_params(mem, l->params, r->params, &params);
	if (status != REG_OK)
	  return status;
	status = tre_empty_path_list_add(mem, list, tags,
					 l->assertions | r->assertions,
					 params);
	if (status != REG_OK)
	  return status;
      }
  return REG_OK;
}

/* Sets `*dst' to the set entry `src' reached through the empty path
   `path'. */
static reg_errcode_t
tre_set_entry_add_path(tre_mem_t mem, tre_pos_and_tags_t *dst,
		       const tre_pos_and_tags_t *src,
		       const tre_empty_path_t *path)
{
  reg_errcode_t status;

  *dst = *src;
  dst->assertions |= path->assertions;
  status = tre_merge_tags(mem, src->tags, path->tags, &dst->tags);
  if (status != REG_OK)
    return status;
  return tre_merge_params(mem, src->params, path->params, &dst->params);
}

/* Returns the union of `set2' and the entries of `set1' reached through
   each path of `paths', which add their tags, assertions, and parameters
   to the entries.  Entries that would need contradicting assertions are
   left out.  For the plain union of the sets, `paths' is &tre_no_path. */
static tre_pos_and_tags_t *
tre_set_union(tre_mem_t mem, tre_pos_and_tags_t *set1,
	      tre_pos_and_tags_t *set2, const tre_empty_path_t *paths)
{
  int s1, s2, n, i, num_paths;
  const tre_empty_path_t *path;
  tre_pos_and_tags_t *new_set;

  for (s1 = 0; set1[s1].position >= 0; s1++);
  for (s2 = 0; set2[s2].position >= 0; s2++);
  for (num_paths = 0, path = paths; path != NULL; path = path->next)
    num_paths++;

  new_set = tre_mem_calloc(mem, sizeof(*new_set) * (s1 * num_paths + s2 + 1));
  if (new_set == NULL)
    return NULL;

  n = 0;
  for (path = paths; path != NULL; path = path->next)
    for (i = 0; i < s1; i++)
      if (!tre_assertions_contradict(set1[i].assertions | path->assertions)
	  && tre_set_entry_add_path(mem, &new_set[n++], &set1[i], path)
	     != REG_OK)
	return NULL;

  for (i = 0; i < s2; i++)
    new_set[n++] = set2[i];
  new_set[n].position = -1;
  return new_set;
}

typedef enum {
  NPFL_RECURSE,
  NPFL_POST_UNION,
  NPFL_POST_CATENATION,
  NPFL_POST_ITERATION
} tre_npfl_stack_symbol_t;


/* Computes and fills in the fields `nullable', `position`, `firstpos',
   and `lastpos' for the nodes of the AST `tree'; `nextpos' points to an
   integer indicating the next available position, and will be updated on
   return to reflect the number of additional positions assigned. */
static reg_errcode_t
tre_compute_npfl(tre_mem_t mem, tre_stack_t *stack, tre_ast_node_t *tree,
		 int *nextpos)
{
  size_t bottom = tre_stack_num_items(stack);
  tre_empty_path_list_t paths;
  reg_errcode_t status;

  STACK_PUSHR(stack, voidptr, tree);
  STACK_PUSHR(stack, int, NPFL_RECURSE);

  while (tre_stack_num_items(stack) > bottom)
    {
      tre_npfl_stack_symbol_t symbol;
      tre_ast_node_t *node;

      symbol = (tre_npfl_stack_symbol_t)tre_stack_pop_int(stack);
      node = tre_stack_pop_voidptr(stack);
      switch (symbol)
	{
	case NPFL_RECURSE:
	  switch (node->type)
	    {
	    case LITERAL:
	      {
		tre_literal_t *lit = (tre_literal_t *)node->obj;
		if (IS_BACKREF(lit))
		  {
		    /* Back references: nullable = false, firstpos = {i},
		       lastpos = {i}. */
		    node->nullable = 0;
		    if (*nextpos == TRE_MAX_POS)
		      return REG_ESPACE;
		    lit->position = (*nextpos)++;
		    node->firstpos = tre_set_one(mem, lit->position, 0,
					     TRE_CHAR_MAX, 0, NULL, -1);
		    if (!node->firstpos)
		      return REG_ESPACE;
		    node->lastpos = tre_set_one(mem, lit->position, 0,
						TRE_CHAR_MAX, 0, NULL,
						lit->code_max);
		    if (!node->lastpos)
		      return REG_ESPACE;
		  }
		else if (lit->code_min < 0)
		  {
		    /* Tags, empty strings, params, and zero width assertions:
		       nullable = true, firstpos = {}, and lastpos = {}. */
		    node->nullable = 1;
		    node->firstpos = tre_set_empty(mem);
		    if (!node->firstpos)
		      return REG_ESPACE;
		    node->lastpos = tre_set_empty(mem);
		    if (!node->lastpos)
		      return REG_ESPACE;
		    paths.head = paths.tail = NULL;
		    status = tre_empty_path_literal(mem, lit, &paths);
		    if (status != REG_OK)
		      return status;
		    node->empty_paths = paths.head;
		  }
		else
		  {
		    /* Literal at position i: nullable = false, firstpos = {i},
		       lastpos = {i}. */
		    node->nullable = 0;
		    if (*nextpos == TRE_MAX_POS)
		      return REG_ESPACE;
		    lit->position = (*nextpos)++;
		    node->firstpos =
		      tre_set_one(mem, lit->position, lit->code_min,
				  lit->code_max, 0, NULL, -1);
		    if (!node->firstpos)
		      return REG_ESPACE;
		    node->lastpos = tre_set_one(mem, lit->position,
						lit->code_min,
						lit->code_max,
						lit->u.class, lit->neg_classes,
						-1);
		    if (!node->lastpos)
		      return REG_ESPACE;
		  }
		break;
	      }

	    case UNION:
	      /* Compute the attributes for the two subtrees, and after that
		 for this node. */
	      STACK_PUSHR(stack, voidptr, node);
	      STACK_PUSHR(stack, int, NPFL_POST_UNION);
	      STACK_PUSHR(stack, voidptr, ((tre_union_t *)node->obj)->right);
	      STACK_PUSHR(stack, int, NPFL_RECURSE);
	      STACK_PUSHR(stack, voidptr, ((tre_union_t *)node->obj)->left);
	      STACK_PUSHR(stack, int, NPFL_RECURSE);
	      break;

	    case CATENATION:
	      /* Compute the attributes for the two subtrees, and after that
		 for this node. */
	      STACK_PUSHR(stack, voidptr, node);
	      STACK_PUSHR(stack, int, NPFL_POST_CATENATION);
	      STACK_PUSHR(stack, voidptr, ((tre_catenation_t *)node->obj)->right);
	      STACK_PUSHR(stack, int, NPFL_RECURSE);
	      STACK_PUSHR(stack, voidptr, ((tre_catenation_t *)node->obj)->left);
	      STACK_PUSHR(stack, int, NPFL_RECURSE);
	      break;

	    case ITERATION:
	      /* Compute the attributes for the subtree, and after that for
		 this node. */
	      STACK_PUSHR(stack, voidptr, node);
	      STACK_PUSHR(stack, int, NPFL_POST_ITERATION);
	      STACK_PUSHR(stack, voidptr, ((tre_iteration_t *)node->obj)->arg);
	      STACK_PUSHR(stack, int, NPFL_RECURSE);
	      break;
	    }
	  break; /* end case: NPFL_RECURSE */

	case NPFL_POST_UNION:
	  {
	    tre_union_t *uni = (tre_union_t *)node->obj;
	    node->nullable = uni->left->nullable || uni->right->nullable;
	    /* The left operand is preferred. */
	    paths.head = paths.tail = NULL;
	    status = tre_empty_path_list_append(mem, &paths,
						uni->left->empty_paths);
	    if (status == REG_OK)
	      status = tre_empty_path_list_append(mem, &paths,
						  uni->right->empty_paths);
	    if (status != REG_OK)
	      return status;
	    node->empty_paths = paths.head;
	    node->firstpos = tre_set_union(mem, uni->left->firstpos,
					   uni->right->firstpos, &tre_no_path);
	    if (!node->firstpos)
	      return REG_ESPACE;
	    node->lastpos = tre_set_union(mem, uni->left->lastpos,
					  uni->right->lastpos, &tre_no_path);
	    if (!node->lastpos)
	      return REG_ESPACE;
	    break;
	  }

	case NPFL_POST_ITERATION:
	  {
	    tre_iteration_t *iter = (tre_iteration_t *)node->obj;

	    if (iter->min == 0 || iter->arg->nullable)
	      node->nullable = 1;
	    else
	      node->nullable = 0;
	    /* Matching the empty string through the argument is preferred
	       over skipping the iteration. */
	    paths.head = paths.tail = NULL;
	    status = tre_empty_path_list_append(mem, &paths,
						iter->arg->empty_paths);
	    if (status == REG_OK && iter->min == 0)
	      status = tre_empty_path_list_add(mem, &paths, NULL, 0, NULL);
	    if (status != REG_OK)
	      return status;
	    node->empty_paths = paths.head;
	    node->firstpos = iter->arg->firstpos;
	    node->lastpos = iter->arg->lastpos;
	    break;
	  }

	case NPFL_POST_CATENATION:
	  {
	    tre_catenation_t *cat = node->obj;
	    node->nullable = cat->left->nullable && cat->right->nullable;
	    if (node->nullable)
	      {
		paths.head = paths.tail = NULL;
		status = tre_empty_path_catenate(mem, &paths,
						 cat->left->empty_paths,
						 cat->right->empty_paths);
		if (status != REG_OK)
		  return status;
		node->empty_paths = paths.head;
	      }

	    /* Compute firstpos.  If the left side matches the empty string,
	       the first positions of the right side are reached through each
	       of the empty paths of the left side. */
	    if (cat->left->nullable)
	      {
		node->firstpos =
		  tre_set_union(mem, cat->right->firstpos, cat->left->firstpos,
				cat->left->empty_paths);
		if (!node->firstpos)
		  return REG_ESPACE;
	      }
	    else
	      node->firstpos = cat->left->firstpos;

	    /* Compute lastpos the same way. */
	    if (cat->right->nullable)
	      {
		node->lastpos =
		  tre_set_union(mem, cat->left->lastpos, cat->right->lastpos,
				cat->right->empty_paths);
		if (!node->lastpos)
		  return REG_ESPACE;
	      }
	    else
	      node->lastpos = cat->right->lastpos;
	    break;
	  }

	default:
	  assert(0);
	  break;
	}
    }

  return REG_OK;
}


/* Adds a transition from each position in `p1' to each position in `p2'. */
static reg_errcode_t
tre_make_trans(tre_pos_and_tags_t *p1, tre_pos_and_tags_t *p2,
	       tre_tnfa_transition_t *transitions,
	       int *counts, int *offs)
{
  tre_pos_and_tags_t *orig_p2 = p2;
  tre_tnfa_transition_t *trans;
  int i, j, k, l, dup;

  if (transitions != NULL)
    while (p1->position >= 0)
      {
	p2 = orig_p2;
	while (p2->position >= 0)
	  {
	    /* A transition that needs contradicting assertions is never
	       taken. */
	    if (tre_assertions_contradict(p1->assertions | p2->assertions))
	      {
		p2++;
		continue;
	      }
	    /* Set `trans' to point to the next unused transition from
	       position `p1->position'. */
	    trans = transitions + offs[p1->position];
	    while (trans->state != NULL)
	      {
#if 0
		/* If we find a previous transition from `p1->position' to
		   `p2->position', it is overwritten.  This can happen only
		   if there are nested loops in the regexp, like in "((a)*)*".
		   In POSIX.2 repetition using the outer loop is always
		   preferred over using the inner loop.	 Therefore the
		   transition for the inner loop is useless and can be thrown
		   away. */
		/* XXX - The same position is used for all nodes in a bracket
		   expression, so this optimization cannot be used (it will
		   break bracket expressions) unless I figure out a way to
		   detect it here. */
		if (trans->state_id == p2->position)
		  {
		    DPRINT(("*"));
		    break;
		  }
#endif
		trans++;
	      }

	    if (trans->state == NULL)
	      (trans + 1)->state = NULL;
	    /* Use the character ranges, assertions, etc. from `p1' for
	       the transition from `p1' to `p2'. */
	    trans->code_min = (tre_cint_t) p1->code_min;
	    trans->code_max = (tre_cint_t) p1->code_max;
	    trans->state = transitions + offs[p2->position];
	    trans->state_id = p2->position;
	    trans->assertions = p1->assertions | p2->assertions
	      | (p1->class ? ASSERT_CHAR_CLASS : 0)
	      | (p1->neg_classes != NULL ? ASSERT_CHAR_CLASS_NEG : 0);
	    if (p1->backref >= 0)
	      {
		assert((trans->assertions & ASSERT_CHAR_CLASS) == 0);
		assert(p2->backref < 0);
		trans->u.backref = p1->backref;
		trans->assertions |= ASSERT_BACKREF;
	      }
	    else
	      trans->u.class = p1->class;
	    if (p1->neg_classes != NULL)
	      {
		for (i = 0; p1->neg_classes[i] != (tre_ctype_t)0; i++);
		trans->neg_classes =
		  xmalloc(sizeof(*trans->neg_classes) * (i + 1));
		if (trans->neg_classes == NULL)
		  return REG_ESPACE;
		for (i = 0; p1->neg_classes[i] != (tre_ctype_t)0; i++)
		  trans->neg_classes[i] = p1->neg_classes[i];
		trans->neg_classes[i] = (tre_ctype_t)0;
	      }
	    else
	      trans->neg_classes = NULL;

	    /* Find out how many tags this transition has. */
	    i = 0;
	    if (p1->tags != NULL)
	      while(p1->tags[i] >= 0)
		i++;
	    j = 0;
	    if (p2->tags != NULL)
	      while(p2->tags[j] >= 0)
		j++;

	    /* If we are overwriting a transition, free the old tag array. */
	    if (trans->tags != NULL)
	      xfree(trans->tags);
	    trans->tags = NULL;

	    /* If there were any tags, allocate an array and fill it. */
	    if (i + j > 0)
	      {
		trans->tags = xmalloc(sizeof(*trans->tags) * (i + j + 1));
		if (!trans->tags)
		  return REG_ESPACE;
		i = 0;
		if (p1->tags != NULL)
		  while(p1->tags[i] >= 0)
		    {
		      trans->tags[i] = p1->tags[i];
		      i++;
		    }
		l = i;
		j = 0;
		if (p2->tags != NULL)
		  while (p2->tags[j] >= 0)
		    {
		      /* Don't add duplicates. */
		      dup = 0;
		      for (k = 0; k < i; k++)
			if (trans->tags[k] == p2->tags[j])
			  {
			    dup = 1;
			    break;
			  }
		      if (!dup)
			trans->tags[l++] = p2->tags[j];
		      j++;
		    }
		trans->tags[l] = -1;
	      }

	    /* Set the parameter array.	 If both `p2' and `p1' have same
	       parameters, the values in `p2' override those in `p1'. */
	    if (p1->params || p2->params)
	      {
		if (!trans->params)
		  trans->params = xmalloc(sizeof(*trans->params)
					  * TRE_PARAM_LAST);
		if (!trans->params)
		  return REG_ESPACE;
		for (i = 0; i < TRE_PARAM_LAST; i++)
		  {
		    trans->params[i] = TRE_PARAM_UNSET;
		    if (p1->params && p1->params[i] != TRE_PARAM_UNSET)
		      trans->params[i] = p1->params[i];
		    if (p2->params && p2->params[i] != TRE_PARAM_UNSET)
		      trans->params[i] = p2->params[i];
		  }
	      }
	    else
	      {
		if (trans->params)
		  xfree(trans->params);
		trans->params = NULL;
	      }


#ifdef TRE_DEBUG
	    {
	      int *tags;

	      DPRINT(("	 %2d -> %2d on %3d", p1->position, p2->position,
		      p1->code_min));
	      if (p1->code_max != p1->code_min)
		DPRINT(("-%3d", p1->code_max));
	      tags = trans->tags;
	      if (tags)
		{
		  DPRINT((", tags ["));
		  while (*tags >= 0)
		    {
		      DPRINT(("%d", *tags));
		      tags++;
		      if (*tags >= 0)
			DPRINT((","));
		    }
		  DPRINT(("]"));
		}
	      if (trans->assertions)
		DPRINT((", assert %d", trans->assertions));
	      if (trans->assertions & ASSERT_BACKREF)
		DPRINT((", backref %d", trans->u.backref));
	      else if (trans->u.class)
		DPRINT((", class %ld", (long)trans->u.class));
	      if (trans->neg_classes)
		DPRINT((", neg_classes %p", trans->neg_classes));
	      if (trans->params)
		{
		  DPRINT((", "));
		  tre_print_params(trans->params);
		}
	      DPRINT(("\n"));
	    }
#endif /* TRE_DEBUG */
	    p2++;
	  }
	p1++;
      }
  else
    /* Compute a maximum limit for the number of transitions leaving
       from each state. */
    while (p1->position >= 0)
      {
	p2 = orig_p2;
	while (p2->position >= 0)
	  {
	    counts[p1->position]++;
	    p2++;
	  }
	p1++;
      }
  return REG_OK;
}

/* Converts the syntax tree to a TNFA.	All the transitions in the TNFA are
   labelled with one character range (there are no transitions on empty
   strings).  The TNFA takes O(n^2) space in the worst case, `n' is size of
   the regexp.

   This is the iterative version using an explicit stack; the previous
   recursive implementation could overflow the C stack for large patterns. */
static reg_errcode_t
tre_ast_to_tnfa(tre_stack_t *stack, tre_ast_node_t *node,
		tre_tnfa_transition_t *transitions,
		int *counts, int *offs)
{
  tre_union_t *uni;
  tre_catenation_t *cat;
  tre_iteration_t *iter;
  reg_errcode_t errcode = REG_OK;

  STACK_PUSHR(stack, voidptr, node);

  while (tre_stack_num_items(stack))
    {
      node = tre_stack_pop_voidptr(stack);

      switch (node->type)
	{
	case LITERAL:
	  break;

	case UNION:
	  uni = (tre_union_t *)node->obj;
	  /* Push right before left so that left is processed first,
	     preserving the traversal order of the recursive version. */
	  STACK_PUSHR(stack, voidptr, uni->right);
	  STACK_PUSHR(stack, voidptr, uni->left);
	  break;

	case CATENATION:
	  cat = (tre_catenation_t *)node->obj;
	  /* Add a transition from each position in cat->left->lastpos
	     to each position in cat->right->firstpos. */
	  errcode = tre_make_trans(cat->left->lastpos, cat->right->firstpos,
				   transitions, counts, offs);
	  if (errcode != REG_OK)
	    return errcode;
	  STACK_PUSHR(stack, voidptr, cat->right);
	  STACK_PUSHR(stack, voidptr, cat->left);
	  break;

	case ITERATION:
	  iter = (tre_iteration_t *)node->obj;
	  assert(iter->max == -1 || iter->max == 1);

	  if (iter->max == -1)
	    {
	      assert(iter->min == 0 || iter->min == 1);
	      /* Add a transition from each last position in the iterated
		 expression to each first position. */
	      errcode = tre_make_trans(iter->arg->lastpos, iter->arg->firstpos,
				       transitions, counts, offs);
	      if (errcode != REG_OK)
		return errcode;
	    }
	  STACK_PUSHR(stack, voidptr, iter->arg);
	  break;
	}
    }
  return REG_OK;
}

/* Returns 1 if each tag in the tag array `t1' is also in `t2'.  Either may
   be NULL. */
static int
tre_tags_subset(const int *t1, const int *t2)
{
  int i, j;

  for (i = 0; t1 != NULL && t1[i] >= 0; i++)
    {
      for (j = 0; t2 != NULL && t2[j] >= 0 && t2[j] != t1[i]; j++);
      if (t2 == NULL || t2[j] < 0)
	return 0;
    }
  return 1;
}

/* Returns 1 if the 0 terminated character class arrays `c1' and `c2'
   (either may be NULL) are the same. */
static int
tre_neg_classes_equal(const tre_ctype_t *c1, const tre_ctype_t *c2)
{
  if (c1 == NULL || c2 == NULL)
    return c1 == c2;
  for (; *c1 != (tre_ctype_t)0 && *c1 == *c2; c1++, c2++);
  return *c1 == *c2;
}

/* Returns 1 if taking the transitions `a' and `b' from one state has the
   same outcome: the same characters lead to the same state, setting the
   same tags and parameters. */
static int
tre_trans_same_outcome(const tre_tnfa_transition_t *a,
		       const tre_tnfa_transition_t *b)
{
  const int kind = ASSERT_CHAR_CLASS | ASSERT_CHAR_CLASS_NEG | ASSERT_BACKREF;

  if (a->state_id != b->state_id
      || a->code_min != b->code_min || a->code_max != b->code_max
      || (a->assertions & kind) != (b->assertions & kind))
    return 0;
  if (a->assertions & ASSERT_BACKREF
      ? a->u.backref != b->u.backref : a->u.class != b->u.class)
    return 0;
  return tre_neg_classes_equal(a->neg_classes, b->neg_classes)
    && tre_tags_subset(a->tags, b->tags) && tre_tags_subset(b->tags, a->tags)
    && tre_params_equal(a->params, b->params);
}

/* Removes the transitions that another transition from the same state
   makes unnecessary: of two transitions with the same outcome, only the
   one with the weaker assertions is needed.  Different parts of the regexp
   can add such transitions between the same two positions.  The remaining
   transitions keep their order. */
static reg_errcode_t
tre_merge_transitions(tre_tnfa_transition_t *transitions,
		      unsigned int num_transitions, int *offs, int numpos)
{
  /* For each target state, the last kept transition into it from the
     state in `source', and for each kept transition the one before it. */
  int *last, *source, *prev;
  int s, t, k, kept;

  last = xmalloc(sizeof(*last) * (2 * numpos + num_transitions + 1));
  if (last == NULL)
    return REG_ESPACE;
  source = last + numpos;
  prev = source + numpos;
  for (s = 0; s < numpos; s++)
    source[s] = -1;

  for (s = 0; s < numpos; s++)
    {
      kept = offs[s];
      for (t = offs[s]; transitions[t].state != NULL; t++)
	{
	  tre_tnfa_transition_t *trans = &transitions[t];
	  int target = trans->state_id;
	  int head = source[target] == s ? last[target] : -1;

	  for (k = head; k >= 0; k = prev[k])
	    if (tre_trans_same_outcome(&transitions[k], trans)
		&& ((transitions[k].assertions & ~trans->assertions) == 0
		    || (trans->assertions & ~transitions[k].assertions) == 0))
	      break;
	  if (k >= 0)
	    {
	      /* Keep the earlier transition with the weaker assertions. */
	      transitions[k].assertions &= trans->assertions;
	      if (trans->tags != NULL)
		xfree(trans->tags);
	      if (trans->params != NULL)
		xfree(trans->params);
	      if (trans->neg_classes != NULL)
		xfree(trans->neg_classes);
	      continue;
	    }
	  if (kept != t)
	    transitions[kept] = *trans;
	  prev[kept] = head;
	  last[target] = kept;
	  source[target] = s;
	  kept++;
	}
      /* Clear the slots that were moved or freed. */
      if (kept < t)
	memset(&transitions[kept], 0, sizeof(*transitions) * (t - kept));
    }

  xfree(last);
  return REG_OK;
}

#define ERROR_EXIT(err)		  \
  do				  \
    {				  \
      errcode = err;		  \
      if (/*CONSTCOND*/(void)1,1)	  \
      	goto error_exit;	  \
    }				  \
 while (/*CONSTCOND*/(void)0,0)


int
tre_compile(regex_t *preg, const tre_char_t *regex, size_t n, int cflags)
{
  tre_stack_t *stack;
  tre_ast_node_t *tree, *tmp_ast_l, *tmp_ast_r;
  tre_pos_and_tags_t *p;
  int *counts = NULL, *offs = NULL;
  int i, add = 0;
  tre_tnfa_transition_t *transitions, *initial;
  tre_tnfa_t *tnfa = NULL;
  tre_submatch_data_t *submatch_data;
  tre_tag_direction_t *tag_directions = NULL;
  reg_errcode_t errcode;
  tre_mem_t mem;
  int numpos = 0;

  /* Parse context. */
  tre_parse_ctx_t parse_ctx;

  /* Allocate a stack used throughout the compilation process for various
     purposes. */
  stack = tre_stack_new(512, TRE_MAX_STACK);
  if (!stack)
    return REG_ESPACE;
  /* Allocate a fast memory allocator. */
  mem = tre_mem_new();
  if (!mem)
    {
      tre_stack_destroy(stack);
      return REG_ESPACE;
    }

  /* Parse the regexp. */
  memset(&parse_ctx, 0, sizeof(parse_ctx));
  parse_ctx.mem = mem;
  parse_ctx.stack = stack;
  parse_ctx.re = regex;
  parse_ctx.len = n;
  parse_ctx.cflags = cflags;
  parse_ctx.max_backref = -1;
  /* Use 8-bit optimizations in 8-bit mode */
  parse_ctx.mb_cur_max = (cflags & REG_USEBYTES) ? 1 : TRE_MB_CUR_MAX;
  DPRINT(("tre_compile: parsing '%.*" STRF "'\n", (int)n, regex));
  errcode = tre_parse(&parse_ctx);
  if (errcode != REG_OK)
    ERROR_EXIT(errcode);
  preg->re_nsub = parse_ctx.submatch_id - 1;
  tree = parse_ctx.result;

  /* Back references and approximate matching cannot currently be used
     in the same regexp. */
  if (parse_ctx.max_backref >= 0 && parse_ctx.have_approx)
    ERROR_EXIT(REG_BADPAT);

#ifdef TRE_DEBUG
  tre_ast_print(tree);
#endif /* TRE_DEBUG */

  /* Referring to nonexistent subexpressions is illegal. */
  if (parse_ctx.max_backref > (int)preg->re_nsub)
    ERROR_EXIT(REG_ESUBREG);

  /* Allocate the TNFA struct. */
  tnfa = xcalloc(1, sizeof(tre_tnfa_t));
  if (tnfa == NULL)
    ERROR_EXIT(REG_ESPACE);
  tnfa->have_backrefs = parse_ctx.max_backref >= 0;
  tnfa->have_approx = parse_ctx.have_approx;
  tnfa->num_submatches = parse_ctx.submatch_id;

  /* Set up tags for submatch addressing.  If REG_NOSUB is set and the
     regexp does not have back references, this can be skipped. */
  if (tnfa->have_backrefs || !(cflags & REG_NOSUB))
    {
      DPRINT(("tre_compile: setting up tags\n"));

      /* Figure out how many tags we will need. */
      errcode = tre_add_tags(NULL, stack, tree, tnfa);
      if (errcode != REG_OK)
	ERROR_EXIT(errcode);
#ifdef TRE_DEBUG
      tre_ast_print(tree);
#endif /* TRE_DEBUG */

      if (tnfa->num_tags > 0)
	{
	  tag_directions = xmalloc(sizeof(*tag_directions)
				   * (tnfa->num_tags + 1));
	  if (tag_directions == NULL)
	    ERROR_EXIT(REG_ESPACE);
	  tnfa->tag_directions = tag_directions;
	  memset(tag_directions, -1,
		 sizeof(*tag_directions) * (tnfa->num_tags + 1));
	}
      tnfa->minimal_tags = xcalloc(tnfa->num_tags * 2 + 1,
				   sizeof(*tnfa->minimal_tags));
      if (tnfa->minimal_tags == NULL)
	ERROR_EXIT(REG_ESPACE);

      submatch_data = xcalloc((unsigned)parse_ctx.submatch_id,
			      sizeof(*submatch_data));
      if (submatch_data == NULL)
	ERROR_EXIT(REG_ESPACE);
      tnfa->submatch_data = submatch_data;

      errcode = tre_add_tags(mem, stack, tree, tnfa);
      if (errcode != REG_OK)
	ERROR_EXIT(errcode);

#ifdef TRE_DEBUG
      for (i = 0; i < parse_ctx.submatch_id; i++)
	DPRINT(("pmatch[%d] = {t%d, t%d}\n",
		i, submatch_data[i].so_tag, submatch_data[i].eo_tag));
      for (i = 0; i < tnfa->num_tags; i++)
	DPRINT(("t%d is %s\n", i,
		tag_directions[i] == TRE_TAG_MINIMIZE ?
		"minimized" : "maximized"));
#endif /* TRE_DEBUG */
    }

  /* Expand iteration nodes. */
  errcode = tre_expand_ast(mem, stack, tree, tag_directions,
			   &tnfa->params_depth);
  if (errcode != REG_OK)
    ERROR_EXIT(errcode);

  /* Add a dummy node for the final state.
     XXX - For certain patterns this dummy node can be optimized away,
	   for example "a*" or "ab*".	Figure out a simple way to detect
	   this possibility. */
  tmp_ast_l = tree;
  tmp_ast_r = tre_ast_new_literal(mem, 0, 0);
  if (tmp_ast_r == NULL)
    ERROR_EXIT(REG_ESPACE);

  tree = tre_ast_new_catenation(mem, tmp_ast_l, tmp_ast_r);
  if (tree == NULL)
    ERROR_EXIT(REG_ESPACE);

  errcode = tre_compute_npfl(mem, stack, tree, &numpos);
  if (errcode != REG_OK)
    ERROR_EXIT(errcode);

#ifdef TRE_DEBUG
  tre_ast_print(tree);
  DPRINT(("Number of states: %d\n", numpos));
#endif /* TRE_DEBUG */

  counts = xmalloc(sizeof(int) * numpos);
  if (counts == NULL)
    ERROR_EXIT(REG_ESPACE);

  offs = xmalloc(sizeof(int) * numpos);
  if (offs == NULL)
    ERROR_EXIT(REG_ESPACE);

  for (i = 0; i < numpos; i++)
    counts[i] = 0;
  errcode = tre_ast_to_tnfa(stack, tree, NULL, counts, NULL);
  if (errcode != REG_OK)
    ERROR_EXIT(errcode);

  add = 0;
  for (i = 0; i < numpos; i++)
    {
      offs[i] = add;
      /* Note that counts[i] cannot exceed TRE_MAX_POS which is orders of
	 magnitude smaller than TRE_MAX_TRANS */
      if (add >= TRE_MAX_TRANS - counts[i] - 1)
	  ERROR_EXIT(REG_ESPACE);
      add += counts[i] + 1;
      counts[i] = 0;
    }
  transitions = xcalloc((unsigned)add + 1, sizeof(*transitions));
  if (transitions == NULL)
    ERROR_EXIT(REG_ESPACE);
  tnfa->transitions = transitions;
  tnfa->num_transitions = add;

  DPRINT(("Converting to TNFA:\n"));
  errcode = tre_ast_to_tnfa(stack, tree, transitions, counts, offs);
  if (errcode != REG_OK)
    ERROR_EXIT(errcode);

  errcode = tre_merge_transitions(transitions, tnfa->num_transitions, offs,
				  numpos);
  if (errcode != REG_OK)
    ERROR_EXIT(errcode);

  /* If in eight bit mode, compute a table of characters that can be the
     first character of a match. */
  tnfa->first_char = -1;
  if (parse_ctx.mb_cur_max == 1 && !tmp_ast_l->nullable)
    {
      int count = 0;
      tre_cint_t k;
      DPRINT(("Characters that can start a match:"));
      tnfa->firstpos_chars = xcalloc(256, sizeof(char));
      if (tnfa->firstpos_chars == NULL)
	ERROR_EXIT(REG_ESPACE);
      for (p = tree->firstpos; p->position >= 0; p++)
	{
	  tre_tnfa_transition_t *j = transitions + offs[p->position];
	  while (j->state != NULL)
	    {
	      for (k = j->code_min; k <= j->code_max && k < 256; k++)
		{
		  DPRINT((" %d", k));
		  tnfa->firstpos_chars[k] = 1;
		  count++;
		}
	      j++;
	    }
	}
      DPRINT(("\n"));
#define TRE_OPTIMIZE_FIRST_CHAR 1
#if TRE_OPTIMIZE_FIRST_CHAR
      if (count == 1)
	{
	  for (k = 0; k < 256; k++)
	    if (tnfa->firstpos_chars[k])
	      {
		DPRINT(("first char must be %d\n", k));
		tnfa->first_char = k;
		xfree(tnfa->firstpos_chars);
		tnfa->firstpos_chars = NULL;
		break;
	      }
	}
#endif

    }
  else
    tnfa->firstpos_chars = NULL;


  p = tree->firstpos;
  i = 0;
  while (p->position >= 0)
    {
      i++;

#ifdef TRE_DEBUG
      {
	int *tags;
	DPRINT(("initial: %d", p->position));
	tags = p->tags;
	if (tags != NULL)
	  {
	    if (*tags >= 0)
	      DPRINT(("/"));
	    while (*tags >= 0)
	      {
		DPRINT(("%d", *tags));
		tags++;
		if (*tags >= 0)
		  DPRINT((","));
	      }
	  }
	DPRINT((", assert %d", p->assertions));
	if (p->params)
	  {
	    DPRINT((", "));
	    tre_print_params(p->params);
	  }
	DPRINT(("\n"));
      }
#endif /* TRE_DEBUG */

      p++;
    }

  initial = xcalloc((unsigned)i + 1, sizeof(tre_tnfa_transition_t));
  if (initial == NULL)
    ERROR_EXIT(REG_ESPACE);
  tnfa->initial = initial;

  i = 0;
  for (p = tree->firstpos; p->position >= 0; p++)
    {
      initial[i].state = transitions + offs[p->position];
      initial[i].state_id = p->position;
      initial[i].tags = NULL;
      /* Copy the arrays p->tags, and p->params, they are allocated
	 from a tre_mem object. */
      if (p->tags)
	{
	  int j;
	  for (j = 0; p->tags[j] >= 0; j++);
	  initial[i].tags = xmalloc(sizeof(*p->tags) * (j + 1));
	  if (!initial[i].tags)
	    ERROR_EXIT(REG_ESPACE);
	  memcpy(initial[i].tags, p->tags, sizeof(*p->tags) * (j + 1));
	}
      initial[i].params = NULL;
      if (p->params)
	{
	  initial[i].params = xmalloc(sizeof(*p->params) * TRE_PARAM_LAST);
	  if (!initial[i].params)
	    ERROR_EXIT(REG_ESPACE);
	  memcpy(initial[i].params, p->params,
		 sizeof(*p->params) * TRE_PARAM_LAST);
	}
      initial[i].assertions = p->assertions;
      i++;
    }
  initial[i].state = NULL;

  tnfa->num_transitions = add;
  tnfa->final = transitions + offs[tree->lastpos[0].position];
  tnfa->num_states = numpos;
  tnfa->cflags = cflags;

  DPRINT(("final state %p\n", (void *)tnfa->final));

  tre_mem_destroy(mem);
  tre_stack_destroy(stack);
  xfree(counts);
  xfree(offs);

  preg->TRE_REGEX_T_FIELD = (void *)tnfa;
  return REG_OK;

 error_exit:
  /* Free everything that was allocated and return the error code. */
  tre_mem_destroy(mem);
  if (stack != NULL)
    tre_stack_destroy(stack);
  if (counts != NULL)
    xfree(counts);
  if (offs != NULL)
    xfree(offs);
  preg->TRE_REGEX_T_FIELD = (void *)tnfa;
  tre_free(preg);
  return errcode;
}




void
tre_free(regex_t *preg)
{
  tre_tnfa_t *tnfa;
  unsigned int i;
  tre_tnfa_transition_t *trans;

  tnfa = (void *)preg->TRE_REGEX_T_FIELD;
  if (!tnfa)
    return;

  for (i = 0; i < tnfa->num_transitions; i++)
    if (tnfa->transitions[i].state)
      {
	if (tnfa->transitions[i].tags)
	  xfree(tnfa->transitions[i].tags);
	if (tnfa->transitions[i].neg_classes)
	  xfree(tnfa->transitions[i].neg_classes);
	if (tnfa->transitions[i].params)
	  xfree(tnfa->transitions[i].params);
      }
  if (tnfa->transitions)
    xfree(tnfa->transitions);

  if (tnfa->initial)
    {
      for (trans = tnfa->initial; trans->state; trans++)
	{
	  if (trans->tags)
	    xfree(trans->tags);
	  if (trans->params)
	    xfree(trans->params);
	}
      xfree(tnfa->initial);
    }

  if (tnfa->submatch_data)
    {
      for (i = 0; i < tnfa->num_submatches; i++)
	if (tnfa->submatch_data[i].parents)
	  xfree(tnfa->submatch_data[i].parents);
      xfree(tnfa->submatch_data);
    }

  if (tnfa->tag_directions)
    xfree(tnfa->tag_directions);
  if (tnfa->firstpos_chars)
    xfree(tnfa->firstpos_chars);
  if (tnfa->minimal_tags)
    xfree(tnfa->minimal_tags);
  xfree(tnfa);
}

char *
tre_version(void)
{
  static char str[256];
  char *version;

  if (str[0] == 0)
    {
      (void) tre_config(TRE_CONFIG_VERSION, &version);
      /* R change: mark this as R's modified copy of TRE. */
      (void) snprintf(str, sizeof(str), "TRE %s R_fixes (BSD)", version);
    }
  return str;
}

int
tre_config(int query, void *result)
{
  int *int_result = result;
  const char **string_result = result;

  switch (query)
    {
    case TRE_CONFIG_APPROX:
#ifdef TRE_APPROX
      *int_result = 1;
#else /* !TRE_APPROX */
      *int_result = 0;
#endif /* !TRE_APPROX */
      return REG_OK;

    case TRE_CONFIG_WCHAR:
#ifdef TRE_WCHAR
      *int_result = 1;
#else /* !TRE_WCHAR */
      *int_result = 0;
#endif /* !TRE_WCHAR */
      return REG_OK;

    case TRE_CONFIG_MULTIBYTE:
#ifdef TRE_MULTIBYTE
      *int_result = 1;
#else /* !TRE_MULTIBYTE */
      *int_result = 0;
#endif /* !TRE_MULTIBYTE */
      return REG_OK;

    case TRE_CONFIG_SYSTEM_ABI:
#ifdef TRE_CONFIG_SYSTEM_ABI
      *int_result = 1;
#else /* !TRE_CONFIG_SYSTEM_ABI */
      *int_result = 0;
#endif /* !TRE_CONFIG_SYSTEM_ABI */
      return REG_OK;

    case TRE_CONFIG_VERSION:
      *string_result = TRE_VERSION;
      return REG_OK;
    }

  return REG_NOMATCH;
}


/* EOF */
