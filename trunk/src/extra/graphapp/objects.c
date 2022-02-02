/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: objects.c -- maintain internal info about graphical objects.
 * Platform: Windows  Version: 2.35  Date: 1998/04/04
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 2.00  Changes: Total overhaul - new object class system.
 * Version: 2.15  Changes: Objects are Transparent by default.
 * Version: 2.20  Changes: Faster and better exiting.
 * Version: 2.30  Changes: Now uses array(), not malloc().
 * Version: 2.35  Changes: New delayed deletion technique.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.

   Copyright (C) 2004 The R Foundation
*/

#include "internal.h"

/*
 *  The current implementation of the list of objects
 *  uses a linked list of primary objects, each object
 *  potentially having a list of children associated with it
 *
 *  e.g. window
 *        +-> menubar -> button -> scrollbar -> textfield
 *             +-> menu
 *                  +-> menuitem -> menuitem -> menuitem ...
 *
 *  The base_object is at the top of the hierarchy.
 */

	static object base_object = NULL;

/*
 *  Initialise the base_object and set the list to be empty.
 */
PROTECTED
void init_objects(void)
{
    if (base_object)
	return;

    base_object = create(objinfo);

    if (! base_object)
	apperror("Out of memory (init_objects)!");

    base_object->kind = BaseObject;
    base_object->next = base_object->prev = base_object;
    base_object->parent = base_object->child = NULL;
}

static void add_object(object obj, object parent)
{
    /* All objects have a parent. */
    if (! parent)
	parent = base_object;

    if (parent->child)
    {	/* add to end of child circularly linked list */
	obj->prev = parent->child->prev;
	obj->next = parent->child;
	obj->prev->next = obj;
	obj->next->prev = obj;
    } else {
	/* create new child list */
	obj->next = obj->prev = obj;
	parent->child = obj;
    }
    obj->parent = parent;

#if DEBUG
    print_object_list();
#endif
}

static void remove_object(object obj)
{
    if ((obj->next) && (obj->next != obj)) {
	obj->prev->next = obj->next;
	obj->next->prev = obj->prev;
    } else {
	obj->next = obj->prev = NULL;
    }

    if (obj->parent)
	if (obj->parent->child == obj)
	    obj->parent->child = obj->next;
}

/*
 *  Bring an object to the front of its sibling list.
 *  Useful for keeping track of which window is up front.
 */
PROTECTED
void move_to_front(object obj)
{
    object parent = obj->parent;
    remove_object(obj);	 /* Remove it so as to reorder list. */
    add_object(obj, parent); /* Add to end of list. */
    parent->child = obj;	 /* Move to front. */
}

/*
 *  Call a function on all objects in a sibling list.
 *  Only applies up to the end of the list, i.e. up to and
 *  including the last element in the circular list.
 *  Used to disable or enable windows when modal windows are used.
 */
void apply_to_list(object first, actionfn fn)
{
    object start = first->parent->child;
    object obj = first;

    do {
	fn(obj);
	obj = obj->next;
    } while (obj != start);
}

/*
 *  When objects are deleted using del() they are added to
 *  the end of a circular list, which is traversed at the end
 *  of each call to doevent(). This traversal actually deletes
 *  the objects in that list. The del_base object is at
 *  the head of this list and is the first object to be
 *  deleted.
 */

typedef struct delnode delnode;
struct delnode {
    object    obj;	/* object to be deleted */
    delnode * next;	/* next object in the list */
    delnode * prev; /* previous object in the list */
};

static delnode * del_base = NULL;

/*
 *  Reduce the reference count of an object:
 *  A by-product of this might be that the object is added
 *  to the list of objects to be deleted. If the refcount
 *  of an object is already at zero, the object is not
 *  added to the list, nor is the refcount reduced any further.
 *  If the refcount of the object is less than zero, the
 *  object is a special object which cannot be deleted under
 *  any circumstances (e.g. default fonts and cursors).
 */
PROTECTED
void decrease_refcount(object obj)
{
    delnode *new_node;

    if (! obj)
	return;
    if (obj->refcount <= 0)
	return;	/* cannot delete this object */
    obj->refcount--;
    if (obj->refcount != 0)
	return;	/* don't delete this object */

    /* the refcount must now be zero, so add to list */
    new_node = create(delnode);
    new_node->obj = obj;
    if (del_base) {
	/* add to end of circular list */
	new_node->prev = del_base->prev;
	new_node->next = del_base;
	new_node->prev->next = new_node;
	new_node->next->prev = new_node;
    }
    else {
	/* create new circular list */
	new_node->next = new_node->prev = new_node;
	del_base = new_node;
    }
}

/*
 *  Increase the reference count of an object.
 *  Avoids increases a negative number (they're special).
 */
PROTECTED
void increase_refcount(object obj)
{
    if (obj && (obj->refcount >= 0))
	obj->refcount ++;
}

/*
 *  Protect an object from being deleted. This function is
 *  used to protect default fonts and cursors from deletion.
 *  It achieves this by simply setting the refcount to the
 *  special value -1.
 */
PROTECTED
void protect_object(object obj)
{
    if (obj)
	obj->refcount = -1;
}

/*
 *  Remove a delnode from the deletion list.
 *  Fix del_base to point correctly.
 */
static void remove_delnode(delnode *n)
{
    /* remove and destroy this node */
    n->prev->next = n->next;
    n->next->prev = n->prev;
    if (n == n->next) /* nothing in list */
	del_base = NULL;
    else if (n == del_base)	/* move head */
	del_base = n->next;
    discard(n);
}

/*
 *  Remove all copies of an object from the deletion list
 *  after that object has been deleted.
 *  This occurs as a by-product of object deletion, and
 *  ensures that an object cannot be deleted twice.
 */
static void remove_deleted_object(object obj)
{
    delnode *last;
    delnode *next;
    delnode *n;

    if (! del_base)
	return;
    next = del_base;
    last = del_base->prev;
    do {
	n = next;
	next = n->next;
	if (n->obj == obj) {
	    /* remove and destroy this node */
	    remove_delnode(n);
	}
    } while (n != last);
}

/*
 *  Traverse the deletion list and delete every object found there.
 *  This function repeatedly deletes del_base until that value is null.
 *  This traversal occurs at the end of every doevent call.
 */
static void del_object(object obj);	/* declaration */
PROTECTED
void deletion_traversal(void)
{
    static int level = 0;
    object obj;

    level++;
    if (level == 1) {
	while (del_base) {
	    obj = del_base->obj;
	    if (obj) {
		if (obj->refcount == 0)
		    del_object(obj);
		else
		    remove_deleted_object(obj);
	    }
	}
    }
    level--;
}

/*
 *  Keep application global variables up-to-date.
 */
static void update_app_globals(object obj)
{
    if (obj->drawstate == current)
	current = & app_drawstate;
    if (obj == current->dest)
	current->dest = NULL;
    if (obj == current_window)
	current_window = NULL;
    if (obj == current_menubar)
	current_menubar = NULL;
    if (obj == current_menu)
	current_menu = NULL;
    if (obj == current->crsr)
	current->crsr = ArrowCursor;
    if (obj == current->fnt)
	current->fnt = SystemFont;
}

/*
 *  Object deletion functions:
 *
 *    The free_private function destroys the operating-system
 *    graphics handles, also calling the user-defined and
 *    internally defined deletion functions. This function
 *    also removes the object from the internal deletion list.
 *
 *    The free_object function does this too, additionally
 *    releasing the memory used by the object from the heap.
 */
static void free_private(object obj)
{
#if DEBUG
    print_object_list();
#endif
    /* Remove from object hierachy. */
    remove_object(obj);
    /* Call user destructor first. */
    if ((obj->call) && (obj->call->die))
	obj->call->die(obj);
    /* Fix application variables. */
    update_app_globals(obj);
    /* Free drawing context. */
    del_context(obj);
    /* Then call private destructor. */
    if (obj->die)
	obj->die(obj);
    /* Remove object from deletion list. */
    remove_deleted_object(obj);
}

static void free_object(object obj)
{
    free_private(obj);

    /* Free any extra internal info. */
    if (obj->drawstate)
	discard(obj->drawstate);
    if (obj->text)
	discard(obj->text);
    if (obj->call)
	discard(obj->call);
    discard(obj);
}

/*
 *  Private recursive object destructor. Deletes child objects
 *  first and then the top-most object specified.
 *  This function is called during the deletion_traversal function,
 *  which is called at the end of each doevent call.
 */
static void del_object(object obj)
{
    while (obj->child)
	del_object(obj->child);
    free_object(obj);
}

/*
 *  Quick destruction function when terminating application.
 *  Make Windows happy by releasing memory which it otherwise
 *  would not automatically release (silly Windows).
 *  Leave application variables on the heap (by not calling
 *  del_object or free_object). This works fine, and avoids
 *  problems when calling exitapp() in callbacks.
 */
static void free_memory(object obj)
{
    if (obj->kind == WindowObject)
	hide(obj);
    while(obj->child)
	free_memory(obj->child);
    free_private(obj);
}

PROTECTED
void finish_objects(void)
{
    free_memory(base_object);
    base_object = NULL;
}

/*
 *  Create and return a new object with a refcount of 1.
 *  The object is Transparent and may have some other
 *  structures associated with it (depending on its kind).
 */
PROTECTED
object new_object(int kind, HANDLE handle, object parent)
{
    object obj;

    /* create the object */
    obj = create(objinfo);
    if (! obj)
	return NULL;
    obj->menubar = obj->popup = obj->toolbar = NULL;
    obj->status[0] = '\0';
    if (kind & ControlObject)
    {
	obj->call = create(callinfo);
	if (! obj->call) {
	    discard(obj);
	    return NULL;
	}
    }

    obj->refcount = 1;
    obj->kind = kind;
    obj->handle = handle;
    obj->bg = Transparent;

    add_object(obj, parent);

    return obj;
}

/*
 *  Return the object if it matches the spec.
 */
static object match_object(object obj, HANDLE handle, int id, int key)
{
    if ((handle != 0) && (obj->handle == handle))
	return obj;
    if ((id != 0) && (obj->id == id))
	return obj;
    if ((key != 0) && (obj->key == key)
	&& (obj->kind == MenuitemObject))
	return obj;
    return NULL;
}

/*
 *  Perform the multi-child tree traversal.
 */
object tree_search(object top, HANDLE handle, int id, int key)
{
    object obj, first_object, found = NULL;

    if ((! top) || (! top->child))
	return NULL;

    first_object = top->child;
    obj = first_object;

    while (obj != top)
    {
	found = match_object(obj, handle, id, key);
	if (found)
	    break;

	if (obj->child)
	{	/* object has children - descend tree */
	    first_object = obj->child;
	    obj = first_object;
	    continue;
	}
	else
	{	/* object is childless - go to next object */
	    obj = obj->next;
	}

	while (obj == first_object)
	{	/* back at first object in sibling list */
	    /* climb the tree */
	    obj = obj->parent;
	    if (obj == top)
		break;
	    if (obj->parent) {
		first_object = obj->parent->child;
		obj = obj->next;
	    }
	    else
		break;
	}
    }

    return found;
}

PROTECTED
object find_object(HANDLE handle, int id, int key)
{
    return tree_search(base_object, handle, id, key);
}

/*
 *  Write the tree structure to disk to examine it.
 */
#if DEBUG
static void print_drawstate(FILE *f, drawstate d, object obj)
{
    fprintf(f, "%lu: [", ((unsigned long)d));
    if (!d)
	return;
    fprintf(f, "drawto=%s", (! d->drawing) ? "null" :
	    (d->drawing == obj) ? "this" : "??");
    fprintf(f, ", 0x%8.8lX", (unsigned long)d->rgb);
    fprintf(f, ", %s", getname(d->font));
    fprintf(f, ", %s", getname(d->cursor));
    fprintf(f, ", (%d,%d)", d->point.x, d->point.y);
    fprintf(f, ", width=%d", d->linewidth);
    fprintf(f, "]");
}

static void print_objects(FILE *f, object obj, int indent)
{
    object child;
    char *s;
    int i;

    for (i=0; i<indent; i++)
	fprintf(f, " ");
    if (! obj) {
	fprintf(f, "Null Object\n");
	return;
    }
    switch(obj->kind) {
    case BaseObject:	s = "BaseObject"; break;
    case ControlObject:	s = "ControlObject"; break;
    case WindowObject:	s = "WindowObject"; break;
    case BitmapObject:	s = "BitmapObject"; break;
    case CursorObject:	s = "CursorObject"; break;
    case FontObject:	s = "FontObject"; break;
    case UserObject:	s = "UserObject"; break;
    case LabelObject:	s = "LabelObject"; break;
    case ButtonObject:	s = "ButtonObject"; break;
    case CheckboxObject:	s = "CheckboxObject"; break;
    case RadioObject:	s = "RadioObject"; break;
    case ScrollbarObject:	s = "ScrollbarObject"; break;
    case FieldObject:	s = "FieldObject"; break;
    case TextboxObject:	s = "TextboxObject"; break;
    case ListboxObject:	s = "ListboxObject"; break;
    case ProgressbarObject:	s = "ProgressbarObject"; break;
    case MultilistObject:	s = "MultilistObject"; break;
    case DroplistObject:	s = "DroplistObject"; break;
    case DropfieldObject:	s = "DropfieldObject"; break;
    case MenubarObject:	s = "MenubarObject"; break;
    case MenuObject:	s = "MenuObject"; break;
    case MenuitemObject:	s = "MenuitemObject"; break;
    default: s = "Unknown Object???"; break;
    }
    fprintf(f, "%s", s);
    if (obj->text)
	fprintf(f, ", text: \"%s\"", obj->text);
    /*
      if (! obj->child)
      fprintf(f, ", child: null");
    */
    if (! obj->handle)
	fprintf(f, ", handle: null");
    if (obj->drawstate) {
	fprintf(f, ", drawstate ");
	print_drawstate(f, obj->drawstate, obj);
    }
    fprintf(f, "\n");

    indent += 2;
    child = obj->child;
    while (child) {
	print_objects(f, child, indent); /* recursive */
	child = child->next;
	if (child == obj->child) /* if back at start of list */
	    break;
    }
    fflush(f);
}

PROTECTED
void print_object_list(void)
{
    static int start_again = 1;
    char *mode = "at"; /* to ensure text mode */
    FILE *f;

    if (! app_initialised)
	return;
    if (start_again) {
	mode = "w";
	start_again = 0;
    }
    f = fopen("obj-list.txt", mode);

    if (current) {
	fprintf(f, "current drawstate ");
	print_drawstate(f, current, NULL);
	fprintf(f, "\n");
    }
    fprintf(f, "global drawstate ");
    print_drawstate(f, & app_drawstate, NULL);
    fprintf(f, "\n");

    print_objects(f, base_object, 0);
    fprintf(f, "------------\n");
    fclose(f);
}
#endif

void remove_menu_item(object obj)
{
    /* Must call private destructor first! */
    if (obj->die) obj->die(obj);
    remove_object(obj);
    remove_deleted_object(obj);
}
