/* This is an implementation of the Unlambda programming language. */
/* This one is in C (the Master One is in Scheme). */

/* Copyright (C) 1999 by David A. Madore <david.madore@ens.fr> */

/* This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty
 * of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
 * the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA */

/* This is a more or less mechanical translation of the primary C
 * version of the interpreter (so as to replace the use of the Boehm
 * GC by reference counting), and _that_ is itself a translation of
 * the Java version of the interpreter.  Please refer to these other
 * interpreters for explanations about how things work. */

#include <stdio.h>
#include <stdlib.h>

struct generic_s {
  unsigned long refcnt;
};

struct function_s {
  unsigned long refcnt;
  enum {
    FUNCTION_I,
    FUNCTION_DOT,
    FUNCTION_K1,
    FUNCTION_K,
    FUNCTION_S2,
    FUNCTION_S1,
    FUNCTION_S,
    FUNCTION_V,
    FUNCTION_D1,
    FUNCTION_D,
    FUNCTION_CONT,
    FUNCTION_C,
    FUNCTION_E,
    FUNCTION_AT,
    FUNCTION_QUES,
    FUNCTION_PIPE
  } t;
  union {
    char function_dot_v;
    struct function_s *function_k1_v;
    struct {
      struct function_s *x, *y;
    } function_s2_v;
    struct function_s *function_s1_v;
    struct expression_s *function_d1_v;
    struct continuation_s *function_cont_v;
    char function_ques_v;
  } d;
};

struct expression_s {
  unsigned long refcnt;
  enum {
    EXPRESSION_FUNCTION,
    EXPRESSION_APPLICATION
  } t;
  union {
    struct function_s *expression_function_v;
    struct {
      struct expression_s *rator, *rand;
    } expression_application_v;
  } d;
};

struct continuation_s {
  unsigned long refcnt;
  enum {
    CONTINUATION_APP1,
    CONTINUATION_APP,
    CONTINUATION_DEL,
    CONTINUATION_FINAL
  } t;
  union {
    struct {
      struct expression_s *rand;
      struct continuation_s *cont;
    } continuation_app1_v;
    struct {
      struct function_s *erator;
      struct continuation_s *cont;
    } continuation_app_v;
    struct {
      struct function_s *erand;
      struct continuation_s *cont;
    } continuation_del_v;
  } d;
};

struct task_s {
  unsigned long refcnt;
  enum {
    TASK_EVAL,
    TASK_APP1,
    TASK_APP,
    TASK_FINAL
  } t;
  union {
    struct {
      struct expression_s *expr;
      struct continuation_s *cont;
    } task_eval_v;
    struct {
      struct function_s *erator;
      struct expression_s *rand;
      struct continuation_s *cont;
    } task_app1_v;
    struct {
      struct function_s *erator, *erand;
      struct continuation_s *cont;
    } task_app_v;
  } d;
};

void
chkmem (void *ptr)
{
  if ( ! ptr )
    {
      fprintf (stderr, "Out of memory!\n");
      exit (1);
    }
}

struct function_s *
new_function (void)
{
  struct function_s *fun;

  fun = malloc (sizeof(struct function_s));
  chkmem (fun);
  fun->refcnt = 0;
  return fun;
}

struct expression_s *
new_expression (void)
{
  struct expression_s *expr;

  expr = malloc (sizeof(struct expression_s));
  chkmem (expr);
  expr->refcnt = 0;
  return expr;
}

struct continuation_s *
new_continuation (void)
{
  struct continuation_s *cont;

  cont = malloc (sizeof(struct continuation_s));
  chkmem (cont);
  cont->refcnt = 0;
  return cont;
}

struct task_s *
new_task (void)
{
  struct task_s *task;

  task = malloc (sizeof(struct task_s));
  chkmem (task);
  task->refcnt = 0;
  return task;
}

void free_function (struct function_s *fun);
void free_expression (struct expression_s *expr);
void free_continuation (struct continuation_s *cont);
void free_task (struct task_s *task);

void
release_function (struct function_s *fun)
{
  fun->refcnt --;
  free_function (fun);
}

void
release_expression (struct expression_s *expr)
{
  expr->refcnt --;
  free_expression (expr);
}

void
release_continuation (struct continuation_s *cont)
{
  cont->refcnt --;
  free_continuation (cont);
}

void
release_task (struct task_s *task)
{
  task->refcnt --;
  free_task (task);
}

void
free_function (struct function_s *fun)
{
  if ( fun->refcnt )
    return;
  switch ( fun->t )
    {
    case FUNCTION_K1:
      release_function (fun->d.function_k1_v);
      break;
    case FUNCTION_S2:
      release_function (fun->d.function_s2_v.x);
      release_function (fun->d.function_s2_v.y);
      break;
    case FUNCTION_S1:
      release_function (fun->d.function_s1_v);
      break;
    case FUNCTION_D1:
      release_expression (fun->d.function_d1_v);
      break;
    case FUNCTION_CONT:
      release_continuation (fun->d.function_cont_v);
      break;
    default:
      ;
    }
  free (fun);
}

void
free_expression (struct expression_s *expr)
{
  if ( expr->refcnt )
    return;
  switch ( expr->t )
    {
    case EXPRESSION_FUNCTION:
      release_function (expr->d.expression_function_v);
      break;
    case EXPRESSION_APPLICATION:
      release_expression (expr->d.expression_application_v.rator);
      release_expression (expr->d.expression_application_v.rand);
      break;
    }
  free (expr);
}

void
free_continuation (struct continuation_s *cont)
{
  if ( cont->refcnt )
    return;
  switch ( cont->t )
    {
    case CONTINUATION_APP1:
      release_expression (cont->d.continuation_app1_v.rand);
      release_continuation (cont->d.continuation_app1_v.cont);
      break;
    case CONTINUATION_APP:
      release_function (cont->d.continuation_app_v.erator);
      release_continuation (cont->d.continuation_app_v.cont);
      break;
    case CONTINUATION_DEL:
      release_function (cont->d.continuation_del_v.erand);
      release_continuation (cont->d.continuation_del_v.cont);
      break;
    default:
      ;
    }
  free (cont);
}

void
free_task (struct task_s *task)
{
  if ( task->refcnt )
    return;
  switch ( task->t )
    {
    case TASK_EVAL:
      release_expression (task->d.task_eval_v.expr);
      release_continuation (task->d.task_eval_v.cont);
      break;
    case TASK_APP1:
      release_function (task->d.task_app1_v.erator);
      release_expression (task->d.task_app1_v.rand);
      release_continuation (task->d.task_app1_v.cont);
      break;
    case TASK_APP:
      release_function (task->d.task_app_v.erator);
      release_function (task->d.task_app_v.erand);
      release_continuation (task->d.task_app_v.cont);
      break;
    default:
      ;
    }
  free (task);
}

void
hold_ptr (struct generic_s *ptr)
{
  ptr->refcnt++;
}

void
init_ptr_ (struct generic_s **pptr, struct generic_s *ptr)
{
  hold_ptr (ptr);
  *pptr = ptr;
}

#define init_ptr(pptr,ptr) \
	init_ptr_((struct generic_s **)pptr,(struct generic_s *)ptr)

char current_ch = EOF;

struct task_s *
invoke (struct continuation_s *cont, struct function_s *val)
{
  switch ( cont->t )
    {
    case CONTINUATION_APP1:
      {
	struct task_s *task = new_task ();

	task->t = TASK_APP1;
	init_ptr (&task->d.task_app1_v.erator, val);
	init_ptr (&task->d.task_app1_v.rand,
		  cont->d.continuation_app1_v.rand);
	init_ptr (&task->d.task_app1_v.cont,
		  cont->d.continuation_app1_v.cont);
	return task;
      }
    case CONTINUATION_APP:
      {
	struct task_s *task = new_task ();

	task->t = TASK_APP;
	init_ptr (&task->d.task_app_v.erator,
		  cont->d.continuation_app_v.erator);
	init_ptr (&task->d.task_app_v.erand, val);
	init_ptr (&task->d.task_app_v.cont,
		  cont->d.continuation_app_v.cont);
	return task;
      }
    case CONTINUATION_DEL:
      {
	struct task_s *task = new_task ();

	task->t = TASK_APP;
	init_ptr (&task->d.task_app_v.erator, val);
	init_ptr (&task->d.task_app_v.erand,
		  cont->d.continuation_del_v.erand);
	init_ptr (&task->d.task_app_v.cont,
		  cont->d.continuation_del_v.cont);
	return task;
      }
    case CONTINUATION_FINAL:
      {
	struct task_s *task = new_task ();

	task->t = TASK_FINAL;
	return task;
      }
    }
  fprintf (stderr, "INTERNAL ERROR: invoke() surprised!\n");
  return NULL;
}

struct task_s *
apply (struct function_s *rator, struct function_s *rand,
       struct continuation_s *cont)
{
  switch ( rator->t )
    {
    case FUNCTION_I:
      return invoke (cont, rand);
    case FUNCTION_DOT:
      putchar (rator->d.function_dot_v);
      return invoke (cont, rand);
    case FUNCTION_K1:
      return invoke (cont, rator->d.function_k1_v);
    case FUNCTION_K:
      {
	struct function_s *val = new_function ();
	struct task_s *task;

	val->t = FUNCTION_K1;
	init_ptr (&val->d.function_k1_v, rand);
	task = invoke (cont, val);
	free_function (val);
	return task;
      }
    case FUNCTION_S2:
      {
	struct expression_s *e_x = new_expression ();
	struct expression_s *e_y = new_expression ();
	struct expression_s *e_z = new_expression ();
	struct expression_s *e1 = new_expression ();
	struct expression_s *e2 = new_expression ();
	struct expression_s *e = new_expression ();
	struct task_s *task = new_task ();

	e_x->t = EXPRESSION_FUNCTION;
	init_ptr (&e_x->d.expression_function_v, rator->d.function_s2_v.x);
	e_y->t = EXPRESSION_FUNCTION;
	init_ptr (&e_y->d.expression_function_v, rator->d.function_s2_v.y);
	e_z->t = EXPRESSION_FUNCTION;
	init_ptr (&e_z->d.expression_function_v, rand);
	e1->t = EXPRESSION_APPLICATION;
	init_ptr (&e1->d.expression_application_v.rator, e_x);
	init_ptr (&e1->d.expression_application_v.rand, e_z);
	e2->t = EXPRESSION_APPLICATION;
	init_ptr (&e2->d.expression_application_v.rator, e_y);
	init_ptr (&e2->d.expression_application_v.rand, e_z);
	e->t = EXPRESSION_APPLICATION;
	init_ptr (&e->d.expression_application_v.rator, e1);
	init_ptr (&e->d.expression_application_v.rand, e2);
	task->t = TASK_EVAL;
	init_ptr (&task->d.task_eval_v.expr, e);
	init_ptr (&task->d.task_eval_v.cont, cont);
#if 0  /* Harmless but not necessary */
	free_expression (e_x);
	free_expression (e_y);
	free_expression (e_z);
	free_expression (e1);
	free_expression (e2);
	free_expression (e);
#endif
	return task;
      }
    case FUNCTION_S1:
      {
	struct function_s *val = new_function ();
	struct task_s *task;

	val->t = FUNCTION_S2;
	init_ptr (&val->d.function_s2_v.x, rator->d.function_s1_v);
	init_ptr (&val->d.function_s2_v.y, rand);
	task = invoke (cont, val);
	free_function (val);
	return task;
      }
    case FUNCTION_S:
      {
	struct function_s *val = new_function ();
	struct task_s *task;

	val->t = FUNCTION_S1;
	init_ptr (&val->d.function_s1_v, rand);
	task = invoke (cont, val);
	free_function (val);
	return task;
      }
    case FUNCTION_V:
      return invoke (cont, rator);
    case FUNCTION_D1:
      {
	struct continuation_s *ncont = new_continuation ();
	struct task_s *task = new_task ();

	ncont->t = CONTINUATION_DEL;
	init_ptr (&ncont->d.continuation_del_v.erand, rand);
	init_ptr (&ncont->d.continuation_del_v.cont, cont);
	task->t = TASK_EVAL;
	init_ptr (&task->d.task_eval_v.expr, rator->d.function_d1_v);
	init_ptr (&task->d.task_eval_v.cont, ncont);
#if 0  /* Harmless but not necessary */
	free_continuation (ncont);
#endif
	return task;
      }
    case FUNCTION_D:
      {
	struct expression_s *promise = new_expression ();
	struct function_s *val = new_function ();
	struct task_s *task;

	promise->t = EXPRESSION_FUNCTION;
	init_ptr (&promise->d.expression_function_v, rand);
	val->t = FUNCTION_D1;
	init_ptr (&val->d.function_d1_v, promise);
	task = invoke (cont, val);
	free_continuation (cont);
	free_function (val);
	return task;
      }
    case FUNCTION_CONT:
      return invoke (rator->d.function_cont_v, rand);
    case FUNCTION_C:
      {
	struct function_s *val = new_function ();
	struct task_s *task = new_task ();

	val->t = FUNCTION_CONT;
	init_ptr (&val->d.function_cont_v, cont);
	task->t = TASK_APP;
	init_ptr (&task->d.task_app_v.erator, rand);
	init_ptr (&task->d.task_app_v.erand, val);
	init_ptr (&task->d.task_app_v.cont, cont);
#if 0  /* Harmless but not necessary */
	free_function (val);
#endif
	return task;
      }
    case FUNCTION_E:
      {
	struct task_s *task = new_task ();

	task->t = TASK_FINAL;
	return task;
      }
    case FUNCTION_AT:
      {
	struct function_s *val = new_function ();
	struct task_s *task = new_task ();

	current_ch = getchar ();
	val->t = (current_ch != EOF ? FUNCTION_I : FUNCTION_V);
	task->t = TASK_APP;
	init_ptr (&task->d.task_app_v.erator, rand);
	init_ptr (&task->d.task_app_v.erand, val);
	init_ptr (&task->d.task_app_v.cont, cont);
#if 0  /* Harmless but not necessary */
	free_function (val);
#endif
	return task;
      }
    case FUNCTION_QUES:
      {
	struct function_s *val = new_function ();
	struct task_s *task = new_task ();

	val->t = (current_ch == rator->d.function_ques_v
		  ? FUNCTION_I : FUNCTION_V);
	task->t = TASK_APP;
	init_ptr (&task->d.task_app_v.erator, rand);
	init_ptr (&task->d.task_app_v.erand, val);
	init_ptr (&task->d.task_app_v.cont, cont);
#if 0  /* Harmless but not necessary */
	free_function (val);
#endif
	return task;
      }
    case FUNCTION_PIPE:
      {
	struct function_s *val = new_function ();
	struct task_s *task = new_task ();

	if ( current_ch != EOF )
	  {
	    val->t = FUNCTION_DOT;
	    val->d.function_dot_v = current_ch;
	  }
	else
	  val->t = FUNCTION_V;
	task->t = TASK_APP;
	init_ptr (&task->d.task_app_v.erator, rand);
	init_ptr (&task->d.task_app_v.erand, val);
	init_ptr (&task->d.task_app_v.cont, cont);
#if 0  /* Harmless but not necessary */
	free_function (val);
#endif
	return task;
      }
    }
  fprintf (stderr, "INTERNAL ERROR: apply() surprised!\n");
  return NULL;
}

struct task_s *
eval (struct expression_s *expr, struct continuation_s *cont)
{
  switch ( expr->t )
    {
    case EXPRESSION_FUNCTION:
      return invoke (cont, expr->d.expression_function_v);
    case EXPRESSION_APPLICATION:
      {
	struct continuation_s *ncont = new_continuation ();
	struct task_s *task = new_task ();

	ncont->t = CONTINUATION_APP1;
	init_ptr (&ncont->d.continuation_app1_v.rand,
		  expr->d.expression_application_v.rand);
	init_ptr (&ncont->d.continuation_app1_v.cont, cont);
	task->t = TASK_EVAL;
	init_ptr (&task->d.task_eval_v.expr,
		  expr->d.expression_application_v.rator);
	init_ptr (&task->d.task_eval_v.cont, ncont);
#if 0  /* Harmless but not necessary */
	free_continuation (ncont);
#endif
	return task;
      }
    }
  fprintf (stderr, "INTERNAL ERROR: eval() surprised!\n");
  return NULL;
}

struct task_s *
run (struct task_s *task)
{
  switch ( task->t )
    {
    case TASK_EVAL:
      return eval (task->d.task_eval_v.expr, task->d.task_eval_v.cont);
    case TASK_APP1:
      {
	if ( task->d.task_app1_v.erator->t == FUNCTION_D )
	  {
	    struct function_s *val = new_function ();
	    struct task_s *ntask;

	    val->t = FUNCTION_D1;
	    init_ptr (&val->d.function_d1_v, task->d.task_app1_v.rand);
	    ntask = invoke (task->d.task_app1_v.cont, val);
	    free_function (val);
	    return ntask;
	  }
	else
	  {
	    struct continuation_s *ncont = new_continuation ();
	    struct task_s *ntask;

	    ncont->t = CONTINUATION_APP;
	    init_ptr (&ncont->d.continuation_app_v.erator,
		      task->d.task_app1_v.erator);
	    init_ptr (&ncont->d.continuation_app_v.cont,
		      task->d.task_app1_v.cont);
	    ntask = eval (task->d.task_app1_v.rand, ncont);
	    free_continuation (ncont);
	    return ntask;
	  }
      }
    case TASK_APP:
      return apply (task->d.task_app_v.erator, task->d.task_app_v.erand,
		    task->d.task_app_v.cont);
    case TASK_FINAL:
      /* Should not happen */;
    }
  fprintf (stderr, "INTERNAL ERROR: run() surprised!\n");
  return NULL;
}

struct expression_s *
parse (FILE *input)
{
  int ch;
  do {
    ch = getc (input);
    if ( ch == '#' )
      while ( ch != '\n' && ch != EOF )
	ch = getc (input);
  } while ( ch == ' ' || ch == '\n' || ch == '\r' || ch == '\t' );
  if ( ch == '`' )
    {
      struct expression_s *rator = parse (input);
      struct expression_s *rand = parse (input);
      struct expression_s *expr = new_expression ();

      expr->t = EXPRESSION_APPLICATION;
      init_ptr (&expr->d.expression_application_v.rator, rator);
      init_ptr (&expr->d.expression_application_v.rand, rand);
#if 0  /* Harmless but not necessary */
      free_expression (rator);
      free_expression (rand);
#endif
      return expr;
    }
  else if ( ch == 'i' || ch == 'I' )
    {
      struct function_s *fun = new_function ();
      struct expression_s *expr = new_expression ();

      fun->t = FUNCTION_I;
      expr->t = EXPRESSION_FUNCTION;
      init_ptr (&expr->d.expression_function_v, fun);
#if 0  /* Harmless but not necessary */
      free_function (fun);
#endif
      return expr;
    }
  else if ( ch == 'k' || ch == 'K' )
    {
      struct function_s *fun = new_function ();
      struct expression_s *expr = new_expression ();

      fun->t = FUNCTION_K;
      expr->t = EXPRESSION_FUNCTION;
      init_ptr (&expr->d.expression_function_v, fun);
#if 0  /* Harmless but not necessary */
      free_function (fun);
#endif
      return expr;
    }
  else if ( ch == 's' || ch == 'S' )
    {
      struct function_s *fun = new_function ();
      struct expression_s *expr = new_expression ();

      fun->t = FUNCTION_S;
      expr->t = EXPRESSION_FUNCTION;
      init_ptr (&expr->d.expression_function_v, fun);
#if 0  /* Harmless but not necessary */
      free_function (fun);
#endif
      return expr;
    }
  else if ( ch == 'v' || ch == 'V' )
    {
      struct function_s *fun = new_function ();
      struct expression_s *expr = new_expression ();

      fun->t = FUNCTION_V;
      expr->t = EXPRESSION_FUNCTION;
      init_ptr (&expr->d.expression_function_v, fun);
#if 0  /* Harmless but not necessary */
      free_function (fun);
#endif
      return expr;
    }
  else if ( ch == 'd' || ch == 'D' )
    {
      struct function_s *fun = new_function ();
      struct expression_s *expr = new_expression ();

      fun->t = FUNCTION_D;
      expr->t = EXPRESSION_FUNCTION;
      init_ptr (&expr->d.expression_function_v, fun);
#if 0  /* Harmless but not necessary */
      free_function (fun);
#endif
      return expr;
    }
  else if ( ch == 'c' || ch == 'C' )
    {
      struct function_s *fun = new_function ();
      struct expression_s *expr = new_expression ();

      fun->t = FUNCTION_C;
      expr->t = EXPRESSION_FUNCTION;
      init_ptr (&expr->d.expression_function_v, fun);
#if 0  /* Harmless but not necessary */
      free_function (fun);
#endif
      return expr;
    }
  else if ( ch == 'e' || ch == 'E' )
    {
      struct function_s *fun = new_function ();
      struct expression_s *expr = new_expression ();

      fun->t = FUNCTION_C;
      expr->t = EXPRESSION_FUNCTION;
      init_ptr (&expr->d.expression_function_v, fun);
#if 0  /* Harmless but not necessary */
      free_function (fun);
#endif
      return expr;
    }
  else if ( ch == 'r' )
    {
      struct function_s *fun = new_function ();
      struct expression_s *expr = new_expression ();

      fun->t = FUNCTION_DOT;
      fun->d.function_dot_v = '\n';
      expr->t = EXPRESSION_FUNCTION;
      init_ptr (&expr->d.expression_function_v, fun);
#if 0  /* Harmless but not necessary */
      free_function (fun);
#endif
      return expr;
    }
  else if ( ch == '.' )
    {
      struct function_s *fun = new_function ();
      struct expression_s *expr = new_expression ();
      int ch2;

      fun->t = FUNCTION_DOT;
      ch2 = getc (input);
      if ( ch2 == EOF )
	goto ueof;
      fun->d.function_dot_v = ch2;
      expr->t = EXPRESSION_FUNCTION;
      init_ptr (&expr->d.expression_function_v, fun);
#if 0  /* Harmless but not necessary */
      free_function (fun);
#endif
      return expr;
    }
  else if ( ch == '@' )
    {
      struct function_s *fun = new_function ();
      struct expression_s *expr = new_expression ();

      fun->t = FUNCTION_AT;
      expr->t = EXPRESSION_FUNCTION;
      init_ptr (&expr->d.expression_function_v, fun);
#if 0  /* Harmless but not necessary */
      free_function (fun);
#endif
      return expr;
    }
  else if ( ch == '?' )
    {
      struct function_s *fun = new_function ();
      struct expression_s *expr = new_expression ();
      int ch2;

      fun->t = FUNCTION_QUES;
      ch2 = getc (input);
      if ( ch2 == EOF )
	goto ueof;
      fun->d.function_ques_v = ch2;
      expr->t = EXPRESSION_FUNCTION;
      init_ptr (&expr->d.expression_function_v, fun);
#if 0  /* Harmless but not necessary */
      free_function (fun);
#endif
      return expr;
    }
  else if ( ch == '|' )
    {
      struct function_s *fun = new_function ();
      struct expression_s *expr = new_expression ();

      fun->t = FUNCTION_PIPE;
      expr->t = EXPRESSION_FUNCTION;
      init_ptr (&expr->d.expression_function_v, fun);
#if 0  /* Harmless but not necessary */
      free_function (fun);
#endif
      return expr;
    }
  else if ( ch == EOF )
    {
    ueof:
      fprintf (stderr, "Unexpected end of file\n");
      exit (1);
    }
  else
    {
      fprintf (stderr, "Character not recognized: %c\n", ch);
      exit (1);
    }
  return NULL;
}

int
main (int argc, char *argv[])
{
  struct expression_s *expr;
  struct continuation_s *finis = new_continuation ();
  struct task_s *task = new_task ();

  if ( argc == 1 )
    expr = parse (stdin);
  else if ( argc == 2 )
    {
      FILE *f;
      f = fopen (argv[1], "r");
      if ( ! f )
	{
	  perror ("Can't open input file");
	  exit (1);
	}
      expr = parse (f);
      fclose (f);
    }
  else
    {
      fprintf (stderr, "Expected zero or one argument");
      exit (1);
    }
  finis->t = CONTINUATION_FINAL;
  task->t = TASK_EVAL;
  init_ptr (&task->d.task_eval_v.expr, expr);
  init_ptr (&task->d.task_eval_v.cont, finis);
  while ( task->t != TASK_FINAL )
    {
      struct task_s *next_task = run (task);

      free_task (task);
      task = next_task;
    }
  return 0;
}
