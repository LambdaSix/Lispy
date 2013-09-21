#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

// LISP Objects
enum type {CONS, ATOM, FUNC, LAMBDA};

typedef struct {
	enum type type;
} object;

/* ATOM is a letter, digit, etc */
typedef struct {
	enum type type;
	char *name;
} atom_object;

/* CONS is essentially a list */
typedef struct {
	enum type type;
	object *car;
	object *cdr;
} cons_object;

/* FUNC is a reference to a C function */
typedef struct {
	enum type type;
	object* (*fn)(object*, object*);
} func_object;

/* LAMBDA holds lambda expressions */
typedef struct {
	enum type type;
	object* args;
	object* sexp;
} lambda_object;

// Helpful mnemonics

// Contents of the Address part of Register
// That is, the first half of the sexp
#define first(X)			(((cons_object *) (X))->car)
// Contents of the Decrement part of Register.
// Or, the second half of the sexp
#define second(X)			(((cons_object *) (X))->cdr)

char *name(object *o) {
	if (o->type != ATOM) exit(1);
	return ((atom_object*)o)->name;
}

// Create a new atom
object *atom(char *n) {
	atom_object *ptr = (atom_object *) malloc(sizeof(atom_object));
	ptr->type = ATOM;
	char *name;
	name = malloc(strlen(n) +1);
	strcpy(name, n);
	ptr->name = name;
	return (object *)ptr;
}

// Create a new cons list.
object *cons(object *first, object *second) {
	cons_object *ptr = (cons_object *) malloc(sizeof(cons_object));
	ptr->type = CONS;
	ptr->car = first;
	ptr->cdr = second;
	return (object *)ptr;
}

// Create a new function binding, parameter is function pointer.
object *func(object* (*fn)(object*, object*)) {
	func_object *ptr = (func_object *) malloc(sizeof(func_object));
	ptr->type = FUNC;
	ptr->fn = fn;
	return (object *)ptr;
}

void append(object *list, object *obj) {
	object *ptr;
	for (ptr = list; second(ptr) != NULL; ptr = second(ptr));

	second(ptr) = cons(obj, NULL);
}

object *lambda(object *args, object *sexp) {
	lambda_object *ptr = (lambda_object *) malloc (sizeof(lambda_object));
	ptr->type = LAMBDA;
	ptr->args = args;
	ptr->sexp = sexp;
	return (object *)ptr;
}

object *tee,*nil;

// Bound functions

object *eval(object *sexp, object *env);

object *fn_first(object *args, object *env) {
	return first(first(args));
}

object *fn_second(object *args, object *env) {
	return second(first(args));
}

object *fn_quote(object *args, object *env) {
	return first(args);
}

object *fn_cons(object *args, object *env) {
	object *list = cons(first(args), NULL);
	args = first(second(args));

	while (args != NULL && args->type == CONS) {
		append(list, first(args));
		args = second(args);
	}

	return list;
}

object *fn_equal(object *args, object *env) {
	object *first = first(args);
	object *second = first(second(args));

	if (strcmp(name(first), name(second)) == 0)
		return tee;
	else 
		return nil;
}

object *fn_atom(object *args, object *env) {
	if (first(args)->type == ATOM)
		return tee;
	else
		return nil;
}

object *fn_cond(object *args, object *env) {
	while (args != NULL && args->type == CONS) {
		object *list = first(args);
		object *pred = nil;

		if (first(list) != NULL) 
			pred = eval(first(list), env);

		object *ret = first(second(list));

		if (pred != nil)
			return eval(ret, env);

		args = second(args);
	}
	return nil;
}

// Utility

object *interleave(object *c1, object *c2) {
	object *list = cons(cons(first(c1), cons(first(c2), NULL)), NULL);
	c1 = second(c1);
	c2 = second(c2);

	while (c1 != NULL && c1->type == CONS) {
		append(list, cons(first(c1), cons(first(c2), NULL)));
		c1 = second(c1);
		c2 = second(c2);
	}

	return list;
}

object *replace_atom(object *sexp, object *with) {
	if (sexp->type == CONS) {
		object *list = cons(replace_atom(first(sexp), with), NULL);
		sexp = second(sexp);

		// Recurse through the list.
		while (sexp != NULL && sexp->type == CONS) {
			append(list, replace_atom(first(sexp), with));
			sexp = second(sexp);
		}

		return list;
	} else {
		object* tmp = with;

		while (tmp != NULL && tmp->type == CONS) {
			object *item = first(tmp);
			object *atom = first(item);
			object *replacement = first(second(item));

			if (strcmp(name(atom), name(sexp)) == 0)
				return replacement;

			tmp = second(tmp);
		}

		return sexp;
	}
}

object *fn_lambda(object *args, object *env) {
	// Lambda objects hold two lists, the parameters and the function.
	object *lambda = first(args);
	args = second(args);

	// Extract the list of arguments
	object *list = interleave((((lambda_object *) (lambda))->args), args);
	// Extract the function S-Expression
	object* sexp = replace_atom((((lambda_object *) (lambda))->sexp), list);

	return eval(sexp, env);
}

object *fn_label(object *args, object *env) {
	append(env, cons(atom(name(first(args))), cons(first(second(args)), NULL)));
	return tee;
}

object* lookup(char* n, object *env) {
	object *tmp = env;

	while (tmp != NULL && tmp->type == CONS) {
		object *item = first(tmp);
		object *fnname = first(item);
		object *value = first(second(item));

		if (strcmp(name(fnname), n) == 0)
			return value;
		tmp = second(tmp);
	}

	return NULL;
}

object* next_token(FILE *in) {
	int ch = getc(in);

	while (isspace(ch)) ch = getc(in);

	if (ch == '\n') ch = getc(in);
	if (ch == EOF) exit(0);

	if (ch == ')') return atom(")");
	if (ch == '(') return atom("(");

	char buffer[128];
	int index =0;

	while (!isspace(ch) && ch != ')') {
		buffer[index++] = ch;
		ch = getc(in);
	}

	buffer[index++] = '\0';
	if (ch == ')')
		ungetc(ch, in);

	return atom(buffer);
}

// Setup an environment and register methods
object* init_env() {
	object *env = cons(cons(atom("QUOTE"), cons(func(&fn_quote), NULL)), NULL);
	append(env, cons(atom("CAR"), cons(func(&fn_first), NULL)));
	append(env, cons(atom("CDR"), cons(func(&fn_second), NULL)));
	append(env, cons(atom("CONS"), cons(func(&fn_cons), NULL)));
	append(env, cons(atom("EQUAL"), cons(func(&fn_equal), NULL)));
	append(env, cons(atom("ATOM"), cons(func(&fn_atom), NULL)));
	append(env, cons(atom("COND"), cons(func(&fn_cond), NULL)));
	append(env, cons(atom("LAMBDA"), cons(func(&fn_lambda), NULL)));
	append(env, cons(atom("LABEL"), cons(func(&fn_label), NULL)));

	// LISP literal 't' (true)
	tee = atom("#T");
	// LISP literal 'nil' (empty list)
	nil = cons(NULL, NULL);

	return env;
}

// Parse the rest of the list.
// read_tail loops until it hits the right hand side of the sexp
object *read_tail(FILE *in) {
	object *token = next_token(in);

	if (strcmp(name(token),")") == 0) {
		return NULL;
	} else if (strcmp(name(token),"(") == 0) {
		object *first = read_tail(in);
		object *second = read_tail(in);
		return cons(first, second);
	} else {
		object *first = token;
		object *second = read_tail(in);
		return cons(first, second);
	}
}

// Get the next token from the file, if a left paren, go parse the list
object *read(FILE *in) {
	object *token = next_token(in);

	if (strcmp(name(token), "(") == 0)
		return read_tail(in);

	return token;
}

object *eval_fn(object *sexp, object *env) {
	object *symbol = first(sexp);
	object *args = second(sexp);

	if (symbol->type == LAMBDA) {
		// Return a new lambda based on the symbol and args.
		return fn_lambda(sexp, env);
	}
	else if (symbol->type == FUNC) {
		// return the appropriate C-binding funcobject
		return (((func_object *) (symbol))->fn)(args, env);
	}
	else
		return sexp;
}

object *eval(object *sexp, object *env) {
	if (sexp == NULL) {
		// Return the empty list for NULL.
		return nil;
	}

	// List
	if (sexp->type == CONS) {
		// ATOM and LAMBDA don't need evaluated, just parameter extraction.
		if (first(sexp)->type == ATOM && strcmp(name(first(sexp)), "LAMBDA") == 0) {
			object* largs =first(second(sexp));
			object* lsexp = first(second(second(sexp)));

			return lambda(largs, lsexp);
		} else {
			// Otherwise just evaluate it.
			object *accum = cons(eval(first(sexp), env), NULL);
			sexp = second(sexp);

			// Evaluate every cell in the inner-list
			while (sexp != NULL && sexp->type == CONS) {
				append(accum, eval(first(sexp), env));
				sexp = second(sexp);
			}

			return eval_fn(accum, env);
		}
	} else {
		// Transform the value into an environment value
		object *val = lookup(name(sexp), env);

		if (val == NULL)
			return sexp;
		else 
			return val;
	}
}

// IO functions

// Pretty-printing.
void print(object *sexp) {
	if (sexp == NULL)
		return;

	if (sexp->type == CONS) {
		printf( "(" );
		// Recurse into the cell.
		print(first(sexp));

		// Get the second half of the S-Expression
		sexp = second(sexp);

		while (sexp != NULL && sexp->type == CONS) {
			// cdr is another list.
			printf(" ");
			print(first(sexp));
			sexp = second(sexp);
		}
		printf( ")" );
	} 
	else if (sexp->type == ATOM) {
		// ATOM's just get their name printed.
		printf("%s", name(sexp));
	}
	else if (sexp->type == LAMBDA) {
		// Pretty-print the lambda name + args
		printf("#");
		// Cast to a lambda_object then recurse into the args+sexp
		print((((lambda_object *) (sexp))->args));
		print((((lambda_object *) (sexp))->sexp));
	} else {
		// Not a list, lambda or atom, oh well.
		printf( "error" );
	}
}

// Cheap REPL
int main(int argc, char *argv[]) {
	object *env = init_env();

	FILE* in;

	if (argc > 1)
		in = fopen(argv[1], "r");
	else
		in = stdin;

	do {
		printf("@> ");
		print(eval(read(in), env));
		printf("\n");
	} while (1);

	return 0;
}