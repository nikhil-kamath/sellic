%{
    open Term
%}

%token <int> INT
%token <string> ID
%token LPAREN
%token RPAREN
%token LET
%token IN
%token FUN
%token ARROW
%token EQUAL
%token PLUS
%token MINUS
%token TIMES
%token EOF
%token TRUE
%token FALSE
%token EXCLAMATION
%token AND
%token OR
%token DOUBLESEMI
%token IF
%token THEN
%token ELSE

%left PLUS MINUS
%left TIMES

%type <term> term
%type <op1> unop
%type <op2> binop
%type <program> program

%start program


%%

program:
| terms=list(eterm); EOF {Program (terms)}

eterm:
| t=term; DOUBLESEMI { t }

term:
| LPAREN; t=term; RPAREN {t}
| t1=term; t2=term {App(t1, t2)}
| i=INT {Scalar(i)}
| TRUE {Bool(true)}
| FALSE {Bool(false)}
| var=ID {Var(var)}
| LET; var=ID; EQUAL; bound_term=term; IN; t=term {Let(var, bound_term, t)}
| FUN; var=ID; ARROW; t=term {Abs(var, TUnknown, t)}
| op=unop; t=term {UOp (op, t)}
| t1=term; op=binop; t2=term {BOp(op, t1, t2)}

%inline unop:
| MINUS { Negate }
| EXCLAMATION { Not }

%inline binop:
| PLUS { Add }
| TIMES { Mult }
| AND { And }
| OR { Or }



