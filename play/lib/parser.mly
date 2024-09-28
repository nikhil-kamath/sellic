%{
    open Term
    open Matrix
    open Core.Option


    let shape_error (pos, _) =
        let open Lexing in
        raise (ShapeError (Core.sprintf "Misshapen matrix on Line:%d Position:%d. Hint: Try using *[ as a prefix." pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)))


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
%token LBRACKET
%token RBRACKET
%token SEMI
%token STARLBRACKET

%left PLUS MINUS
%left TIMES

%type <term> term
%type <op1> unop
%type <op2> binop
%type <program> program
%type <nested> matrix
%type <nested> irregular_matrix

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
| IF; p=term; THEN; t=term; ELSE; f=term {If(p, t, f)}
| m=matrix { match Matrix.shape m with
    | None -> shape_error $loc
    | Some s -> Matrix {shape = s; elements = m}
}
| m=irregular_matrix { Matrix { shape = []; elements = m }}

matrix :
| LBRACKET; l=matrix_tail { Nested l }

matrix_tail :
| RBRACKET { [] }
| i=INT; RBRACKET { [Item i] }
| i=INT; SEMI; tail=matrix_tail { (Item i) :: tail }
| h=matrix; RBRACKET { [h] }
| h=matrix; SEMI; tail=matrix_tail { h :: tail }

irregular_matrix :
| STARLBRACKET; l=matrix_tail { Nested l }




%inline unop:
| MINUS { Negate }
| EXCLAMATION { Not }

%inline binop:
| PLUS { Add }
| TIMES { Mult }
| AND { And }
| OR { Or }



