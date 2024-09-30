%{
    open Term
    open Matrix
    open Core
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
%token COLON
%token M
%token LANGLE
%token RANGLE
%token SPARSE
%token UNKNOWN
%token DENSE
%token COMMA
%token TBOOL
%token TSCALAR
%token LE
%token GE
%token SQUOTE
%token FSLASH
%token BSLASH

%nonassoc FUN
%nonassoc IF
%nonassoc ELSE
%nonassoc LET
%nonassoc IN
%nonassoc ARROW
%left AND OR
%right EXCLAMATION
%left LANGLE LE RANGLE GE
%left PLUS MINUS
%left TIMES
%left EQUAL
%nonassoc TRUE FALSE INT ID LBRACKET LPAREN STARLBRACKET

%type <term> term
%type <term> eterm
%type <term list> list(eterm)
%type <op1> unop
%type <op2> binop
%type <program> program
%type <nested> matrix
%type <nested> irregular_matrix
%type <nested list> matrix_tail
%type <typ> typ
%type <sparsity> sparsity

%start program


%%

program:
| terms=list(eterm); EOF {Program (terms)}

eterm:
| t=term; DOUBLESEMI { t }

term:
| LPAREN; t=term; RPAREN {t}
| i=INT {Scalar(i)}
| TRUE {Bool(true)}
| FALSE {Bool(false)}
| var=ID {Var(var)}
| LET; var=ID; EQUAL; bound_term=term; IN; t=term {Let(var, bound_term, t)}
| FUN; defs=nonempty_list(var=ID; COLON; ty=typ { var, ty }); ARROW; body=term { List.fold_right defs ~f:(fun (x, t) a -> Abs (x, t, a)) ~init:body }
| t1=term; op=binop; t2=term {BOp(op, t1, t2)}
| op=unop; t=term {UOp (op, t)}
| IF; p=term; THEN; t=term; ELSE; f=term {If(p, t, f)}
| m=matrix { match Matrix.shape m with
    | None -> shape_error $loc
    | Some s -> Matrix {shape = s; elements = m}
}
| m=irregular_matrix { Matrix { shape = []; elements = m }}
| t1=term; SQUOTE; t2=term {Map(t1, t2)}
| t1=term; FSLASH; t2=term; BSLASH; t3=term {Fold(t1, t2, t3)}
| t1=term; t2=term {App(t1, t2)}

matrix :
| LBRACKET; l=matrix_tail; { Nested l }

irregular_matrix :
| STARLBRACKET; l=matrix_tail { Nested l }

matrix_tail :
| RBRACKET { [] }
| i=INT; RBRACKET { [Item i] }
| i=INT; SEMI; tail=matrix_tail { (Item i) :: tail }
| h=matrix; RBRACKET { [h] }
| h=matrix; SEMI; tail=matrix_tail { h :: tail }

typ :
| TSCALAR { TScalar }
| TBOOL { TBool }
| M; LANGLE; d=separated_list(TIMES, INT); s=option(COMMA; s=sparsity {s}); RANGLE { TMatrix {shape=d; sparsity=Option.value s ~default:Unknown} }

sparsity:
| SPARSE { Sparse }
| UNKNOWN { Unknown }
| DENSE { Dense }

%inline unop:
| MINUS { Negate }
| EXCLAMATION { Not }

%inline binop:
| PLUS { Add }
| TIMES { Mult }
| MINUS { Sub }
| EQUAL { Eq }
| GE { Gte }
| LE { Lte }
| LANGLE { Lt }
| RANGLE { Gt }
| AND { And }
| OR { Or }



